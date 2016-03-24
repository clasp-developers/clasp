#ifdef DECLARE_FORWARDS
 namespace asttooling {
    class DerivableSyntaxOnlyAction;
    class DerivableASTFrontendAction;
    class DerivableMatchCallback;
    class DerivableFrontendActionFactory;
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
KIND_LISPALLOC_core__SingleDispatchMethod_O = 60,
KIND_LISPALLOC_core__RandomState_O = 61,
KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O = 62,
KIND_LISPALLOC_core__SequenceStepper_O = 63,
KIND_LISPALLOC_core__ConsStepper_O = 64,
KIND_LISPALLOC_core__VectorStepper_O = 65,
KIND_LISPALLOC_llvmo__DebugLoc_O = 66,
KIND_LISPALLOC_llvmo__Attribute_O = 67,
KIND_LISPALLOC_core__RegexMatch_O = 68,
KIND_LISPALLOC_core__WeakPointer_O = 69,
KIND_BOOTSTRAP_core__StandardObject_O = 70,
KIND_BOOTSTRAP_core__Metaobject_O = 71,
KIND_BOOTSTRAP_core__Specializer_O = 72,
KIND_BOOTSTRAP_core__Class_O = 73,
KIND_BOOTSTRAP_core__StdClass_O = 74,
KIND_BOOTSTRAP_core__StandardClass_O = 75,
KIND_LISPALLOC_core__FuncallableStandardClass_O = 76,
KIND_BOOTSTRAP_core__StructureClass_O = 77,
KIND_LISPALLOC_core__ForwardReferencedClass_O = 78,
KIND_LISPALLOC_core__CxxClass_O = 79,
KIND_BOOTSTRAP_core__BuiltInClass_O = 80,
KIND_LISPALLOC_clbind__ClassRep_O = 81,
KIND_LISPALLOC_core__ExternalObject_O = 82,
KIND_LISPALLOC_llvmo__Value_O = 83,
KIND_LISPALLOC_llvmo__Argument_O = 84,
KIND_LISPALLOC_llvmo__User_O = 85,
KIND_LISPALLOC_llvmo__Instruction_O = 86,
KIND_LISPALLOC_llvmo__AtomicRMWInst_O = 87,
KIND_LISPALLOC_llvmo__LandingPadInst_O = 88,
KIND_LISPALLOC_llvmo__PHINode_O = 89,
KIND_LISPALLOC_llvmo__CallInst_O = 90,
KIND_LISPALLOC_llvmo__StoreInst_O = 91,
KIND_LISPALLOC_llvmo__UnaryInstruction_O = 92,
KIND_LISPALLOC_llvmo__LoadInst_O = 93,
KIND_LISPALLOC_llvmo__AllocaInst_O = 94,
KIND_LISPALLOC_llvmo__VAArgInst_O = 95,
KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O = 96,
KIND_LISPALLOC_llvmo__TerminatorInst_O = 97,
KIND_LISPALLOC_llvmo__UnreachableInst_O = 98,
KIND_LISPALLOC_llvmo__SwitchInst_O = 99,
KIND_LISPALLOC_llvmo__ReturnInst_O = 100,
KIND_LISPALLOC_llvmo__ResumeInst_O = 101,
KIND_LISPALLOC_llvmo__BranchInst_O = 102,
KIND_LISPALLOC_llvmo__InvokeInst_O = 103,
KIND_LISPALLOC_llvmo__IndirectBrInst_O = 104,
KIND_LISPALLOC_llvmo__FenceInst_O = 105,
KIND_LISPALLOC_llvmo__Constant_O = 106,
KIND_LISPALLOC_llvmo__BlockAddress_O = 107,
KIND_LISPALLOC_llvmo__GlobalValue_O = 108,
KIND_LISPALLOC_llvmo__GlobalVariable_O = 109,
KIND_LISPALLOC_llvmo__Function_O = 110,
KIND_LISPALLOC_llvmo__ConstantArray_O = 111,
KIND_LISPALLOC_llvmo__ConstantInt_O = 112,
KIND_LISPALLOC_llvmo__ConstantDataSequential_O = 113,
KIND_LISPALLOC_llvmo__ConstantDataArray_O = 114,
KIND_LISPALLOC_llvmo__ConstantStruct_O = 115,
KIND_LISPALLOC_llvmo__ConstantFP_O = 116,
KIND_LISPALLOC_llvmo__UndefValue_O = 117,
KIND_LISPALLOC_llvmo__ConstantPointerNull_O = 118,
KIND_LISPALLOC_llvmo__ConstantExpr_O = 119,
KIND_LISPALLOC_llvmo__BasicBlock_O = 120,
KIND_LISPALLOC_llvmo__IRBuilderBase_O = 121,
KIND_LISPALLOC_llvmo__IRBuilder_O = 122,
KIND_LISPALLOC_llvmo__DIBuilder_O = 123,
KIND_LISPALLOC_llvmo__Metadata_O = 124,
KIND_LISPALLOC_llvmo__ValueAsMetadata_O = 125,
KIND_LISPALLOC_llvmo__MDNode_O = 126,
KIND_LISPALLOC_llvmo__MDString_O = 127,
KIND_LISPALLOC_llvmo__ExecutionEngine_O = 128,
KIND_LISPALLOC_llvmo__APFloat_O = 129,
KIND_LISPALLOC_llvmo__PassManagerBuilder_O = 130,
KIND_LISPALLOC_llvmo__DataLayout_O = 131,
KIND_LISPALLOC_llvmo__Triple_O = 132,
KIND_LISPALLOC_llvmo__APInt_O = 133,
KIND_LISPALLOC_llvmo__PassManagerBase_O = 134,
KIND_LISPALLOC_llvmo__FunctionPassManager_O = 135,
KIND_LISPALLOC_llvmo__PassManager_O = 136,
KIND_LISPALLOC_llvmo__TargetMachine_O = 137,
KIND_LISPALLOC_llvmo__LLVMTargetMachine_O = 138,
KIND_LISPALLOC_llvmo__TargetOptions_O = 139,
KIND_LISPALLOC_llvmo__Type_O = 140,
KIND_LISPALLOC_llvmo__IntegerType_O = 141,
KIND_LISPALLOC_llvmo__CompositeType_O = 142,
KIND_LISPALLOC_llvmo__SequentialType_O = 143,
KIND_LISPALLOC_llvmo__VectorType_O = 144,
KIND_LISPALLOC_llvmo__PointerType_O = 145,
KIND_LISPALLOC_llvmo__ArrayType_O = 146,
KIND_LISPALLOC_llvmo__StructType_O = 147,
KIND_LISPALLOC_llvmo__FunctionType_O = 148,
KIND_LISPALLOC_llvmo__NamedMDNode_O = 149,
KIND_LISPALLOC_llvmo__Linker_O = 150,
KIND_LISPALLOC_llvmo__Pass_O = 151,
KIND_LISPALLOC_llvmo__FunctionPass_O = 152,
KIND_LISPALLOC_llvmo__ModulePass_O = 153,
KIND_LISPALLOC_llvmo__ImmutablePass_O = 154,
KIND_LISPALLOC_llvmo__DataLayoutPass_O = 155,
KIND_LISPALLOC_llvmo__TargetLibraryInfo_O = 156,
KIND_LISPALLOC_llvmo__MCSubtargetInfo_O = 157,
KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O = 158,
KIND_LISPALLOC_llvmo__Module_O = 159,
KIND_LISPALLOC_llvmo__EngineBuilder_O = 160,
KIND_LISPALLOC_core__ForeignData_O = 161,
KIND_LISPALLOC_llvmo__LLVMContext_O = 162,
KIND_LISPALLOC_llvmo__Target_O = 163,
KIND_LISPALLOC_core__LoadTimeValues_O = 164,
KIND_LISPALLOC_core__Binder_O = 165,
KIND_LISPALLOC_core__IntArray_O = 166,
KIND_LISPALLOC_core__SourceManager_O = 167,
KIND_LISPALLOC_core__Record_O = 168,
KIND_LISPALLOC_core__Functor_O = 169,
KIND_LISPALLOC_core__Closure_O = 170,
KIND_LISPALLOC_core__FunctionClosure_O = 171,
KIND_LISPALLOC_core__InstanceClosure_O = 172,
KIND_LISPALLOC_core__CompiledClosure_O = 173,
KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O = 174,
KIND_LISPALLOC_core__MacroClosure_O = 175,
KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O = 176,
KIND_LISPALLOC_core__InterpretedClosure_O = 177,
KIND_LISPALLOC_core__LightUserData_O = 178,
KIND_LISPALLOC_core__UserData_O = 179,
KIND_BOOTSTRAP_core__Symbol_O = 180,
KIND_LISPALLOC_core__Null_O = 181,
KIND_LISPALLOC_core__SourcePosInfo_O = 182,
KIND_TEMPLATED_LISPALLOC_core__Iterator_O = 183,
KIND_LISPALLOC_core__DirectoryIterator_O = 184,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 185,
KIND_LISPALLOC_core__Regex_O = 186,
KIND_LISPALLOC_core__PosixTimeDuration_O = 187,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 188,
KIND_LISPALLOC_core__CandoException_O = 189,
KIND_LISPALLOC_core__Stream_O = 190,
KIND_LISPALLOC_core__AnsiStream_O = 191,
KIND_LISPALLOC_core__FileStream_O = 192,
KIND_LISPALLOC_core__IOStreamStream_O = 193,
KIND_LISPALLOC_core__IOFileStream_O = 194,
KIND_LISPALLOC_core__ConcatenatedStream_O = 195,
KIND_LISPALLOC_core__StringStream_O = 196,
KIND_LISPALLOC_core__StringInputStream_O = 197,
KIND_LISPALLOC_core__StringOutputStream_O = 198,
KIND_LISPALLOC_core__SynonymStream_O = 199,
KIND_LISPALLOC_core__EchoStream_O = 200,
KIND_LISPALLOC_core__TwoWayStream_O = 201,
KIND_LISPALLOC_core__BroadcastStream_O = 202,
KIND_LISPALLOC_core__Reader_O = 203,
KIND_LISPALLOC_core__SharpEqualWrapper_O = 204,
KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O = 205,
KIND_LISPALLOC_core__Archive_O = 206,
KIND_LISPALLOC_core__SaveArchive_O = 207,
KIND_LISPALLOC_core__SexpSaveArchive_O = 208,
KIND_LISPALLOC_core__LoadArchive_O = 209,
KIND_LISPALLOC_core__SexpLoadArchive_O = 210,
KIND_LISPALLOC_core__HashTable_O = 211,
KIND_LISPALLOC_core__HashTableEq_O = 212,
KIND_LISPALLOC_core__HashTableEqualp_O = 213,
KIND_LISPALLOC_core__HashTableEql_O = 214,
KIND_LISPALLOC_core__HashTableEqual_O = 215,
KIND_TEMPLATED_LISPALLOC_core__Creator_O = 216,
KIND_LISPALLOC_clbind__DummyCreator_O = 217,
KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O = 218,
KIND_LISPALLOC_core__InstanceCreator_O = 219,
KIND_LISPALLOC_cffi__Pointer_O = 220,
KIND_LISPALLOC_core__CxxObject_O = 221,
KIND_LISPALLOC_core__WeakKeyMapping_O = 222,
KIND_LISPALLOC_core__Cache_O = 223,
KIND_LISPALLOC_core__LambdaListHandler_O = 224,
KIND_LISPALLOC_llvmo__InsertPoint_O = 225,
KIND_LISPALLOC_core__SourceFileInfo_O = 226,
KIND_LISPALLOC_core__SNode_O = 227,
KIND_LISPALLOC_core__LeafSNode_O = 228,
KIND_LISPALLOC_core__BranchSNode_O = 229,
KIND_LISPALLOC_core__Path_O = 230,
KIND_LISPALLOC_asttooling__AstVisitor_O = 231,
KIND_LISPALLOC_llvmo__AttributeSet_O = 232,
KIND_LISPALLOC_core__StructureObject_O = 233,
KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O = 234,
KIND_LISPALLOC_core__Package_O = 235,
KIND_LISPALLOC_core__DirectoryEntry_O = 236,
KIND_LISPALLOC_core__Character_dummy_O = 237,
KIND_LISPALLOC_core__Function_O = 238,
KIND_LISPALLOC_core__CompiledFunction_O = 239,
KIND_LISPALLOC_core__SingleDispatchGenericFunction_O = 240,
KIND_LISPALLOC_core__SpecialForm_O = 241,
KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O = 242,
KIND_LISPALLOC_core__Instance_O = 243,
KIND_LISPALLOC_core__Pointer_O = 244,
KIND_LISPALLOC_clbind__ClassRegistry_O = 245,
KIND_LISPALLOC_llvmo__DebugInfo_O = 246,
KIND_LISPALLOC_llvmo__DIDerivedType_O = 247,
KIND_LISPALLOC_llvmo__DIArray_O = 248,
KIND_LISPALLOC_llvmo__DIBasicType_O = 249,
KIND_LISPALLOC_llvmo__DISubprogram_O = 250,
KIND_LISPALLOC_llvmo__DILexicalBlock_O = 251,
KIND_LISPALLOC_llvmo__DICompileUnit_O = 252,
KIND_LISPALLOC_llvmo__DIDescriptor_O = 253,
KIND_LISPALLOC_llvmo__DIType_O = 254,
KIND_LISPALLOC_llvmo__DISubroutineType_O = 255,
KIND_LISPALLOC_llvmo__DICompositeType_O = 256,
KIND_LISPALLOC_llvmo__DITypeArray_O = 257,
KIND_LISPALLOC_llvmo__DIFile_O = 258,
KIND_LISPALLOC_llvmo__DIScope_O = 259,
KIND_LISPALLOC_core__SmallMultimap_O = 260,
KIND_LISPALLOC_core__Pathname_O = 261,
KIND_LISPALLOC_core__LogicalPathname_O = 262,
KIND_LISPALLOC_core__PosixTime_O = 263,
KIND_LISPALLOC_core__SmallMap_O = 264,
KIND_ROOTCLASSALLOC_core__Lisp_O = 265,
KIND_LISPALLOC_core__CoreExposer_O = 266,
KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ = 267,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 268,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 269,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 270,
KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ = 271,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_ = 272,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 273,
KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O = 274,
KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___ = 275,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_ = 276,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ = 277,
KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_ = 278,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__ = 279,
KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ = 280,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_ = 281,
KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O = 282,
KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O = 283,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 284,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 285,
KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___ = 286,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 287,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ = 288,
KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory = 289,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_ = 290,
KIND_LISPALLOC_asttooling__DerivableMatchCallback = 291,
KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O = 292,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_ = 293,
KIND_LISPALLOC_asttooling__DerivableASTFrontendAction = 294,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ = 295,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_ = 296,
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
      // low high --> 95 95 
      return (kindVal == 95);
  };
};
template <typename FP> struct Cast<core::LoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 209 210 
      return ((209 <= kindVal) && (kindVal <= 210));
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
      // low high --> 171 177 
      return ((171 <= kindVal) && (kindVal <= 177));
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
      // low high --> 163 163 
      return (kindVal == 163);
  };
};
template <typename FP> struct Cast<llvmo::FunctionType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 148 148 
      return (kindVal == 148);
  };
};
template <typename FP> struct Cast<llvmo::ConstantExpr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 119 119 
      return (kindVal == 119);
  };
};
template <typename FP> struct Cast<llvmo::LLVMContext_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 162 162 
      return (kindVal == 162);
  };
};
template <typename FP> struct Cast<core::Lisp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 265 265 
      return (kindVal == 265);
  };
};
template <typename FP> struct Cast<llvmo::TargetSubtargetInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 158 158 
      return (kindVal == 158);
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
      // low high --> 264 264 
      return (kindVal == 264);
  };
};
template <typename FP> struct Cast<llvmo::FenceInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 105 105 
      return (kindVal == 105);
  };
};
template <typename FP> struct Cast<core::PosixTime_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 263 263 
      return (kindVal == 263);
  };
};
template <typename FP> struct Cast<core::Pathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 261 262 
      return ((261 <= kindVal) && (kindVal <= 262));
  };
};
template <typename FP> struct Cast<core::MacroClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 175 175 
      return (kindVal == 175);
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
      // low high --> 202 202 
      return (kindVal == 202);
  };
};
template <typename FP> struct Cast<core::Instance_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 243 243 
      return (kindVal == 243);
  };
};
template <typename FP> struct Cast<core::VectorStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 65 65 
      return (kindVal == 65);
  };
};
template <typename FP> struct Cast<llvmo::DIScope_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 259 259 
      return (kindVal == 259);
  };
};
template <typename FP> struct Cast<core::ForeignData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 161 161 
      return (kindVal == 161);
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
      // low high --> 104 104 
      return (kindVal == 104);
  };
};
template <typename FP> struct Cast<core::InterpretedClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 177 177 
      return (kindVal == 177);
  };
};
template <typename FP> struct Cast<llvmo::ConstantPointerNull_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 118 118 
      return (kindVal == 118);
  };
};
template <typename FP> struct Cast<llvmo::EngineBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 160 160 
      return (kindVal == 160);
  };
};
template <typename FP> struct Cast<llvmo::Module_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 159 159 
      return (kindVal == 159);
  };
};
template <typename FP> struct Cast<core::SmallMultimap_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 260 260 
      return (kindVal == 260);
  };
};
template <typename FP> struct Cast<core::LogicalPathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 262 262 
      return (kindVal == 262);
  };
};
template <typename FP> struct Cast<llvmo::DebugInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 246 259 
      return ((246 <= kindVal) && (kindVal <= 259));
  };
};
template <typename FP> struct Cast<clbind::ClassRegistry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 245 245 
      return (kindVal == 245);
  };
};
template <typename FP> struct Cast<core::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 244 244 
      return (kindVal == 244);
  };
};
template <typename FP> struct Cast<core::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 238 243 
      return ((238 <= kindVal) && (kindVal <= 243));
  };
};
template <typename FP> struct Cast<core::HashTableEqual_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 215 215 
      return (kindVal == 215);
  };
};
template <typename FP> struct Cast<core::Character_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 237 237 
      return (kindVal == 237);
  };
};
template <typename FP> struct Cast<core::DirectoryEntry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 236 236 
      return (kindVal == 236);
  };
};
template <typename FP> struct Cast<llvmo::MCSubtargetInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 157 158 
      return ((157 <= kindVal) && (kindVal <= 158));
  };
};
template <typename FP> struct Cast<llvmo::MDString_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 127 127 
      return (kindVal == 127);
  };
};
template <typename FP> struct Cast<core::Class_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 73 81 
      return ((73 <= kindVal) && (kindVal <= 81));
  };
};
template <typename FP> struct Cast<core::FuncallableStandardClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 76 76 
      return (kindVal == 76);
  };
};
template <typename FP> struct Cast<llvmo::DIFile_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 258 258 
      return (kindVal == 258);
  };
};
template <typename FP> struct Cast<llvmo::Pass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 151 156 
      return ((151 <= kindVal) && (kindVal <= 156));
  };
};
template <typename FP> struct Cast<llvmo::Linker_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 150 150 
      return (kindVal == 150);
  };
};
template <typename FP> struct Cast<llvmo::TerminatorInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 97 104 
      return ((97 <= kindVal) && (kindVal <= 104));
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
      // low high --> 117 117 
      return (kindVal == 117);
  };
};
template <typename FP> struct Cast<core::TwoWayStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 201 201 
      return (kindVal == 201);
  };
};
template <typename FP> struct Cast<core::IOFileStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 194 194 
      return (kindVal == 194);
  };
};
template <typename FP> struct Cast<llvmo::NamedMDNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 149 149 
      return (kindVal == 149);
  };
};
template <typename FP> struct Cast<llvmo::ArrayType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 146 146 
      return (kindVal == 146);
  };
};
template <typename FP> struct Cast<core::RecursiveDirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 185 185 
      return (kindVal == 185);
  };
};
template <typename FP> struct Cast<llvmo::Type_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 140 148 
      return ((140 <= kindVal) && (kindVal <= 148));
  };
};
template <typename FP> struct Cast<llvmo::AtomicCmpXchgInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 96 96 
      return (kindVal == 96);
  };
};
template <typename FP> struct Cast<llvmo::AllocaInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 94 94 
      return (kindVal == 94);
  };
};
template <typename FP> struct Cast<core::Package_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 235 235 
      return (kindVal == 235);
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
      // low high --> 120 120 
      return (kindVal == 120);
  };
};
template <typename FP> struct Cast<llvmo::CompositeType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 142 147 
      return ((142 <= kindVal) && (kindVal <= 147));
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
      // low high --> 136 136 
      return (kindVal == 136);
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
      // low high --> 193 193 
      return (kindVal == 193);
  };
};
template <typename FP> struct Cast<core::InvocationHistoryFrameIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 234 234 
      return (kindVal == 234);
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
      // low high --> 233 233 
      return (kindVal == 233);
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
      // low high --> 232 232 
      return (kindVal == 232);
  };
};
template <typename FP> struct Cast<core::BuiltInClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 80 81 
      return ((80 <= kindVal) && (kindVal <= 81));
  };
};
template <typename FP> struct Cast<core::CxxClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 79 79 
      return (kindVal == 79);
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
      // low high --> 184 184 
      return (kindVal == 184);
  };
};
template <typename FP> struct Cast<core::SexpLoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 210 210 
      return (kindVal == 210);
  };
};
template <typename FP> struct Cast<core::StringOutputStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 198 198 
      return (kindVal == 198);
  };
};
template <typename FP> struct Cast<asttooling::AstVisitor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 231 231 
      return (kindVal == 231);
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
      // low high --> 230 230 
      return (kindVal == 230);
  };
};
template <typename FP> struct Cast<llvmo::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 110 110 
      return (kindVal == 110);
  };
};
template <typename FP> struct Cast<llvmo::InvokeInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 103 103 
      return (kindVal == 103);
  };
};
template <typename FP> struct Cast<core::SNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 227 229 
      return ((227 <= kindVal) && (kindVal <= 229));
  };
};
template <typename FP> struct Cast<core::SourceFileInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 226 226 
      return (kindVal == 226);
  };
};
template <typename FP> struct Cast<llvmo::InsertPoint_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 225 225 
      return (kindVal == 225);
  };
};
template <typename FP> struct Cast<llvmo::DITypeArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 257 257 
      return (kindVal == 257);
  };
};
template <typename FP> struct Cast<core::ForwardReferencedClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 78 78 
      return (kindVal == 78);
  };
};
template <typename FP> struct Cast<core::HashTableEql_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 214 214 
      return (kindVal == 214);
  };
};
template <typename FP> struct Cast<core::LambdaListHandler_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 224 224 
      return (kindVal == 224);
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
      // low high --> 223 223 
      return (kindVal == 223);
  };
};
template <typename FP> struct Cast<llvmo::TargetOptions_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 139 139 
      return (kindVal == 139);
  };
};
template <typename FP> struct Cast<core::Metaobject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 71 81 
      return ((71 <= kindVal) && (kindVal <= 81));
  };
};
template <typename FP> struct Cast<llvmo::PointerType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 145 145 
      return (kindVal == 145);
  };
};
template <typename FP> struct Cast<core::WeakKeyMapping_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 222 222 
      return (kindVal == 222);
  };
};
template <typename FP> struct Cast<core::CxxObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 221 221 
      return (kindVal == 221);
  };
};
template <typename FP> struct Cast<cffi::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 220 220 
      return (kindVal == 220);
  };
};
template <typename FP> struct Cast<core::InstanceCreator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 219 219 
      return (kindVal == 219);
  };
};
template <typename FP> struct Cast<llvmo::DICompositeType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 256 256 
      return (kindVal == 256);
  };
};
template <typename FP> struct Cast<core::General_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 9 264 
      return ((9 <= kindVal) && (kindVal <= 264));
  };
};
template <typename FP> struct Cast<core::Creator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 216 219 
      return ((216 <= kindVal) && (kindVal <= 219));
  };
};
template <typename FP> struct Cast<llvmo::BranchInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 102 102 
      return (kindVal == 102);
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
      // low high --> 211 215 
      return ((211 <= kindVal) && (kindVal <= 215));
  };
};
template <typename FP> struct Cast<core::Archive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 206 210 
      return ((206 <= kindVal) && (kindVal <= 210));
  };
};
template <typename FP> struct Cast<asttooling::RegMap::RegistryMaps_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 205 205 
      return (kindVal == 205);
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
      // low high --> 204 204 
      return (kindVal == 204);
  };
};
template <typename FP> struct Cast<core::SingleDispatchGenericFunctionClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 176 176 
      return (kindVal == 176);
  };
};
template <typename FP> struct Cast<core::Reader_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 203 203 
      return (kindVal == 203);
  };
};
template <typename FP> struct Cast<core::Stream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 190 202 
      return ((190 <= kindVal) && (kindVal <= 202));
  };
};
template <typename FP> struct Cast<llvmo::UnaryInstruction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 92 95 
      return ((92 <= kindVal) && (kindVal <= 95));
  };
};
template <typename FP> struct Cast<core::BranchSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 229 229 
      return (kindVal == 229);
  };
};
template <typename FP> struct Cast<llvmo::FunctionPassManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 135 135 
      return (kindVal == 135);
  };
};
template <typename FP> struct Cast<core::SingleDispatchEffectiveMethodFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 242 242 
      return (kindVal == 242);
  };
};
template <typename FP> struct Cast<core::StringInputStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 197 197 
      return (kindVal == 197);
  };
};
template <typename FP> struct Cast<llvmo::TargetMachine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 137 138 
      return ((137 <= kindVal) && (kindVal <= 138));
  };
};
template <typename FP> struct Cast<core::CandoException_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 189 189 
      return (kindVal == 189);
  };
};
template <typename FP> struct Cast<llvmo::GlobalVariable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 109 109 
      return (kindVal == 109);
  };
};
template <typename FP> struct Cast<core::EchoStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 200 200 
      return (kindVal == 200);
  };
};
template <typename FP> struct Cast<core::SymbolToEnumConverter_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 188 188 
      return (kindVal == 188);
  };
};
template <typename FP> struct Cast<core::PosixTimeDuration_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 187 187 
      return (kindVal == 187);
  };
};
template <typename FP> struct Cast<core::StandardClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 75 75 
      return (kindVal == 75);
  };
};
template <typename FP> struct Cast<core::Regex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 186 186 
      return (kindVal == 186);
  };
};
template <typename FP> struct Cast<llvmo::PassManagerBase_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 134 136 
      return ((134 <= kindVal) && (kindVal <= 136));
  };
};
template <typename FP> struct Cast<core::Iterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 183 185 
      return ((183 <= kindVal) && (kindVal <= 185));
  };
};
template <typename FP> struct Cast<clbind::ClassRep_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 81 81 
      return (kindVal == 81);
  };
};
template <typename FP> struct Cast<llvmo::IRBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 122 122 
      return (kindVal == 122);
  };
};
template <typename FP> struct Cast<llvmo::StoreInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 91 91 
      return (kindVal == 91);
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
      // low high --> 101 101 
      return (kindVal == 101);
  };
};
template <typename FP> struct Cast<llvmo::TargetLibraryInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 156 156 
      return (kindVal == 156);
  };
};
template <typename FP> struct Cast<llvmo::DISubroutineType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 255 255 
      return (kindVal == 255);
  };
};
template <typename FP> struct Cast<llvmo::DIType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 254 254 
      return (kindVal == 254);
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
      // low high --> 126 126 
      return (kindVal == 126);
  };
};
template <typename FP> struct Cast<core::SourcePosInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 182 182 
      return (kindVal == 182);
  };
};
template <typename FP> struct Cast<llvmo::DIDescriptor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 253 253 
      return (kindVal == 253);
  };
};
template <typename FP> struct Cast<llvmo::LLVMTargetMachine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 138 138 
      return (kindVal == 138);
  };
};
template <typename FP> struct Cast<llvmo::APInt_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 133 133 
      return (kindVal == 133);
  };
};
template <typename FP> struct Cast<llvmo::ReturnInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 100 100 
      return (kindVal == 100);
  };
};
template <typename FP> struct Cast<core::ConsStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 64 64 
      return (kindVal == 64);
  };
};
template <typename FP> struct Cast<llvmo::Triple_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 132 132 
      return (kindVal == 132);
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
      // low high --> 180 181 
      return ((180 <= kindVal) && (kindVal <= 181));
  };
};
template <typename FP> struct Cast<llvmo::DataLayout_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 131 131 
      return (kindVal == 131);
  };
};
template <typename FP> struct Cast<core::LightUserData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 178 179 
      return ((178 <= kindVal) && (kindVal <= 179));
  };
};
template <typename FP> struct Cast<core::Functor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 169 177 
      return ((169 <= kindVal) && (kindVal <= 177));
  };
};
template <typename FP> struct Cast<core::T_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 6 264 
      return ((6 <= kindVal) && (kindVal <= 264));
  };
};
template <typename FP> struct Cast<core::Null_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 181 181 
      return (kindVal == 181);
  };
};
template <typename FP> struct Cast<llvmo::PassManagerBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 130 130 
      return (kindVal == 130);
  };
};
template <typename FP> struct Cast<core::Specializer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 72 81 
      return ((72 <= kindVal) && (kindVal <= 81));
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
      // low high --> 116 116 
      return (kindVal == 116);
  };
};
template <typename FP> struct Cast<llvmo::LoadInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 93 93 
      return (kindVal == 93);
  };
};
template <typename FP> struct Cast<llvmo::APFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 129 129 
      return (kindVal == 129);
  };
};
template <typename FP> struct Cast<core::Record_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 168 168 
      return (kindVal == 168);
  };
};
template <typename FP> struct Cast<core::SaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 207 208 
      return ((207 <= kindVal) && (kindVal <= 208));
  };
};
template <typename FP> struct Cast<core::SourceManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 167 167 
      return (kindVal == 167);
  };
};
template <typename FP> struct Cast<core::IntArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 166 166 
      return (kindVal == 166);
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
      // low high --> 228 228 
      return (kindVal == 228);
  };
};
template <typename FP> struct Cast<llvmo::Constant_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 106 119 
      return ((106 <= kindVal) && (kindVal <= 119));
  };
};
template <typename FP> struct Cast<core::Binder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 165 165 
      return (kindVal == 165);
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
      // low high --> 199 199 
      return (kindVal == 199);
  };
};
template <typename FP> struct Cast<core::LoadTimeValues_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 164 164 
      return (kindVal == 164);
  };
};
template <typename FP> struct Cast<core::ExternalObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 82 163 
      return ((82 <= kindVal) && (kindVal <= 163));
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
      // low high --> 70 81 
      return ((70 <= kindVal) && (kindVal <= 81));
  };
};
template <typename FP> struct Cast<llvmo::ImmutablePass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 154 156 
      return ((154 <= kindVal) && (kindVal <= 156));
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
      // low high --> 170 177 
      return ((170 <= kindVal) && (kindVal <= 177));
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
      // low high --> 147 147 
      return (kindVal == 147);
  };
};
template <typename FP> struct Cast<core::WeakPointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 69 69 
      return (kindVal == 69);
  };
};
template <typename FP> struct Cast<llvmo::ExecutionEngine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 128 128 
      return (kindVal == 128);
  };
};
template <typename FP> struct Cast<llvmo::ConstantDataArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 114 114 
      return (kindVal == 114);
  };
};
template <typename FP> struct Cast<core::RegexMatch_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 68 68 
      return (kindVal == 68);
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
      // low high --> 218 218 
      return (kindVal == 218);
  };
};
template <typename FP> struct Cast<core::BuiltinClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 174 175 
      return ((174 <= kindVal) && (kindVal <= 175));
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
      // low high --> 77 77 
      return (kindVal == 77);
  };
};
template <typename FP> struct Cast<llvmo::IntegerType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 141 141 
      return (kindVal == 141);
  };
};
template <typename FP> struct Cast<llvmo::Attribute_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 67 67 
      return (kindVal == 67);
  };
};
template <typename FP> struct Cast<core::HashTableEqualp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 213 213 
      return (kindVal == 213);
  };
};
template <typename FP> struct Cast<llvmo::DebugLoc_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 66 66 
      return (kindVal == 66);
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
      // low high --> 252 252 
      return (kindVal == 252);
  };
};
template <typename FP> struct Cast<llvmo::VectorType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 144 144 
      return (kindVal == 144);
  };
};
template <typename FP> struct Cast<core::StdClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 74 76 
      return ((74 <= kindVal) && (kindVal <= 76));
  };
};
template <typename FP> struct Cast<core::SequenceStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 63 65 
      return ((63 <= kindVal) && (kindVal <= 65));
  };
};
template <typename FP> struct Cast<llvmo::DILexicalBlock_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 251 251 
      return (kindVal == 251);
  };
};
template <typename FP> struct Cast<llvmo::Metadata_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 124 127 
      return ((124 <= kindVal) && (kindVal <= 127));
  };
};
template <typename FP> struct Cast<llvmo::User_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 85 119 
      return ((85 <= kindVal) && (kindVal <= 119));
  };
};
template <typename FP> struct Cast<core::WrappedPointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 62 62 
      return (kindVal == 62);
  };
};
template <typename FP> struct Cast<core::UserData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 179 179 
      return (kindVal == 179);
  };
};
template <typename FP> struct Cast<core::RandomState_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 61 61 
      return (kindVal == 61);
  };
};
template <typename FP> struct Cast<llvmo::SequentialType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 143 146 
      return ((143 <= kindVal) && (kindVal <= 146));
  };
};
template <typename FP> struct Cast<llvmo::ConstantStruct_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 115 115 
      return (kindVal == 115);
  };
};
template <typename FP> struct Cast<llvmo::CallInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 90 90 
      return (kindVal == 90);
  };
};
template <typename FP> struct Cast<llvmo::SwitchInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 99 99 
      return (kindVal == 99);
  };
};
template <typename FP> struct Cast<core::SexpSaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 208 208 
      return (kindVal == 208);
  };
};
template <typename FP> struct Cast<core::SingleDispatchMethod_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 60 60 
      return (kindVal == 60);
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
template <typename FP> struct Cast<core::StringStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 196 198 
      return ((196 <= kindVal) && (kindVal <= 198));
  };
};
template <typename FP> struct Cast<llvmo::DIBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 123 123 
      return (kindVal == 123);
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
      // low high --> 113 114 
      return ((113 <= kindVal) && (kindVal <= 114));
  };
};
template <typename FP> struct Cast<llvmo::ConstantInt_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 112 112 
      return (kindVal == 112);
  };
};
template <typename FP> struct Cast<core::CompiledClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 173 173 
      return (kindVal == 173);
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
      // low high --> 125 125 
      return (kindVal == 125);
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
      // low high --> 98 98 
      return (kindVal == 98);
  };
};
template <typename FP> struct Cast<core::ConcatenatedStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 195 195 
      return (kindVal == 195);
  };
};
template <typename FP> struct Cast<llvmo::PHINode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 89 89 
      return (kindVal == 89);
  };
};
template <typename FP> struct Cast<llvmo::Instruction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 86 105 
      return ((86 <= kindVal) && (kindVal <= 105));
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
      // low high --> 191 202 
      return ((191 <= kindVal) && (kindVal <= 202));
  };
};
template <typename FP> struct Cast<core::SpecialForm_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 241 241 
      return (kindVal == 241);
  };
};
template <typename FP> struct Cast<llvmo::ConstantArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 111 111 
      return (kindVal == 111);
  };
};
template <typename FP> struct Cast<core::InstanceClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 172 172 
      return (kindVal == 172);
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
      // low high --> 250 250 
      return (kindVal == 250);
  };
};
template <typename FP> struct Cast<llvmo::DIBasicType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 249 249 
      return (kindVal == 249);
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
      // low high --> 108 110 
      return ((108 <= kindVal) && (kindVal <= 110));
  };
};
template <typename FP> struct Cast<llvmo::LandingPadInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 88 88 
      return (kindVal == 88);
  };
};
template <typename FP> struct Cast<llvmo::IRBuilderBase_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 121 122 
      return ((121 <= kindVal) && (kindVal <= 122));
  };
};
template <typename FP> struct Cast<core::SingleDispatchGenericFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 240 240 
      return (kindVal == 240);
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
      // low high --> 83 120 
      return ((83 <= kindVal) && (kindVal <= 120));
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
      // low high --> 239 239 
      return (kindVal == 239);
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
      // low high --> 248 248 
      return (kindVal == 248);
  };
};
template <typename FP> struct Cast<llvmo::ModulePass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 153 156 
      return ((153 <= kindVal) && (kindVal <= 156));
  };
};
template <typename FP> struct Cast<llvmo::DIDerivedType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 247 247 
      return (kindVal == 247);
  };
};
template <typename FP> struct Cast<llvmo::Argument_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 84 84 
      return (kindVal == 84);
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
      // low high --> 107 107 
      return (kindVal == 107);
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
      // low high --> 152 152 
      return (kindVal == 152);
  };
};
template <typename FP> struct Cast<core::FileStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 192 194 
      return ((192 <= kindVal) && (kindVal <= 194));
  };
};
template <typename FP> struct Cast<llvmo::AtomicRMWInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 87 87 
      return (kindVal == 87);
  };
};
template <typename FP> struct Cast<core::HashTableEq_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 212 212 
      return (kindVal == 212);
  };
};
template <typename FP> struct Cast<llvmo::DataLayoutPass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 155 155 
      return (kindVal == 155);
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
      // low high --> 217 217 
      return (kindVal == 217);
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
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ ;
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
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_ ;
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
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_ ;
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
obj_scan_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_LISPALLOC_core__Creator_O:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O:
{
}
goto SCAN_ADVANCE;
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)
{ class_kind, KIND_ROOTCLASSALLOC_clbind__detail__class_map, sizeof(clbind::detail::class_map), 0, "clbind::detail::class_map" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>>), offsetof(SAFE_TYPE_MACRO(clbind::detail::class_map),m_classes._Vector._Contents), "m_classes._Vector._Contents" },
{ class_kind, KIND_BOOTSTRAP_core__T_O, sizeof(core::T_O), 0, "core::T_O" },
{ class_kind, KIND_LISPALLOC_core__VaList_dummy_O, sizeof(core::VaList_dummy_O), 0, "core::VaList_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__Cons_O, sizeof(core::Cons_O), 0, "core::Cons_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Cons_O),_Car), "_Car" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Cons_O),_Cdr), "_Cdr" },
{ class_kind, KIND_LISPALLOC_core__General_O, sizeof(core::General_O), 0, "core::General_O" },
{ class_kind, KIND_LISPALLOC_core__MultiStringBuffer_O, sizeof(core::MultiStringBuffer_O), 0, "core::MultiStringBuffer_O" },
{ class_kind, KIND_LISPALLOC_core__ReadTable_O, sizeof(core::ReadTable_O), 0, "core::ReadTable_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::ReadTable_O),_Case), "_Case" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::ReadTable_O),_SyntaxTypes), "_SyntaxTypes" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::ReadTable_O),_MacroCharacters), "_MacroCharacters" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::ReadTable_O),_DispatchMacroCharacters), "_DispatchMacroCharacters" },
{ class_kind, KIND_LISPALLOC_core__Number_O, sizeof(core::Number_O), 0, "core::Number_O" },
{ class_kind, KIND_LISPALLOC_core__Complex_O, sizeof(core::Complex_O), 0, "core::Complex_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_real), "_real" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_imaginary), "_imaginary" },
{ class_kind, KIND_LISPALLOC_core__Real_O, sizeof(core::Real_O), 0, "core::Real_O" },
{ class_kind, KIND_LISPALLOC_core__Rational_O, sizeof(core::Rational_O), 0, "core::Rational_O" },
{ class_kind, KIND_LISPALLOC_core__Integer_O, sizeof(core::Integer_O), 0, "core::Integer_O" },
{ class_kind, KIND_LISPALLOC_core__Bignum_O, sizeof(core::Bignum_O), 0, "core::Bignum_O" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::Bignum_O),_value.mp), "_value.mp" },
{ class_kind, KIND_LISPALLOC_core__Fixnum_dummy_O, sizeof(core::Fixnum_dummy_O), 0, "core::Fixnum_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__Ratio_O, sizeof(core::Ratio_O), 0, "core::Ratio_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_numerator), "_numerator" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_denominator), "_denominator" },
{ class_kind, KIND_LISPALLOC_core__Float_O, sizeof(core::Float_O), 0, "core::Float_O" },
{ class_kind, KIND_LISPALLOC_core__DoubleFloat_O, sizeof(core::DoubleFloat_O), 0, "core::DoubleFloat_O" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::DoubleFloat_O),_Value), "_Value" },
{ class_kind, KIND_LISPALLOC_core__LongFloat_O, sizeof(core::LongFloat_O), 0, "core::LongFloat_O" },
{ class_kind, KIND_LISPALLOC_core__SingleFloat_dummy_O, sizeof(core::SingleFloat_dummy_O), 0, "core::SingleFloat_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__ShortFloat_O, sizeof(core::ShortFloat_O), 0, "core::ShortFloat_O" },
{  fixed_field, ctype_float, sizeof(float), offsetof(SAFE_TYPE_MACRO(core::ShortFloat_O),_Value), "_Value" },
{ class_kind, KIND_LISPALLOC_core__FileStatus_O, sizeof(core::FileStatus_O), 0, "core::FileStatus_O" },
{ class_kind, KIND_LISPALLOC_core__WeakHashTable_O, sizeof(core::WeakHashTable_O), 0, "core::WeakHashTable_O" },
{ class_kind, KIND_LISPALLOC_core__WeakKeyHashTable_O, sizeof(core::WeakKeyHashTable_O), 0, "core::WeakKeyHashTable_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Length), "_HashTable._Length" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::WeakLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Keys), "_HashTable._Keys" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::StrongLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Values), "_HashTable._Values" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._epoch), "_HashTable._LocationDependency._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._rs), "_HashTable._LocationDependency._rs" },
{ class_kind, KIND_LISPALLOC_core__Environment_O, sizeof(core::Environment_O), 0, "core::Environment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Environment_O),_EnvId), "_EnvId" },
{ class_kind, KIND_LISPALLOC_core__ActivationFrame_O, sizeof(core::ActivationFrame_O), 0, "core::ActivationFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ActivationFrame_O),_EnvId), "_EnvId" },
{ class_kind, KIND_LISPALLOC_core__TagbodyFrame_O, sizeof(core::TagbodyFrame_O), 0, "core::TagbodyFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_ParentFrame), "_ParentFrame" },
{ class_kind, KIND_LISPALLOC_core__ValueFrame_O, sizeof(core::ValueFrame_O), 0, "core::ValueFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_ParentFrame), "_ParentFrame" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Objects._Array._Contents), "_Objects._Array._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_DebuggingInfo), "_DebuggingInfo" },
{ class_kind, KIND_LISPALLOC_core__FunctionFrame_O, sizeof(core::FunctionFrame_O), 0, "core::FunctionFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_ParentFrame), "_ParentFrame" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Objects._Array._Contents), "_Objects._Array._Contents" },
{ class_kind, KIND_LISPALLOC_core__LexicalEnvironment_O, sizeof(core::LexicalEnvironment_O), 0, "core::LexicalEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O, sizeof(core::RuntimeVisibleEnvironment_O), 0, "core::RuntimeVisibleEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{ class_kind, KIND_LISPALLOC_core__FunctionValueEnvironment_O, sizeof(core::FunctionValueEnvironment_O), 0, "core::FunctionValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionIndices), "_FunctionIndices" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::FunctionFrame_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionFrame), "_FunctionFrame" },
{ class_kind, KIND_LISPALLOC_core__ValueEnvironment_O, sizeof(core::ValueEnvironment_O), 0, "core::ValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_SymbolIndex), "_SymbolIndex" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ActivationFrame), "_ActivationFrame" },
{ class_kind, KIND_LISPALLOC_core__TagbodyEnvironment_O, sizeof(core::TagbodyEnvironment_O), 0, "core::TagbodyEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Tags), "_Tags" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_TagCode._Vector._Contents), "_TagCode._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ActivationFrame), "_ActivationFrame" },
{ class_kind, KIND_LISPALLOC_core__CompileTimeEnvironment_O, sizeof(core::CompileTimeEnvironment_O), 0, "core::CompileTimeEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__UnwindProtectEnvironment_O, sizeof(core::UnwindProtectEnvironment_O), 0, "core::UnwindProtectEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_CleanupForm), "_CleanupForm" },
{ class_kind, KIND_LISPALLOC_core__SymbolMacroletEnvironment_O, sizeof(core::SymbolMacroletEnvironment_O), 0, "core::SymbolMacroletEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Macros), "_Macros" },
{ class_kind, KIND_LISPALLOC_core__FunctionContainerEnvironment_O, sizeof(core::FunctionContainerEnvironment_O), 0, "core::FunctionContainerEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__StackValueEnvironment_O, sizeof(core::StackValueEnvironment_O), 0, "core::StackValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Values), "_Values" },
{ class_kind, KIND_LISPALLOC_core__BlockEnvironment_O, sizeof(core::BlockEnvironment_O), 0, "core::BlockEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_BlockSymbol), "_BlockSymbol" },
{ class_kind, KIND_LISPALLOC_core__MacroletEnvironment_O, sizeof(core::MacroletEnvironment_O), 0, "core::MacroletEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Macros), "_Macros" },
{ class_kind, KIND_LISPALLOC_core__CatchEnvironment_O, sizeof(core::CatchEnvironment_O), 0, "core::CatchEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__GlueEnvironment_O, sizeof(core::GlueEnvironment_O), 0, "core::GlueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Map), "_Map" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Args), "_Args" },
{ class_kind, KIND_LISPALLOC_core__Array_O, sizeof(core::Array_O), 0, "core::Array_O" },
{ class_kind, KIND_LISPALLOC_core__ArrayObjects_O, sizeof(core::ArrayObjects_O), 0, "core::ArrayObjects_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ArrayObjects_O),_ElementType), "_ElementType" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::ArrayObjects_O),_Values._Vector._Contents), "_Values._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__ArrayDisplaced_O, sizeof(core::ArrayDisplaced_O), 0, "core::ArrayDisplaced_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ArrayDisplaced_O),_ElementType), "_ElementType" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Array_O>), offsetof(SAFE_TYPE_MACRO(core::ArrayDisplaced_O),_Array), "_Array" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ArrayDisplaced_O),_DisplacedIndexOffset), "_DisplacedIndexOffset" },
{ class_kind, KIND_LISPALLOC_core__Vector_O, sizeof(core::Vector_O), 0, "core::Vector_O" },
{ class_kind, KIND_LISPALLOC_core__BitVector_O, sizeof(core::BitVector_O), 0, "core::BitVector_O" },
{ class_kind, KIND_LISPALLOC_core__SimpleBitVector_O, sizeof(core::SimpleBitVector_O), 0, "core::SimpleBitVector_O" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SimpleBitVector_O),_length), "_length" },
{ class_kind, KIND_LISPALLOC_core__BitVectorWithFillPtr_O, sizeof(core::BitVectorWithFillPtr_O), 0, "core::BitVectorWithFillPtr_O" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::BitVectorWithFillPtr_O),_fill_ptr), "_fill_ptr" },
{ class_kind, KIND_LISPALLOC_core__VectorDisplaced_O, sizeof(core::VectorDisplaced_O), 0, "core::VectorDisplaced_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorDisplaced_O),_ElementType), "_ElementType" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Vector_O>), offsetof(SAFE_TYPE_MACRO(core::VectorDisplaced_O),_Vector), "_Vector" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::VectorDisplaced_O),_Size), "_Size" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::VectorDisplaced_O),_DisplacedIndexOffset), "_DisplacedIndexOffset" },
{ class_kind, KIND_LISPALLOC_core__String_O, sizeof(core::String_O), 0, "core::String_O" },
{ class_kind, KIND_BOOTSTRAP_core__Str_O, sizeof(core::Str_O), 0, "core::Str_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::Str_O),_Contents._Contents), "_Contents._Contents" },
{ class_kind, KIND_LISPALLOC_core__StrWithFillPtr_O, sizeof(core::StrWithFillPtr_O), 0, "core::StrWithFillPtr_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::StrWithFillPtr_O),_Contents._Contents), "_Contents._Contents" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::StrWithFillPtr_O),_FillPointer), "_FillPointer" },
{ class_kind, KIND_LISPALLOC_core__VectorObjects_O, sizeof(core::VectorObjects_O), 0, "core::VectorObjects_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_ElementType), "_ElementType" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_Values._Vector._Contents), "_Values._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O, sizeof(core::VectorObjectsWithFillPtr_O), 0, "core::VectorObjectsWithFillPtr_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_ElementType), "_ElementType" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_Values._Vector._Contents), "_Values._Vector._Contents" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_FillPtr), "_FillPtr" },
{ class_kind, KIND_LISPALLOC_core__SingleDispatchMethod_O, sizeof(core::SingleDispatchMethod_O), 0, "core::SingleDispatchMethod_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchMethod_O),_name), "_name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchMethod_O),_receiver_class), "_receiver_class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Function_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchMethod_O),code), "code" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchMethod_O),_argument_handler), "_argument_handler" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchMethod_O),_declares), "_declares" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchMethod_O),_docstring), "_docstring" },
{ class_kind, KIND_LISPALLOC_core__RandomState_O, sizeof(core::RandomState_O), 0, "core::RandomState_O" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::RandomState_O),_Producer.x), "_Producer.x" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::RandomState_O),_Producer.i), "_Producer.i" },
{ templated_kind, KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O, sizeof(core::WrappedPointer_O), 0, "core::WrappedPointer_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::WrappedPointer_O),_Class), "_Class" },
{ templated_class_jump_table_index, 0, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_core__SequenceStepper_O, sizeof(core::SequenceStepper_O), 0, "core::SequenceStepper_O" },
{ class_kind, KIND_LISPALLOC_core__ConsStepper_O, sizeof(core::ConsStepper_O), 0, "core::ConsStepper_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::ConsStepper_O),_Cur), "_Cur" },
{ class_kind, KIND_LISPALLOC_core__VectorStepper_O, sizeof(core::VectorStepper_O), 0, "core::VectorStepper_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Vector_O>), offsetof(SAFE_TYPE_MACRO(core::VectorStepper_O),_Domain), "_Domain" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::VectorStepper_O),_Index), "_Index" },
{ class_kind, KIND_LISPALLOC_llvmo__DebugLoc_O, sizeof(llvmo::DebugLoc_O), 0, "llvmo::DebugLoc_O" },
{ class_kind, KIND_LISPALLOC_llvmo__Attribute_O, sizeof(llvmo::Attribute_O), 0, "llvmo::Attribute_O" },
{ class_kind, KIND_LISPALLOC_core__RegexMatch_O, sizeof(core::RegexMatch_O), 0, "core::RegexMatch_O" },
//badgen{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::RegexMatch_O),_Match.m_last_closed_paren), "_Match.m_last_closed_paren" },
{ class_kind, KIND_LISPALLOC_core__WeakPointer_O, sizeof(core::WeakPointer_O), 0, "core::WeakPointer_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::WeakPointer>), offsetof(SAFE_TYPE_MACRO(core::WeakPointer_O),_WeakObject.pointer), "_WeakObject.pointer" },
{ class_kind, KIND_BOOTSTRAP_core__StandardObject_O, sizeof(core::StandardObject_O), 0, "core::StandardObject_O" },
{ class_kind, KIND_BOOTSTRAP_core__Metaobject_O, sizeof(core::Metaobject_O), 0, "core::Metaobject_O" },
{ class_kind, KIND_BOOTSTRAP_core__Specializer_O, sizeof(core::Specializer_O), 0, "core::Specializer_O" },
{ class_kind, KIND_BOOTSTRAP_core__Class_O, sizeof(core::Class_O), 0, "core::Class_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_BOOTSTRAP_core__StdClass_O, sizeof(core::StdClass_O), 0, "core::StdClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StdClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::StdClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::StdClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_BOOTSTRAP_core__StandardClass_O, sizeof(core::StandardClass_O), 0, "core::StandardClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StandardClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::StandardClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::StandardClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::StandardClass_O),_InstanceCoreClass), "_InstanceCoreClass" },
{ class_kind, KIND_LISPALLOC_core__FuncallableStandardClass_O, sizeof(core::FuncallableStandardClass_O), 0, "core::FuncallableStandardClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FuncallableStandardClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::FuncallableStandardClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::FuncallableStandardClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::FuncallableStandardClass_O),_InstanceCoreClass), "_InstanceCoreClass" },
{ class_kind, KIND_BOOTSTRAP_core__StructureClass_O, sizeof(core::StructureClass_O), 0, "core::StructureClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StructureClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::StructureClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::StructureClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::StructureClass_O),_InstanceCoreClass), "_InstanceCoreClass" },
{ class_kind, KIND_LISPALLOC_core__ForwardReferencedClass_O, sizeof(core::ForwardReferencedClass_O), 0, "core::ForwardReferencedClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ForwardReferencedClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::ForwardReferencedClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::ForwardReferencedClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::BuiltInClass_O>), offsetof(SAFE_TYPE_MACRO(core::ForwardReferencedClass_O),_InstanceCoreClass), "_InstanceCoreClass" },
{ class_kind, KIND_LISPALLOC_core__CxxClass_O, sizeof(core::CxxClass_O), 0, "core::CxxClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CxxClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::CxxClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::CxxClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_BOOTSTRAP_core__BuiltInClass_O, sizeof(core::BuiltInClass_O), 0, "core::BuiltInClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltInClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltInClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::BuiltInClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_clbind__ClassRep_O, sizeof(clbind::ClassRep_O), 0, "clbind::ClassRep_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_bases._Vector._Contents), "m_bases._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__ExternalObject_O, sizeof(core::ExternalObject_O), 0, "core::ExternalObject_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::ExternalObject_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Value_O, sizeof(llvmo::Value_O), 0, "llvmo::Value_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Value_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Argument_O, sizeof(llvmo::Argument_O), 0, "llvmo::Argument_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Argument_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__User_O, sizeof(llvmo::User_O), 0, "llvmo::User_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::User_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Instruction_O, sizeof(llvmo::Instruction_O), 0, "llvmo::Instruction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Instruction_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__AtomicRMWInst_O, sizeof(llvmo::AtomicRMWInst_O), 0, "llvmo::AtomicRMWInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::AtomicRMWInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__LandingPadInst_O, sizeof(llvmo::LandingPadInst_O), 0, "llvmo::LandingPadInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LandingPadInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__PHINode_O, sizeof(llvmo::PHINode_O), 0, "llvmo::PHINode_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PHINode_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__CallInst_O, sizeof(llvmo::CallInst_O), 0, "llvmo::CallInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::CallInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__StoreInst_O, sizeof(llvmo::StoreInst_O), 0, "llvmo::StoreInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::StoreInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__UnaryInstruction_O, sizeof(llvmo::UnaryInstruction_O), 0, "llvmo::UnaryInstruction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::UnaryInstruction_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__LoadInst_O, sizeof(llvmo::LoadInst_O), 0, "llvmo::LoadInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LoadInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__AllocaInst_O, sizeof(llvmo::AllocaInst_O), 0, "llvmo::AllocaInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::AllocaInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__VAArgInst_O, sizeof(llvmo::VAArgInst_O), 0, "llvmo::VAArgInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::VAArgInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O, sizeof(llvmo::AtomicCmpXchgInst_O), 0, "llvmo::AtomicCmpXchgInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::AtomicCmpXchgInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TerminatorInst_O, sizeof(llvmo::TerminatorInst_O), 0, "llvmo::TerminatorInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TerminatorInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__UnreachableInst_O, sizeof(llvmo::UnreachableInst_O), 0, "llvmo::UnreachableInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::UnreachableInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__SwitchInst_O, sizeof(llvmo::SwitchInst_O), 0, "llvmo::SwitchInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::SwitchInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ReturnInst_O, sizeof(llvmo::ReturnInst_O), 0, "llvmo::ReturnInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ReturnInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ResumeInst_O, sizeof(llvmo::ResumeInst_O), 0, "llvmo::ResumeInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ResumeInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__BranchInst_O, sizeof(llvmo::BranchInst_O), 0, "llvmo::BranchInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::BranchInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__InvokeInst_O, sizeof(llvmo::InvokeInst_O), 0, "llvmo::InvokeInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::InvokeInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__IndirectBrInst_O, sizeof(llvmo::IndirectBrInst_O), 0, "llvmo::IndirectBrInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IndirectBrInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__FenceInst_O, sizeof(llvmo::FenceInst_O), 0, "llvmo::FenceInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FenceInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Constant_O, sizeof(llvmo::Constant_O), 0, "llvmo::Constant_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Constant_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__BlockAddress_O, sizeof(llvmo::BlockAddress_O), 0, "llvmo::BlockAddress_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::BlockAddress_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__GlobalValue_O, sizeof(llvmo::GlobalValue_O), 0, "llvmo::GlobalValue_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::GlobalValue_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__GlobalVariable_O, sizeof(llvmo::GlobalVariable_O), 0, "llvmo::GlobalVariable_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::GlobalVariable_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Function_O, sizeof(llvmo::Function_O), 0, "llvmo::Function_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Function_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LoadTimeValues_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Function_O),_RunTimeValues), "_RunTimeValues" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantArray_O, sizeof(llvmo::ConstantArray_O), 0, "llvmo::ConstantArray_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantArray_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantInt_O, sizeof(llvmo::ConstantInt_O), 0, "llvmo::ConstantInt_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantInt_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantDataSequential_O, sizeof(llvmo::ConstantDataSequential_O), 0, "llvmo::ConstantDataSequential_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantDataSequential_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantDataArray_O, sizeof(llvmo::ConstantDataArray_O), 0, "llvmo::ConstantDataArray_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantDataArray_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantStruct_O, sizeof(llvmo::ConstantStruct_O), 0, "llvmo::ConstantStruct_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantStruct_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantFP_O, sizeof(llvmo::ConstantFP_O), 0, "llvmo::ConstantFP_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantFP_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__UndefValue_O, sizeof(llvmo::UndefValue_O), 0, "llvmo::UndefValue_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::UndefValue_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantPointerNull_O, sizeof(llvmo::ConstantPointerNull_O), 0, "llvmo::ConstantPointerNull_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantPointerNull_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantExpr_O, sizeof(llvmo::ConstantExpr_O), 0, "llvmo::ConstantExpr_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantExpr_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__BasicBlock_O, sizeof(llvmo::BasicBlock_O), 0, "llvmo::BasicBlock_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::BasicBlock_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__IRBuilderBase_O, sizeof(llvmo::IRBuilderBase_O), 0, "llvmo::IRBuilderBase_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IRBuilderBase_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__IRBuilder_O, sizeof(llvmo::IRBuilder_O), 0, "llvmo::IRBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IRBuilder_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__DIBuilder_O, sizeof(llvmo::DIBuilder_O), 0, "llvmo::DIBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::DIBuilder_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Metadata_O, sizeof(llvmo::Metadata_O), 0, "llvmo::Metadata_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Metadata_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ValueAsMetadata_O, sizeof(llvmo::ValueAsMetadata_O), 0, "llvmo::ValueAsMetadata_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ValueAsMetadata_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__MDNode_O, sizeof(llvmo::MDNode_O), 0, "llvmo::MDNode_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::MDNode_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__MDString_O, sizeof(llvmo::MDString_O), 0, "llvmo::MDString_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::MDString_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ExecutionEngine_O, sizeof(llvmo::ExecutionEngine_O), 0, "llvmo::ExecutionEngine_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ExecutionEngine_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ExecutionEngine_O),_DependentModules), "_DependentModules" },
{ class_kind, KIND_LISPALLOC_llvmo__APFloat_O, sizeof(llvmo::APFloat_O), 0, "llvmo::APFloat_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::APFloat_O),_Class), "_Class" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(llvmo::APFloat_O),_value.significand.part), "_value.significand.part" },
//badgen{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(llvmo::APFloat_O),_value.sign), "_value.sign" },
{ class_kind, KIND_LISPALLOC_llvmo__PassManagerBuilder_O, sizeof(llvmo::PassManagerBuilder_O), 0, "llvmo::PassManagerBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__DataLayout_O, sizeof(llvmo::DataLayout_O), 0, "llvmo::DataLayout_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::DataLayout_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Triple_O, sizeof(llvmo::Triple_O), 0, "llvmo::Triple_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Triple_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__APInt_O, sizeof(llvmo::APInt_O), 0, "llvmo::APInt_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::APInt_O),_Class), "_Class" },
//badgen{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(llvmo::APInt_O),_value.BitWidth), "_value.BitWidth" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(llvmo::APInt_O),_value.NO-NAME.VAL), "_value.NO-NAME.VAL" },
{ class_kind, KIND_LISPALLOC_llvmo__PassManagerBase_O, sizeof(llvmo::PassManagerBase_O), 0, "llvmo::PassManagerBase_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBase_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__FunctionPassManager_O, sizeof(llvmo::FunctionPassManager_O), 0, "llvmo::FunctionPassManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FunctionPassManager_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__PassManager_O, sizeof(llvmo::PassManager_O), 0, "llvmo::PassManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PassManager_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetMachine_O, sizeof(llvmo::TargetMachine_O), 0, "llvmo::TargetMachine_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetMachine_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__LLVMTargetMachine_O, sizeof(llvmo::LLVMTargetMachine_O), 0, "llvmo::LLVMTargetMachine_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LLVMTargetMachine_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetOptions_O, sizeof(llvmo::TargetOptions_O), 0, "llvmo::TargetOptions_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetOptions_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Type_O, sizeof(llvmo::Type_O), 0, "llvmo::Type_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Type_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__IntegerType_O, sizeof(llvmo::IntegerType_O), 0, "llvmo::IntegerType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IntegerType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__CompositeType_O, sizeof(llvmo::CompositeType_O), 0, "llvmo::CompositeType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::CompositeType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__SequentialType_O, sizeof(llvmo::SequentialType_O), 0, "llvmo::SequentialType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::SequentialType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__VectorType_O, sizeof(llvmo::VectorType_O), 0, "llvmo::VectorType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::VectorType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__PointerType_O, sizeof(llvmo::PointerType_O), 0, "llvmo::PointerType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PointerType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ArrayType_O, sizeof(llvmo::ArrayType_O), 0, "llvmo::ArrayType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ArrayType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__StructType_O, sizeof(llvmo::StructType_O), 0, "llvmo::StructType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::StructType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__FunctionType_O, sizeof(llvmo::FunctionType_O), 0, "llvmo::FunctionType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FunctionType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__NamedMDNode_O, sizeof(llvmo::NamedMDNode_O), 0, "llvmo::NamedMDNode_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::NamedMDNode_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Linker_O, sizeof(llvmo::Linker_O), 0, "llvmo::Linker_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Linker_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Pass_O, sizeof(llvmo::Pass_O), 0, "llvmo::Pass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Pass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__FunctionPass_O, sizeof(llvmo::FunctionPass_O), 0, "llvmo::FunctionPass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FunctionPass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ModulePass_O, sizeof(llvmo::ModulePass_O), 0, "llvmo::ModulePass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ModulePass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ImmutablePass_O, sizeof(llvmo::ImmutablePass_O), 0, "llvmo::ImmutablePass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ImmutablePass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__DataLayoutPass_O, sizeof(llvmo::DataLayoutPass_O), 0, "llvmo::DataLayoutPass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::DataLayoutPass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetLibraryInfo_O, sizeof(llvmo::TargetLibraryInfo_O), 0, "llvmo::TargetLibraryInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetLibraryInfo_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__MCSubtargetInfo_O, sizeof(llvmo::MCSubtargetInfo_O), 0, "llvmo::MCSubtargetInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::MCSubtargetInfo_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O, sizeof(llvmo::TargetSubtargetInfo_O), 0, "llvmo::TargetSubtargetInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetSubtargetInfo_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Module_O, sizeof(llvmo::Module_O), 0, "llvmo::Module_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Module_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Module_O),_UniqueGlobalVariableStrings), "_UniqueGlobalVariableStrings" },
{ class_kind, KIND_LISPALLOC_llvmo__EngineBuilder_O, sizeof(llvmo::EngineBuilder_O), 0, "llvmo::EngineBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__ForeignData_O, sizeof(core::ForeignData_O), 0, "core::ForeignData_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_Kind), "_Kind" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_OwnershipFlags), "_OwnershipFlags" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_Size), "_Size" },
{ class_kind, KIND_LISPALLOC_llvmo__LLVMContext_O, sizeof(llvmo::LLVMContext_O), 0, "llvmo::LLVMContext_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LLVMContext_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Target_O, sizeof(llvmo::Target_O), 0, "llvmo::Target_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Target_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__LoadTimeValues_O, sizeof(core::LoadTimeValues_O), 0, "core::LoadTimeValues_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::LoadTimeValues_O),_Objects._Vector._Contents), "_Objects._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>>), offsetof(SAFE_TYPE_MACRO(core::LoadTimeValues_O),_Symbols._Vector._Contents), "_Symbols._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__Binder_O, sizeof(core::Binder_O), 0, "core::Binder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::Binder_O),_Bindings), "_Bindings" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::Binder_O),_Values), "_Values" },
{ class_kind, KIND_LISPALLOC_core__IntArray_O, sizeof(core::IntArray_O), 0, "core::IntArray_O" },
{ class_kind, KIND_LISPALLOC_core__SourceManager_O, sizeof(core::SourceManager_O), 0, "core::SourceManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::WeakKeyHashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SourceManager_O),_SourcePosInfo), "_SourcePosInfo" },
{ class_kind, KIND_LISPALLOC_core__Record_O, sizeof(core::Record_O), 0, "core::Record_O" },
//badgen{  fixed_field, ctype_core__Record_O__RecordStage, sizeof(core::Record_O::RecordStage), offsetof(SAFE_TYPE_MACRO(core::Record_O),_stage), "_stage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_alist), "_alist" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_replacement_table), "_replacement_table" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_Seen), "_Seen" },
{ class_kind, KIND_LISPALLOC_core__Functor_O, sizeof(core::Functor_O), 0, "core::Functor_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Functor_O),name), "name" },
{ class_kind, KIND_LISPALLOC_core__Closure_O, sizeof(core::Closure_O), 0, "core::Closure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Closure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Closure_O),closedEnvironment), "closedEnvironment" },
{ class_kind, KIND_LISPALLOC_core__FunctionClosure_O, sizeof(core::FunctionClosure_O), 0, "core::FunctionClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_column), "_column" },
{ class_kind, KIND_LISPALLOC_core__InstanceClosure_O, sizeof(core::InstanceClosure_O), 0, "core::InstanceClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Instance_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),instance), "instance" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceClosure_O),lambda_list), "lambda_list" },
{ class_kind, KIND_LISPALLOC_core__CompiledClosure_O, sizeof(core::CompiledClosure_O), 0, "core::CompiledClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),llvmFunction), "llvmFunction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),associatedFunctions), "associatedFunctions" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_lambdaList), "_lambdaList" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_Slots._Data), "_Slots._Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_Slots._Capacity), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_Slots._Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ templated_kind, KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O, sizeof(core::BuiltinClosure_O), 0, "core::BuiltinClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_lambdaListHandler), "_lambdaListHandler" },
{ templated_class_jump_table_index, 1, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_core__MacroClosure_O, sizeof(core::MacroClosure_O), 0, "core::MacroClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_lambdaListHandler), "_lambdaListHandler" },
{ class_kind, KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O, sizeof(core::SingleDispatchGenericFunctionClosure_O), 0, "core::SingleDispatchGenericFunctionClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_Methods), "_Methods" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunctionClosure_O),_lambdaListHandler), "_lambdaListHandler" },
{ class_kind, KIND_LISPALLOC_core__InterpretedClosure_O, sizeof(core::InterpretedClosure_O), 0, "core::InterpretedClosure_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_lambdaListHandler), "_lambdaListHandler" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_declares), "_declares" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_docstring), "_docstring" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_code), "_code" },
{ class_kind, KIND_LISPALLOC_core__LightUserData_O, sizeof(core::LightUserData_O), 0, "core::LightUserData_O" },
{ class_kind, KIND_LISPALLOC_core__UserData_O, sizeof(core::UserData_O), 0, "core::UserData_O" },
{ class_kind, KIND_BOOTSTRAP_core__Symbol_O, sizeof(core::Symbol_O), 0, "core::Symbol_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Str_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_HomePackage), "_HomePackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Value), "_Value" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Function), "_Function" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_SetfFunction), "_SetfFunction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_PropertyList), "_PropertyList" },
{ class_kind, KIND_LISPALLOC_core__Null_O, sizeof(core::Null_O), 0, "core::Null_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Str_O>), offsetof(SAFE_TYPE_MACRO(core::Null_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Null_O),_HomePackage), "_HomePackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Null_O),_Value), "_Value" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Null_O),_Function), "_Function" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Null_O),_SetfFunction), "_SetfFunction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Null_O),_PropertyList), "_PropertyList" },
{ class_kind, KIND_LISPALLOC_core__SourcePosInfo_O, sizeof(core::SourcePosInfo_O), 0, "core::SourcePosInfo_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_FileId), "_FileId" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Filepos), "_Filepos" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Lineno), "_Lineno" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Column), "_Column" },
{ templated_kind, KIND_TEMPLATED_LISPALLOC_core__Iterator_O, sizeof(core::Iterator_O), 0, "core::Iterator_O" },
{ templated_class_jump_table_index, 2, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_core__DirectoryIterator_O, sizeof(core::DirectoryIterator_O), 0, "core::DirectoryIterator_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Path_O>), offsetof(SAFE_TYPE_MACRO(core::DirectoryIterator_O),_Path), "_Path" },
{ class_kind, KIND_LISPALLOC_core__RecursiveDirectoryIterator_O, sizeof(core::RecursiveDirectoryIterator_O), 0, "core::RecursiveDirectoryIterator_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Path_O>), offsetof(SAFE_TYPE_MACRO(core::RecursiveDirectoryIterator_O),_Path), "_Path" },
{ class_kind, KIND_LISPALLOC_core__Regex_O, sizeof(core::Regex_O), 0, "core::Regex_O" },
{ class_kind, KIND_LISPALLOC_core__PosixTimeDuration_O, sizeof(core::PosixTimeDuration_O), 0, "core::PosixTimeDuration_O" },
//badgen{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::PosixTimeDuration_O),_Duration.ticks_.value_), "_Duration.ticks_.value_" },
{ class_kind, KIND_LISPALLOC_core__SymbolToEnumConverter_O, sizeof(core::SymbolToEnumConverter_O), 0, "core::SymbolToEnumConverter_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_WhatTheEnumsRepresent._Contents), "_WhatTheEnumsRepresent._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToSymbol), "_EnumToSymbol" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_ArchiveSymbolToEnum), "_ArchiveSymbolToEnum" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToArchiveSymbol), "_EnumToArchiveSymbol" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_SymbolToEnum), "_SymbolToEnum" },
{ class_kind, KIND_LISPALLOC_core__CandoException_O, sizeof(core::CandoException_O), 0, "core::CandoException_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::CandoException_O),_message._Contents), "_message._Contents" },
{ class_kind, KIND_LISPALLOC_core__Stream_O, sizeof(core::Stream_O), 0, "core::Stream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Stream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{ class_kind, KIND_LISPALLOC_core__AnsiStream_O, sizeof(core::AnsiStream_O), 0, "core::AnsiStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::AnsiStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{ class_kind, KIND_LISPALLOC_core__FileStream_O, sizeof(core::FileStream_O), 0, "core::FileStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_Filename), "_Filename" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FileStream_O),_ElementType), "_ElementType" },
{ class_kind, KIND_LISPALLOC_core__IOStreamStream_O, sizeof(core::IOStreamStream_O), 0, "core::IOStreamStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_Filename), "_Filename" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOStreamStream_O),_ElementType), "_ElementType" },
{ class_kind, KIND_LISPALLOC_core__IOFileStream_O, sizeof(core::IOFileStream_O), 0, "core::IOFileStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_Filename), "_Filename" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_ElementType), "_ElementType" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::IOFileStream_O),_FileDescriptor), "_FileDescriptor" },
{ class_kind, KIND_LISPALLOC_core__ConcatenatedStream_O, sizeof(core::ConcatenatedStream_O), 0, "core::ConcatenatedStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ConcatenatedStream_O),_List), "_List" },
{ class_kind, KIND_LISPALLOC_core__StringStream_O, sizeof(core::StringStream_O), 0, "core::StringStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{ class_kind, KIND_LISPALLOC_core__StringInputStream_O, sizeof(core::StringInputStream_O), 0, "core::StringInputStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Str_O>), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_Contents), "_Contents" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_InputPosition), "_InputPosition" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::StringInputStream_O),_InputLimit), "_InputLimit" },
{ class_kind, KIND_LISPALLOC_core__StringOutputStream_O, sizeof(core::StringOutputStream_O), 0, "core::StringOutputStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::StrWithFillPtr_O>), offsetof(SAFE_TYPE_MACRO(core::StringOutputStream_O),_Contents), "_Contents" },
{ class_kind, KIND_LISPALLOC_core__SynonymStream_O, sizeof(core::SynonymStream_O), 0, "core::SynonymStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SynonymStream_O),_SynonymSymbol), "_SynonymSymbol" },
{ class_kind, KIND_LISPALLOC_core__EchoStream_O, sizeof(core::EchoStream_O), 0, "core::EchoStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_In), "_In" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::EchoStream_O),_Out), "_Out" },
{ class_kind, KIND_LISPALLOC_core__TwoWayStream_O, sizeof(core::TwoWayStream_O), 0, "core::TwoWayStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_In), "_In" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TwoWayStream_O),_Out), "_Out" },
{ class_kind, KIND_LISPALLOC_core__BroadcastStream_O, sizeof(core::BroadcastStream_O), 0, "core::BroadcastStream_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_Closed), "_Closed" },
{  fixed_field, ctype_enum_core__StreamMode, sizeof(enum core::StreamMode), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_Mode), "_Mode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_Format), "_Format" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_ByteSize), "_ByteSize" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_Flags), "_Flags" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_ByteStack), "_ByteStack" },
//badgen{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_LastCode), "_LastCode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_EofChar), "_EofChar" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_LastOp), "_LastOp" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_LastChar), "_LastChar" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_ExternalFormat), "_ExternalFormat" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_OutputColumn), "_OutputColumn" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_InputCursor._LineNumber), "_InputCursor._LineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_InputCursor._Column), "_InputCursor._Column" },
{  fixed_field, ctype_long_long, sizeof(long long), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_InputCursor._PrevLineNumber), "_InputCursor._PrevLineNumber" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_InputCursor._PrevColumn), "_InputCursor._PrevColumn" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BroadcastStream_O),_Streams), "_Streams" },
{ class_kind, KIND_LISPALLOC_core__Reader_O, sizeof(core::Reader_O), 0, "core::Reader_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Reader_O),_Input), "_Input" },
{ class_kind, KIND_LISPALLOC_core__SharpEqualWrapper_O, sizeof(core::SharpEqualWrapper_O), 0, "core::SharpEqualWrapper_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SharpEqualWrapper_O),_Value), "_Value" },
{ class_kind, KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O, sizeof(asttooling::RegMap::RegistryMaps_O), 0, "asttooling::RegMap::RegistryMaps_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>>), offsetof(SAFE_TYPE_MACRO(asttooling::RegMap::RegistryMaps_O),Constructors._Vector._Contents), "Constructors._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__Archive_O, sizeof(core::Archive_O), 0, "core::Archive_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Archive_O),_Version), "_Version" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::Archive_O),_TopNode.theObject), "_TopNode.theObject" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Archive_O),_NextUniqueId), "_NextUniqueId" },
{ class_kind, KIND_LISPALLOC_core__SaveArchive_O, sizeof(core::SaveArchive_O), 0, "core::SaveArchive_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_Version), "_Version" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_TopNode.theObject), "_TopNode.theObject" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_NextUniqueId), "_NextUniqueId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_SNodeForObject), "_SNodeForObject" },
{ class_kind, KIND_LISPALLOC_core__SexpSaveArchive_O, sizeof(core::SexpSaveArchive_O), 0, "core::SexpSaveArchive_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SexpSaveArchive_O),_Version), "_Version" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::SexpSaveArchive_O),_TopNode.theObject), "_TopNode.theObject" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SexpSaveArchive_O),_NextUniqueId), "_NextUniqueId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SexpSaveArchive_O),_SNodeForObject), "_SNodeForObject" },
{ class_kind, KIND_LISPALLOC_core__LoadArchive_O, sizeof(core::LoadArchive_O), 0, "core::LoadArchive_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_Version), "_Version" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_TopNode.theObject), "_TopNode.theObject" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_NextUniqueId), "_NextUniqueId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_ObjectForSNode), "_ObjectForSNode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_NodesToFinalize), "_NodesToFinalize" },
{ class_kind, KIND_LISPALLOC_core__SexpLoadArchive_O, sizeof(core::SexpLoadArchive_O), 0, "core::SexpLoadArchive_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SexpLoadArchive_O),_Version), "_Version" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::SexpLoadArchive_O),_TopNode.theObject), "_TopNode.theObject" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SexpLoadArchive_O),_NextUniqueId), "_NextUniqueId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SexpLoadArchive_O),_ObjectForSNode), "_ObjectForSNode" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SexpLoadArchive_O),_NodesToFinalize), "_NodesToFinalize" },
{ class_kind, KIND_LISPALLOC_core__HashTable_O, sizeof(core::HashTable_O), 0, "core::HashTable_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_InitialSize), "_InitialSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_RehashSize), "_RehashSize" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_RehashThreshold), "_RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_HashTable), "_HashTable" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_HashTableCount), "_HashTableCount" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" },
{ class_kind, KIND_LISPALLOC_core__HashTableEq_O, sizeof(core::HashTableEq_O), 0, "core::HashTableEq_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_InitialSize), "_InitialSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_RehashSize), "_RehashSize" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_RehashThreshold), "_RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_HashTable), "_HashTable" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_HashTableCount), "_HashTableCount" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" },
{ class_kind, KIND_LISPALLOC_core__HashTableEqualp_O, sizeof(core::HashTableEqualp_O), 0, "core::HashTableEqualp_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_InitialSize), "_InitialSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_RehashSize), "_RehashSize" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_RehashThreshold), "_RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_HashTable), "_HashTable" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_HashTableCount), "_HashTableCount" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" },
{ class_kind, KIND_LISPALLOC_core__HashTableEql_O, sizeof(core::HashTableEql_O), 0, "core::HashTableEql_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_InitialSize), "_InitialSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_RehashSize), "_RehashSize" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_RehashThreshold), "_RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_HashTable), "_HashTable" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_HashTableCount), "_HashTableCount" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" },
{ class_kind, KIND_LISPALLOC_core__HashTableEqual_O, sizeof(core::HashTableEqual_O), 0, "core::HashTableEqual_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_InitialSize), "_InitialSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_RehashSize), "_RehashSize" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_RehashThreshold), "_RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_HashTable), "_HashTable" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_HashTableCount), "_HashTableCount" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" },
{ templated_kind, KIND_TEMPLATED_LISPALLOC_core__Creator_O, sizeof(core::Creator_O), 0, "core::Creator_O" },
{ templated_class_jump_table_index, 3, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_clbind__DummyCreator_O, sizeof(clbind::DummyCreator_O), 0, "clbind::DummyCreator_O" },
{ templated_kind, KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O, sizeof(clbind::ConstructorCreator_O), 0, "clbind::ConstructorCreator_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(clbind::ConstructorCreator_O),_mostDerivedClassSymbol), "_mostDerivedClassSymbol" },
{ templated_class_jump_table_index, 4, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_core__InstanceCreator_O, sizeof(core::InstanceCreator_O), 0, "core::InstanceCreator_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceCreator_O),_className), "_className" },
{ class_kind, KIND_LISPALLOC_cffi__Pointer_O, sizeof(cffi::Pointer_O), 0, "cffi::Pointer_O" },
{ class_kind, KIND_LISPALLOC_core__CxxObject_O, sizeof(core::CxxObject_O), 0, "core::CxxObject_O" },
{ class_kind, KIND_LISPALLOC_core__WeakKeyMapping_O, sizeof(core::WeakKeyMapping_O), 0, "core::WeakKeyMapping_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Mapping<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::WeakLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyMapping_O),_WeakObject.Key), "_WeakObject.Key" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Mapping<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::StrongLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyMapping_O),_WeakObject.Value), "_WeakObject.Value" },
{ class_kind, KIND_LISPALLOC_core__Cache_O, sizeof(core::Cache_O), 0, "core::Cache_O" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Cache_O),_searches), "_searches" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Cache_O),_misses), "_misses" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Cache_O),_total_depth), "_total_depth" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Cache_O),_keys._Vector._Contents), "_keys._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::CacheRecord>>), offsetof(SAFE_TYPE_MACRO(core::Cache_O),_table._Vector._Contents), "_table._Vector._Contents" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Cache_O),_generation), "_generation" },
{ class_kind, KIND_LISPALLOC_core__LambdaListHandler_O, sizeof(core::LambdaListHandler_O), 0, "core::LambdaListHandler_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_ClassifiedSymbolList), "_ClassifiedSymbolList" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_SpecialSymbolSet.theObject), "_SpecialSymbolSet.theObject" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_DeclareSpecifierList), "_DeclareSpecifierList" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::RequiredArgument>>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_RequiredArguments._Vector._Contents), "_RequiredArguments._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::OptionalArgument>>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_OptionalArguments._Vector._Contents), "_OptionalArguments._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_RestArgument._ArgTarget), "_RestArgument._ArgTarget" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_RestArgument._ArgTargetFrameIndex), "_RestArgument._ArgTargetFrameIndex" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_KeyFlag), "_KeyFlag" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::KeywordArgument>>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_KeywordArguments._Vector._Contents), "_KeywordArguments._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_AllowOtherKeys), "_AllowOtherKeys" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::AuxArgument>>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_AuxArguments._Vector._Contents), "_AuxArguments._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_Comment._Contents), "_Comment._Contents" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_NumberOfLexicalVariables), "_NumberOfLexicalVariables" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_LexicalVariableNamesForDebugging), "_LexicalVariableNamesForDebugging" },
{ class_kind, KIND_LISPALLOC_llvmo__InsertPoint_O, sizeof(llvmo::InsertPoint_O), 0, "llvmo::InsertPoint_O" },
{ class_kind, KIND_LISPALLOC_core__SourceFileInfo_O, sizeof(core::SourceFileInfo_O), 0, "core::SourceFileInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Pathname_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_pathname), "_pathname" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_FileHandle), "_FileHandle" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugNamestring), "_SourceDebugNamestring" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugOffset), "_SourceDebugOffset" },
{ class_kind, KIND_LISPALLOC_core__SNode_O, sizeof(core::SNode_O), 0, "core::SNode_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SNode_O),_RefCount), "_RefCount" },
{ class_kind, KIND_LISPALLOC_core__LeafSNode_O, sizeof(core::LeafSNode_O), 0, "core::LeafSNode_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LeafSNode_O),_RefCount), "_RefCount" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LeafSNode_O),_Object), "_Object" },
{ class_kind, KIND_LISPALLOC_core__BranchSNode_O, sizeof(core::BranchSNode_O), 0, "core::BranchSNode_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_RefCount), "_RefCount" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_Kind), "_Kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_SNodePList), "_SNodePList" },
{  fixed_field, POINTER_OFFSET, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_VectorSNodes.theObject), "_VectorSNodes.theObject" },
{ class_kind, KIND_LISPALLOC_core__Path_O, sizeof(core::Path_O), 0, "core::Path_O" },
{ class_kind, KIND_LISPALLOC_asttooling__AstVisitor_O, sizeof(asttooling::AstVisitor_O), 0, "asttooling::AstVisitor_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::AstVisitor_O),_Target), "_Target" },
{ class_kind, KIND_LISPALLOC_llvmo__AttributeSet_O, sizeof(llvmo::AttributeSet_O), 0, "llvmo::AttributeSet_O" },
{ class_kind, KIND_LISPALLOC_core__StructureObject_O, sizeof(core::StructureObject_O), 0, "core::StructureObject_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StructureObject_O),_Type), "_Type" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::StructureObject_O),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O, sizeof(core::InvocationHistoryFrameIterator_O), 0, "core::InvocationHistoryFrameIterator_O" },
{ class_kind, KIND_LISPALLOC_core__Package_O, sizeof(core::Package_O), 0, "core::Package_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_Name._Contents), "_Name._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_InternalSymbols), "_InternalSymbols" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_ExternalSymbols), "_ExternalSymbols" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_Shadowing), "_Shadowing" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_UsingPackages._Vector._Contents), "_UsingPackages._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_PackagesUsedBy._Vector._Contents), "_PackagesUsedBy._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_Nicknames), "_Nicknames" },
{ class_kind, KIND_LISPALLOC_core__DirectoryEntry_O, sizeof(core::DirectoryEntry_O), 0, "core::DirectoryEntry_O" },
{ class_kind, KIND_LISPALLOC_core__Character_dummy_O, sizeof(core::Character_dummy_O), 0, "core::Character_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__Function_O, sizeof(core::Function_O), 0, "core::Function_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::Function_O),closure), "closure" },
{ class_kind, KIND_LISPALLOC_core__CompiledFunction_O, sizeof(core::CompiledFunction_O), 0, "core::CompiledFunction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledFunction_O),closure), "closure" },
{ class_kind, KIND_LISPALLOC_core__SingleDispatchGenericFunction_O, sizeof(core::SingleDispatchGenericFunction_O), 0, "core::SingleDispatchGenericFunction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchGenericFunction_O),closure), "closure" },
{ class_kind, KIND_LISPALLOC_core__SpecialForm_O, sizeof(core::SpecialForm_O), 0, "core::SpecialForm_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::SpecialForm_O),closure), "closure" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SpecialForm_O),_SpecialSymbol), "_SpecialSymbol" },
{ class_kind, KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O, sizeof(core::SingleDispatchEffectiveMethodFunction_O), 0, "core::SingleDispatchEffectiveMethodFunction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchEffectiveMethodFunction_O),closure), "closure" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::SingleDispatchEffectiveMethodFunction_O),_Methods), "_Methods" },
{ class_kind, KIND_LISPALLOC_core__Instance_O, sizeof(core::Instance_O), 0, "core::Instance_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Sig), "_Sig" },
{ class_kind, KIND_LISPALLOC_core__Pointer_O, sizeof(core::Pointer_O), 0, "core::Pointer_O" },
{ class_kind, KIND_LISPALLOC_clbind__ClassRegistry_O, sizeof(clbind::ClassRegistry_O), 0, "clbind::ClassRegistry_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRegistry_O),m_classes), "m_classes" },
{ class_kind, KIND_LISPALLOC_llvmo__DebugInfo_O, sizeof(llvmo::DebugInfo_O), 0, "llvmo::DebugInfo_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIDerivedType_O, sizeof(llvmo::DIDerivedType_O), 0, "llvmo::DIDerivedType_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIArray_O, sizeof(llvmo::DIArray_O), 0, "llvmo::DIArray_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIBasicType_O, sizeof(llvmo::DIBasicType_O), 0, "llvmo::DIBasicType_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DISubprogram_O, sizeof(llvmo::DISubprogram_O), 0, "llvmo::DISubprogram_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DILexicalBlock_O, sizeof(llvmo::DILexicalBlock_O), 0, "llvmo::DILexicalBlock_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DICompileUnit_O, sizeof(llvmo::DICompileUnit_O), 0, "llvmo::DICompileUnit_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIDescriptor_O, sizeof(llvmo::DIDescriptor_O), 0, "llvmo::DIDescriptor_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIType_O, sizeof(llvmo::DIType_O), 0, "llvmo::DIType_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DISubroutineType_O, sizeof(llvmo::DISubroutineType_O), 0, "llvmo::DISubroutineType_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DICompositeType_O, sizeof(llvmo::DICompositeType_O), 0, "llvmo::DICompositeType_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DITypeArray_O, sizeof(llvmo::DITypeArray_O), 0, "llvmo::DITypeArray_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIFile_O, sizeof(llvmo::DIFile_O), 0, "llvmo::DIFile_O" },
{ class_kind, KIND_LISPALLOC_llvmo__DIScope_O, sizeof(llvmo::DIScope_O), 0, "llvmo::DIScope_O" },
{ class_kind, KIND_LISPALLOC_core__SmallMultimap_O, sizeof(core::SmallMultimap_O), 0, "core::SmallMultimap_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>>), offsetof(SAFE_TYPE_MACRO(core::SmallMultimap_O),map._Contents), "map._Contents" },
{ class_kind, KIND_LISPALLOC_core__Pathname_O, sizeof(core::Pathname_O), 0, "core::Pathname_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Host), "_Host" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Device), "_Device" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Directory), "_Directory" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Type), "_Type" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Version), "_Version" },
{ class_kind, KIND_LISPALLOC_core__LogicalPathname_O, sizeof(core::LogicalPathname_O), 0, "core::LogicalPathname_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Host), "_Host" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Device), "_Device" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Directory), "_Directory" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Type), "_Type" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Version), "_Version" },
{ class_kind, KIND_LISPALLOC_core__PosixTime_O, sizeof(core::PosixTime_O), 0, "core::PosixTime_O" },
//badgen{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::PosixTime_O),_Time.time_.time_count_.value_), "_Time.time_.time_count_.value_" },
{ class_kind, KIND_LISPALLOC_core__SmallMap_O, sizeof(core::SmallMap_O), 0, "core::SmallMap_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>>), offsetof(SAFE_TYPE_MACRO(core::SmallMap_O),map._Contents), "map._Contents" },
{ class_kind, KIND_ROOTCLASSALLOC_core__Lisp_O, sizeof(core::Lisp_O), 0, "core::Lisp_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._BufferStringPool), "_Roots._BufferStringPool" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::ExceptionEntry>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._ExceptionStack._Stack._Vector._Contents), "_Roots._ExceptionStack._Stack._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._TerminalIO), "_Roots._TerminalIO" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::StringOutputStream_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._BformatStringOutputStream), "_Roots._BformatStringOutputStream" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Bignum_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._BignumRegister0), "_Roots._BignumRegister0" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Bignum_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._BignumRegister1), "_Roots._BignumRegister1" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Bignum_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._BignumRegister2), "_Roots._BignumRegister2" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._IntegerOverflowAdjust), "_Roots._IntegerOverflowAdjust" },
//badgen{  fixed_field, ctype_enum_std___Rb_tree_color, sizeof(enum std::_Rb_tree_color), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots.charInfo.gNamesToCharacterIndex._M_t._M_impl._M_header._M_color), "_Roots.charInfo.gNamesToCharacterIndex._M_t._M_impl._M_header._M_color" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots.charInfo.gNamesToCharacterIndex._M_t._M_impl._M_node_count), "_Roots.charInfo.gNamesToCharacterIndex._M_t._M_impl._M_node_count" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots.charInfo.gIndexedCharacters._Vector._Contents), "_Roots.charInfo.gIndexedCharacters._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots.charInfo.gCharacterNames._Vector._Contents), "_Roots.charInfo.gCharacterNames._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._ClassSymbolsHolder._Vector._Contents), "_Roots._ClassSymbolsHolder._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SystemProperties), "_Roots._SystemProperties" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::DynamicBinding>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._Bindings._Bindings._Vector._Contents), "_Roots._Bindings._Bindings._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SourceFiles._Vector._Contents), "_Roots._SourceFiles._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._CatchInfo), "_Roots._CatchInfo" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<core::SymbolClassPair>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots.bootClassTable._Vector._Contents), "_Roots.bootClassTable._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._LoadTimeValueArrays), "_Roots._LoadTimeValueArrays" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._CommandLineArguments), "_Roots._CommandLineArguments" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._Packages._Vector._Contents), "_Roots._Packages._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SetfDefinitions), "_Roots._SetfDefinitions" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._CorePackage), "_Roots._CorePackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._KeywordPackage), "_Roots._KeywordPackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._CommonLispPackage), "_Roots._CommonLispPackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SpecialForms), "_Roots._SpecialForms" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SingleDispatchGenericFunctionTable), "_Roots._SingleDispatchGenericFunctionTable" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._TrueObject), "_Roots._TrueObject" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cache_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SingleDispatchMethodCachePtr), "_Roots._SingleDispatchMethodCachePtr" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cache_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._MethodCachePtr), "_Roots._MethodCachePtr" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cache_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SlotCachePtr), "_Roots._SlotCachePtr" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::DoubleFloat_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._RehashSize), "_Roots._RehashSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::DoubleFloat_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._RehashThreshold), "_Roots._RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._NullStream), "_Roots._NullStream" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._PathnameTranslations), "_Roots._PathnameTranslations" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Complex_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._ImaginaryUnit), "_Roots._ImaginaryUnit" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Complex_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._ImaginaryUnitNegative), "_Roots._ImaginaryUnitNegative" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Ratio_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._PlusHalf), "_Roots._PlusHalf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Ratio_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._MinusHalf), "_Roots._MinusHalf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SingleFloat_I>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SingleFloatMinusZero), "_Roots._SingleFloatMinusZero" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SingleFloat_I>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SingleFloatPlusZero), "_Roots._SingleFloatPlusZero" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::DoubleFloat_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._DoubleFloatMinusZero), "_Roots._DoubleFloatMinusZero" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::DoubleFloat_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._DoubleFloatPlusZero), "_Roots._DoubleFloatPlusZero" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SingleFloat_I>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._SingleFloatOne), "_Roots._SingleFloatOne" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::DoubleFloat_O>), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Roots._DoubleFloatOne), "_Roots._DoubleFloatOne" },
//badgen{  fixed_field, ctype_enum_std___Rb_tree_color, sizeof(enum std::_Rb_tree_color), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_OpenDynamicLibraryHandles._M_t._M_impl._M_header._M_color), "_OpenDynamicLibraryHandles._M_t._M_impl._M_header._M_color" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_OpenDynamicLibraryHandles._M_t._M_impl._M_node_count), "_OpenDynamicLibraryHandles._M_t._M_impl._M_node_count" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_StackWarnSize), "_StackWarnSize" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_StackSampleCount), "_StackSampleCount" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_StackSampleSize), "_StackSampleSize" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_StackSampleMax), "_StackSampleMax" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Argc), "_Argc" },
//badgen{  fixed_field, ctype_enum_std___Rb_tree_color, sizeof(enum std::_Rb_tree_color), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_SourceFileIndices._M_t._M_impl._M_header._M_color), "_SourceFileIndices._M_t._M_impl._M_header._M_color" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_SourceFileIndices._M_t._M_impl._M_node_count), "_SourceFileIndices._M_t._M_impl._M_node_count" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_Mode), "_Mode" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_ReplCounter), "_ReplCounter" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_IntegerOrdering._mpz_import_word_order), "_IntegerOrdering._mpz_import_word_order" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_IntegerOrdering._mpz_import_size), "_IntegerOrdering._mpz_import_size" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_IntegerOrdering._mpz_import_endian), "_IntegerOrdering._mpz_import_endian" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_SingleStepLevel), "_SingleStepLevel" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_TraceLevel), "_TraceLevel" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_DebuggerLevel), "_DebuggerLevel" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_MpiRank), "_MpiRank" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_MpiSize), "_MpiSize" },
//badgen{  fixed_field, ctype_enum_std___Rb_tree_color, sizeof(enum std::_Rb_tree_color), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_PackageNameIndexMap._M_t._M_impl._M_header._M_color), "_PackageNameIndexMap._M_t._M_impl._M_header._M_color" },
//badgen{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_PackageNameIndexMap._M_t._M_impl._M_node_count), "_PackageNameIndexMap._M_t._M_impl._M_node_count" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_EnvironmentId), "_EnvironmentId" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_RequireLevel), "_RequireLevel" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_PathMax), "_PathMax" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Lisp_O),_LineNumber), "_LineNumber" },
{ class_kind, KIND_LISPALLOC_core__CoreExposer_O, sizeof(core::CoreExposer_O), 0, "core::CoreExposer_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(core::CoreExposer_O),_Package), "_Package" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_, sizeof(gctools::GCVector_moveable<core::KeywordArgument>), 0, "gctools::GCVector_moveable<core::KeywordArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::KeywordArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::KeywordArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::KeywordArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::KeywordArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Default), "_Default" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Keyword), "_Keyword" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Sensor._ArgTarget), "_Sensor._ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Sensor._ArgTargetFrameIndex), "_Sensor._ArgTargetFrameIndex" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_, sizeof(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>), 0, "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), 0, "only" },
{ container_kind, KIND_GCSTRING_gctools__GCString_moveable_char_, sizeof(gctools::GCString_moveable<char>), 0, "gctools::GCString_moveable<char>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Data), "_Data" },
{  variable_capacity, sizeof(char), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Capacity), NULL },
{    variable_field, ctype_char, sizeof(char), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_, sizeof(gctools::GCVector_moveable<core::RequiredArgument>), 0, "gctools::GCVector_moveable<core::RequiredArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::RequiredArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::RequiredArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::RequiredArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::RequiredArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RequiredArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::RequiredArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_, sizeof(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>), 0, "gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::RegMap::SymbolMatcherDescriptorPair_O), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(asttooling::RegMap::SymbolMatcherDescriptorPair_O),Name), "Name" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>), offsetof(SAFE_TYPE_MACRO(asttooling::RegMap::SymbolMatcherDescriptorPair_O),matcher), "matcher" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O, sizeof(asttooling::internal::VariadicOperatorMatcherDescriptor_O), 0, "asttooling::internal::VariadicOperatorMatcherDescriptor_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(asttooling::internal::VariadicOperatorMatcherDescriptor_O),MinCount), "MinCount" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(asttooling::internal::VariadicOperatorMatcherDescriptor_O),MaxCount), "MaxCount" },
//badgen{  fixed_field, ctype_enum_clang__ast_matchers__internal__DynTypedMatcher__VariadicOperator, sizeof(enum clang::ast_matchers::internal::DynTypedMatcher::VariadicOperator), offsetof(SAFE_TYPE_MACRO(asttooling::internal::VariadicOperatorMatcherDescriptor_O),Op), "Op" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(asttooling::internal::VariadicOperatorMatcherDescriptor_O),MatcherName), "MatcherName" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___, sizeof(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>), 0, "gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>),_Data), "_Data" },
{  variable_capacity, sizeof(std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>),first), "first" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>),second), "second" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_, sizeof(gctools::GCVector_moveable<core::SymbolStorage>), 0, "gctools::GCVector_moveable<core::SymbolStorage>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolStorage>),_Data), "_Data" },
{  variable_capacity, sizeof(core::SymbolStorage), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolStorage>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolStorage>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolStorage),_Symbol), "_Symbol" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_, sizeof(gctools::GCVector_moveable<asttooling::ContextFrame>), 0, "gctools::GCVector_moveable<asttooling::ContextFrame>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ContextFrame>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::ContextFrame), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ContextFrame>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ContextFrame>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(asttooling::ContextFrame),Range), "Range" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_, sizeof(gctools::GCVector_moveable<core::T_O *>), 0, "gctools::GCVector_moveable<core::T_O *>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::T_O *>),_Data), "_Data" },
{  variable_capacity, sizeof(core::T_O*), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::T_O *>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::T_O *>),_Capacity), NULL },
{    variable_field, POINTER_OFFSET, sizeof(core::T_O*), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_, sizeof(gctools::GCVector_moveable<core::AuxArgument>), 0, "gctools::GCVector_moveable<core::AuxArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::AuxArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::AuxArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::AuxArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::AuxArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::AuxArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AuxArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::AuxArgument),_Expression), "_Expression" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_, sizeof(gctools::GCVector_moveable<asttooling::ParserValue>), 0, "gctools::GCVector_moveable<asttooling::ParserValue>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ParserValue>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::ParserValue), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ParserValue>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ParserValue>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(asttooling::ParserValue),Range), "Range" },
//badgen{    variable_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(asttooling::ParserValue),Value.Value.Unsigned), "Value.Value.Unsigned" },
{ class_kind, KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O, sizeof(asttooling::internal::FreeFuncMatcherDescriptor_O), 0, "asttooling::internal::FreeFuncMatcherDescriptor_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(asttooling::internal::FreeFuncMatcherDescriptor_O),MatcherName), "MatcherName" },
{ class_kind, KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O, sizeof(asttooling::internal::FixedArgCountMatcherDescriptor_O), 0, "asttooling::internal::FixedArgCountMatcherDescriptor_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(asttooling::internal::FixedArgCountMatcherDescriptor_O),MatcherName), "MatcherName" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_, sizeof(gctools::GCVector_moveable<core::SymbolClassPair>), 0, "gctools::GCVector_moveable<core::SymbolClassPair>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Data), "_Data" },
{  variable_capacity, sizeof(core::SymbolClassPair), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),symbol), "symbol" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),theClass), "theClass" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___, sizeof(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>), 0, "gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>),_Data), "_Data" },
{  variable_capacity, sizeof(std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>),first), "first" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>),second), "second" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_, sizeof(gctools::GCVector_moveable<core::CacheRecord>), 0, "gctools::GCVector_moveable<core::CacheRecord>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Data), "_Data" },
{  variable_capacity, sizeof(core::CacheRecord), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_key), "_key" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_value), "_value" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_generation), "_generation" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory, sizeof(asttooling::DerivableFrontendActionFactory), 0, "asttooling::DerivableFrontendActionFactory" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_Sig), "_Sig" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_, sizeof(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>), 0, "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableMatchCallback, sizeof(asttooling::DerivableMatchCallback), 0, "asttooling::DerivableMatchCallback" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_Sig), "_Sig" },
{ class_kind, KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O, sizeof(asttooling::internal::OverloadedMatcherDescriptor_O), 0, "asttooling::internal::OverloadedMatcherDescriptor_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::internal::OverloadedMatcherDescriptor_O),Overloads._Vector._Contents), "Overloads._Vector._Contents" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_, sizeof(gctools::GCVector_moveable<asttooling::ErrorContent>), 0, "gctools::GCVector_moveable<asttooling::ErrorContent>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ErrorContent>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::ErrorContent), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ErrorContent>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ErrorContent>),_Capacity), NULL },
{    variable_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<asttooling::ContextFrame>>), offsetof(SAFE_TYPE_MACRO(asttooling::ErrorContent),ContextStack._Vector._Contents), "ContextStack._Vector._Contents" },
{    variable_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<asttooling::Message>>), offsetof(SAFE_TYPE_MACRO(asttooling::ErrorContent),Messages._Vector._Contents), "Messages._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableASTFrontendAction, sizeof(asttooling::DerivableASTFrontendAction), 0, "asttooling::DerivableASTFrontendAction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_Sig), "_Sig" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_, sizeof(gctools::GCVector_moveable<asttooling::Message>), 0, "gctools::GCVector_moveable<asttooling::Message>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::Message>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::Message), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::Message>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::Message>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(asttooling::Message),Range), "Range" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_, sizeof(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>), 0, "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction, sizeof(asttooling::DerivableSyntaxOnlyAction), 0, "asttooling::DerivableSyntaxOnlyAction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_Sig), "_Sig" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SingleDispatchMethod_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SingleDispatchMethod_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_, sizeof(gctools::GCVector_moveable<core::OptionalArgument>), 0, "gctools::GCVector_moveable<core::OptionalArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::OptionalArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::OptionalArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::OptionalArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::OptionalArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_Default), "_Default" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_Sensor._ArgTarget), "_Sensor._ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_Sensor._ArgTargetFrameIndex), "_Sensor._ArgTargetFrameIndex" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SequenceStepper_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SequenceStepper_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_, sizeof(gctools::GCVector_moveable<core::ExceptionEntry>), 0, "gctools::GCVector_moveable<core::ExceptionEntry>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Data), "_Data" },
{  variable_capacity, sizeof(core::ExceptionEntry), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Capacity), NULL },
{    variable_field, ctype_core__FrameKind, sizeof(core::FrameKind), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_FrameKind), "_FrameKind" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_Key), "_Key" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_, sizeof(gctools::GCVector_moveable<core::DynamicBinding>), 0, "gctools::GCVector_moveable<core::DynamicBinding>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Data), "_Data" },
{  variable_capacity, sizeof(core::DynamicBinding), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Var), "_Var" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Val), "_Val" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<clbind::ClassRep_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<clbind::ClassRep_O>), 0, "only" },

#endif // defined(GC_OBJ_SCAN_HELPERS)
#if defined(GC_OBJ_SCAN_TABLE)
static void* OBJ_SCAN_table[] = { 
//badgen  /* 62 */ &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
//badgen  /* 174 */ &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O,
//badgen  /* 183 */ &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__Iterator_O,
//badgen  /* 216 */ &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__Creator_O,
//badgen  /* 218 */ &&obj_scan_KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O,
   NULL
};
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
obj_finalize_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
    clbind::detail::class_map* obj_gc_safe = reinterpret_cast<clbind::detail::class_map*>(client);
    obj_gc_safe->~class_map();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__T_O:
{
    core::T_O* obj_gc_safe = reinterpret_cast<core::T_O*>(client);
    obj_gc_safe->~T_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VaList_dummy_O:
{
    core::VaList_dummy_O* obj_gc_safe = reinterpret_cast<core::VaList_dummy_O*>(client);
    obj_gc_safe->~VaList_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    obj_gc_safe->~Cons_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    obj_gc_safe->~General_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
    core::MultiStringBuffer_O* obj_gc_safe = reinterpret_cast<core::MultiStringBuffer_O*>(client);
    obj_gc_safe->~MultiStringBuffer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ReadTable_O:
{
    core::ReadTable_O* obj_gc_safe = reinterpret_cast<core::ReadTable_O*>(client);
    obj_gc_safe->~ReadTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    obj_gc_safe->~Number_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    obj_gc_safe->~Complex_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    obj_gc_safe->~Real_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    obj_gc_safe->~Rational_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    obj_gc_safe->~Integer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    obj_gc_safe->~Bignum_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    obj_gc_safe->~Fixnum_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    obj_gc_safe->~Ratio_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    obj_gc_safe->~Float_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    obj_gc_safe->~DoubleFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    obj_gc_safe->~LongFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    obj_gc_safe->~SingleFloat_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    obj_gc_safe->~ShortFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    obj_gc_safe->~FileStatus_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    obj_gc_safe->~WeakHashTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    obj_gc_safe->~WeakKeyHashTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    obj_gc_safe->~Environment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ActivationFrame_O:
{
    core::ActivationFrame_O* obj_gc_safe = reinterpret_cast<core::ActivationFrame_O*>(client);
    obj_gc_safe->~ActivationFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    obj_gc_safe->~TagbodyFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    obj_gc_safe->~ValueFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    obj_gc_safe->~FunctionFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    obj_gc_safe->~LexicalEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    obj_gc_safe->~RuntimeVisibleEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    obj_gc_safe->~FunctionValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    obj_gc_safe->~ValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    obj_gc_safe->~TagbodyEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    obj_gc_safe->~CompileTimeEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    obj_gc_safe->~UnwindProtectEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    obj_gc_safe->~SymbolMacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    obj_gc_safe->~FunctionContainerEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    obj_gc_safe->~StackValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    obj_gc_safe->~BlockEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    obj_gc_safe->~MacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    obj_gc_safe->~CatchEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    obj_gc_safe->~GlueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    obj_gc_safe->~Array_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ArrayObjects_O:
{
    core::ArrayObjects_O* obj_gc_safe = reinterpret_cast<core::ArrayObjects_O*>(client);
    obj_gc_safe->~ArrayObjects_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
    core::ArrayDisplaced_O* obj_gc_safe = reinterpret_cast<core::ArrayDisplaced_O*>(client);
    obj_gc_safe->~ArrayDisplaced_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    obj_gc_safe->~Vector_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BitVector_O:
{
    core::BitVector_O* obj_gc_safe = reinterpret_cast<core::BitVector_O*>(client);
    obj_gc_safe->~BitVector_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SimpleBitVector_O:
{
    core::SimpleBitVector_O* obj_gc_safe = reinterpret_cast<core::SimpleBitVector_O*>(client);
    obj_gc_safe->~SimpleBitVector_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
    core::BitVectorWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::BitVectorWithFillPtr_O*>(client);
    obj_gc_safe->~BitVectorWithFillPtr_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorDisplaced_O:
{
    core::VectorDisplaced_O* obj_gc_safe = reinterpret_cast<core::VectorDisplaced_O*>(client);
    obj_gc_safe->~VectorDisplaced_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    obj_gc_safe->~String_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    obj_gc_safe->~Str_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
    core::StrWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::StrWithFillPtr_O*>(client);
    obj_gc_safe->~StrWithFillPtr_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    obj_gc_safe->~VectorObjects_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    obj_gc_safe->~VectorObjectsWithFillPtr_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
    core::SingleDispatchMethod_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchMethod_O*>(client);
    obj_gc_safe->~SingleDispatchMethod_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RandomState_O:
{
    core::RandomState_O* obj_gc_safe = reinterpret_cast<core::RandomState_O*>(client);
    obj_gc_safe->~RandomState_O();
    return;
}
obj_finalize_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    obj_gc_safe->~WrappedPointer_O();
}
obj_finalize_KIND_LISPALLOC_core__SequenceStepper_O:
{
    core::SequenceStepper_O* obj_gc_safe = reinterpret_cast<core::SequenceStepper_O*>(client);
    obj_gc_safe->~SequenceStepper_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ConsStepper_O:
{
    core::ConsStepper_O* obj_gc_safe = reinterpret_cast<core::ConsStepper_O*>(client);
    obj_gc_safe->~ConsStepper_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorStepper_O:
{
    core::VectorStepper_O* obj_gc_safe = reinterpret_cast<core::VectorStepper_O*>(client);
    obj_gc_safe->~VectorStepper_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
    llvmo::DebugLoc_O* obj_gc_safe = reinterpret_cast<llvmo::DebugLoc_O*>(client);
    obj_gc_safe->~DebugLoc_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Attribute_O:
{
    llvmo::Attribute_O* obj_gc_safe = reinterpret_cast<llvmo::Attribute_O*>(client);
    obj_gc_safe->~Attribute_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RegexMatch_O:
{
    core::RegexMatch_O* obj_gc_safe = reinterpret_cast<core::RegexMatch_O*>(client);
    obj_gc_safe->~RegexMatch_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakPointer_O:
{
    core::WeakPointer_O* obj_gc_safe = reinterpret_cast<core::WeakPointer_O*>(client);
    obj_gc_safe->~WeakPointer_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    obj_gc_safe->~StandardObject_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    obj_gc_safe->~Metaobject_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    obj_gc_safe->~Specializer_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    obj_gc_safe->~Class_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__StdClass_O:
{
    core::StdClass_O* obj_gc_safe = reinterpret_cast<core::StdClass_O*>(client);
    obj_gc_safe->~StdClass_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__StandardClass_O:
{
    core::StandardClass_O* obj_gc_safe = reinterpret_cast<core::StandardClass_O*>(client);
    obj_gc_safe->~StandardClass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
    core::FuncallableStandardClass_O* obj_gc_safe = reinterpret_cast<core::FuncallableStandardClass_O*>(client);
    obj_gc_safe->~FuncallableStandardClass_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__StructureClass_O:
{
    core::StructureClass_O* obj_gc_safe = reinterpret_cast<core::StructureClass_O*>(client);
    obj_gc_safe->~StructureClass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
    core::ForwardReferencedClass_O* obj_gc_safe = reinterpret_cast<core::ForwardReferencedClass_O*>(client);
    obj_gc_safe->~ForwardReferencedClass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CxxClass_O:
{
    core::CxxClass_O* obj_gc_safe = reinterpret_cast<core::CxxClass_O*>(client);
    obj_gc_safe->~CxxClass_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    core::BuiltInClass_O* obj_gc_safe = reinterpret_cast<core::BuiltInClass_O*>(client);
    obj_gc_safe->~BuiltInClass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_clbind__ClassRep_O:
{
    clbind::ClassRep_O* obj_gc_safe = reinterpret_cast<clbind::ClassRep_O*>(client);
    obj_gc_safe->~ClassRep_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ExternalObject_O:
{
    core::ExternalObject_O* obj_gc_safe = reinterpret_cast<core::ExternalObject_O*>(client);
    obj_gc_safe->~ExternalObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Value_O:
{
    llvmo::Value_O* obj_gc_safe = reinterpret_cast<llvmo::Value_O*>(client);
    obj_gc_safe->~Value_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Argument_O:
{
    llvmo::Argument_O* obj_gc_safe = reinterpret_cast<llvmo::Argument_O*>(client);
    obj_gc_safe->~Argument_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__User_O:
{
    llvmo::User_O* obj_gc_safe = reinterpret_cast<llvmo::User_O*>(client);
    obj_gc_safe->~User_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Instruction_O:
{
    llvmo::Instruction_O* obj_gc_safe = reinterpret_cast<llvmo::Instruction_O*>(client);
    obj_gc_safe->~Instruction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    llvmo::AtomicRMWInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicRMWInst_O*>(client);
    obj_gc_safe->~AtomicRMWInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    llvmo::LandingPadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LandingPadInst_O*>(client);
    obj_gc_safe->~LandingPadInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PHINode_O:
{
    llvmo::PHINode_O* obj_gc_safe = reinterpret_cast<llvmo::PHINode_O*>(client);
    obj_gc_safe->~PHINode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__CallInst_O:
{
    llvmo::CallInst_O* obj_gc_safe = reinterpret_cast<llvmo::CallInst_O*>(client);
    obj_gc_safe->~CallInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    llvmo::StoreInst_O* obj_gc_safe = reinterpret_cast<llvmo::StoreInst_O*>(client);
    obj_gc_safe->~StoreInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    llvmo::UnaryInstruction_O* obj_gc_safe = reinterpret_cast<llvmo::UnaryInstruction_O*>(client);
    obj_gc_safe->~UnaryInstruction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    llvmo::LoadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LoadInst_O*>(client);
    obj_gc_safe->~LoadInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    llvmo::AllocaInst_O* obj_gc_safe = reinterpret_cast<llvmo::AllocaInst_O*>(client);
    obj_gc_safe->~AllocaInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    llvmo::VAArgInst_O* obj_gc_safe = reinterpret_cast<llvmo::VAArgInst_O*>(client);
    obj_gc_safe->~VAArgInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    llvmo::AtomicCmpXchgInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicCmpXchgInst_O*>(client);
    obj_gc_safe->~AtomicCmpXchgInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    llvmo::TerminatorInst_O* obj_gc_safe = reinterpret_cast<llvmo::TerminatorInst_O*>(client);
    obj_gc_safe->~TerminatorInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    llvmo::UnreachableInst_O* obj_gc_safe = reinterpret_cast<llvmo::UnreachableInst_O*>(client);
    obj_gc_safe->~UnreachableInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    llvmo::SwitchInst_O* obj_gc_safe = reinterpret_cast<llvmo::SwitchInst_O*>(client);
    obj_gc_safe->~SwitchInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    llvmo::ReturnInst_O* obj_gc_safe = reinterpret_cast<llvmo::ReturnInst_O*>(client);
    obj_gc_safe->~ReturnInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    llvmo::ResumeInst_O* obj_gc_safe = reinterpret_cast<llvmo::ResumeInst_O*>(client);
    obj_gc_safe->~ResumeInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    llvmo::BranchInst_O* obj_gc_safe = reinterpret_cast<llvmo::BranchInst_O*>(client);
    obj_gc_safe->~BranchInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    llvmo::InvokeInst_O* obj_gc_safe = reinterpret_cast<llvmo::InvokeInst_O*>(client);
    obj_gc_safe->~InvokeInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    llvmo::IndirectBrInst_O* obj_gc_safe = reinterpret_cast<llvmo::IndirectBrInst_O*>(client);
    obj_gc_safe->~IndirectBrInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    llvmo::FenceInst_O* obj_gc_safe = reinterpret_cast<llvmo::FenceInst_O*>(client);
    obj_gc_safe->~FenceInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Constant_O:
{
    llvmo::Constant_O* obj_gc_safe = reinterpret_cast<llvmo::Constant_O*>(client);
    obj_gc_safe->~Constant_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    llvmo::BlockAddress_O* obj_gc_safe = reinterpret_cast<llvmo::BlockAddress_O*>(client);
    obj_gc_safe->~BlockAddress_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    llvmo::GlobalValue_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalValue_O*>(client);
    obj_gc_safe->~GlobalValue_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    llvmo::GlobalVariable_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalVariable_O*>(client);
    obj_gc_safe->~GlobalVariable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Function_O:
{
    llvmo::Function_O* obj_gc_safe = reinterpret_cast<llvmo::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    llvmo::ConstantArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantArray_O*>(client);
    obj_gc_safe->~ConstantArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    llvmo::ConstantInt_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantInt_O*>(client);
    obj_gc_safe->~ConstantInt_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    llvmo::ConstantDataSequential_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataSequential_O*>(client);
    obj_gc_safe->~ConstantDataSequential_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    llvmo::ConstantDataArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataArray_O*>(client);
    obj_gc_safe->~ConstantDataArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    llvmo::ConstantStruct_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantStruct_O*>(client);
    obj_gc_safe->~ConstantStruct_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    llvmo::ConstantFP_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantFP_O*>(client);
    obj_gc_safe->~ConstantFP_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    llvmo::UndefValue_O* obj_gc_safe = reinterpret_cast<llvmo::UndefValue_O*>(client);
    obj_gc_safe->~UndefValue_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    llvmo::ConstantPointerNull_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantPointerNull_O*>(client);
    obj_gc_safe->~ConstantPointerNull_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    llvmo::ConstantExpr_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantExpr_O*>(client);
    obj_gc_safe->~ConstantExpr_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    llvmo::BasicBlock_O* obj_gc_safe = reinterpret_cast<llvmo::BasicBlock_O*>(client);
    obj_gc_safe->~BasicBlock_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    llvmo::IRBuilderBase_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilderBase_O*>(client);
    obj_gc_safe->~IRBuilderBase_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    llvmo::IRBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilder_O*>(client);
    obj_gc_safe->~IRBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
    llvmo::DIBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::DIBuilder_O*>(client);
    obj_gc_safe->~DIBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Metadata_O:
{
    llvmo::Metadata_O* obj_gc_safe = reinterpret_cast<llvmo::Metadata_O*>(client);
    obj_gc_safe->~Metadata_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    llvmo::ValueAsMetadata_O* obj_gc_safe = reinterpret_cast<llvmo::ValueAsMetadata_O*>(client);
    obj_gc_safe->~ValueAsMetadata_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__MDNode_O:
{
    llvmo::MDNode_O* obj_gc_safe = reinterpret_cast<llvmo::MDNode_O*>(client);
    obj_gc_safe->~MDNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__MDString_O:
{
    llvmo::MDString_O* obj_gc_safe = reinterpret_cast<llvmo::MDString_O*>(client);
    obj_gc_safe->~MDString_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    llvmo::ExecutionEngine_O* obj_gc_safe = reinterpret_cast<llvmo::ExecutionEngine_O*>(client);
    obj_gc_safe->~ExecutionEngine_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__APFloat_O:
{
    llvmo::APFloat_O* obj_gc_safe = reinterpret_cast<llvmo::APFloat_O*>(client);
    obj_gc_safe->~APFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    llvmo::PassManagerBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBuilder_O*>(client);
    obj_gc_safe->~PassManagerBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    llvmo::DataLayout_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayout_O*>(client);
    obj_gc_safe->~DataLayout_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Triple_O:
{
    llvmo::Triple_O* obj_gc_safe = reinterpret_cast<llvmo::Triple_O*>(client);
    obj_gc_safe->~Triple_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__APInt_O:
{
    llvmo::APInt_O* obj_gc_safe = reinterpret_cast<llvmo::APInt_O*>(client);
    obj_gc_safe->~APInt_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    llvmo::PassManagerBase_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBase_O*>(client);
    obj_gc_safe->~PassManagerBase_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    llvmo::FunctionPassManager_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPassManager_O*>(client);
    obj_gc_safe->~FunctionPassManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PassManager_O:
{
    llvmo::PassManager_O* obj_gc_safe = reinterpret_cast<llvmo::PassManager_O*>(client);
    obj_gc_safe->~PassManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    llvmo::TargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::TargetMachine_O*>(client);
    obj_gc_safe->~TargetMachine_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    llvmo::LLVMTargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMTargetMachine_O*>(client);
    obj_gc_safe->~LLVMTargetMachine_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    llvmo::TargetOptions_O* obj_gc_safe = reinterpret_cast<llvmo::TargetOptions_O*>(client);
    obj_gc_safe->~TargetOptions_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Type_O:
{
    llvmo::Type_O* obj_gc_safe = reinterpret_cast<llvmo::Type_O*>(client);
    obj_gc_safe->~Type_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    llvmo::IntegerType_O* obj_gc_safe = reinterpret_cast<llvmo::IntegerType_O*>(client);
    obj_gc_safe->~IntegerType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    llvmo::CompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::CompositeType_O*>(client);
    obj_gc_safe->~CompositeType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    llvmo::SequentialType_O* obj_gc_safe = reinterpret_cast<llvmo::SequentialType_O*>(client);
    obj_gc_safe->~SequentialType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__VectorType_O:
{
    llvmo::VectorType_O* obj_gc_safe = reinterpret_cast<llvmo::VectorType_O*>(client);
    obj_gc_safe->~VectorType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PointerType_O:
{
    llvmo::PointerType_O* obj_gc_safe = reinterpret_cast<llvmo::PointerType_O*>(client);
    obj_gc_safe->~PointerType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    llvmo::ArrayType_O* obj_gc_safe = reinterpret_cast<llvmo::ArrayType_O*>(client);
    obj_gc_safe->~ArrayType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__StructType_O:
{
    llvmo::StructType_O* obj_gc_safe = reinterpret_cast<llvmo::StructType_O*>(client);
    obj_gc_safe->~StructType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    llvmo::FunctionType_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionType_O*>(client);
    obj_gc_safe->~FunctionType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    llvmo::NamedMDNode_O* obj_gc_safe = reinterpret_cast<llvmo::NamedMDNode_O*>(client);
    obj_gc_safe->~NamedMDNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Linker_O:
{
    llvmo::Linker_O* obj_gc_safe = reinterpret_cast<llvmo::Linker_O*>(client);
    obj_gc_safe->~Linker_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Pass_O:
{
    llvmo::Pass_O* obj_gc_safe = reinterpret_cast<llvmo::Pass_O*>(client);
    obj_gc_safe->~Pass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    llvmo::FunctionPass_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPass_O*>(client);
    obj_gc_safe->~FunctionPass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    llvmo::ModulePass_O* obj_gc_safe = reinterpret_cast<llvmo::ModulePass_O*>(client);
    obj_gc_safe->~ModulePass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    llvmo::ImmutablePass_O* obj_gc_safe = reinterpret_cast<llvmo::ImmutablePass_O*>(client);
    obj_gc_safe->~ImmutablePass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    llvmo::DataLayoutPass_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayoutPass_O*>(client);
    obj_gc_safe->~DataLayoutPass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    llvmo::TargetLibraryInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetLibraryInfo_O*>(client);
    obj_gc_safe->~TargetLibraryInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    llvmo::MCSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::MCSubtargetInfo_O*>(client);
    obj_gc_safe->~MCSubtargetInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    llvmo::TargetSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetSubtargetInfo_O*>(client);
    obj_gc_safe->~TargetSubtargetInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Module_O:
{
    llvmo::Module_O* obj_gc_safe = reinterpret_cast<llvmo::Module_O*>(client);
    obj_gc_safe->~Module_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    llvmo::EngineBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::EngineBuilder_O*>(client);
    obj_gc_safe->~EngineBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ForeignData_O:
{
    core::ForeignData_O* obj_gc_safe = reinterpret_cast<core::ForeignData_O*>(client);
    obj_gc_safe->~ForeignData_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    llvmo::LLVMContext_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMContext_O*>(client);
    obj_gc_safe->~LLVMContext_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Target_O:
{
    llvmo::Target_O* obj_gc_safe = reinterpret_cast<llvmo::Target_O*>(client);
    obj_gc_safe->~Target_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LoadTimeValues_O:
{
    core::LoadTimeValues_O* obj_gc_safe = reinterpret_cast<core::LoadTimeValues_O*>(client);
    obj_gc_safe->~LoadTimeValues_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Binder_O:
{
    core::Binder_O* obj_gc_safe = reinterpret_cast<core::Binder_O*>(client);
    obj_gc_safe->~Binder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__IntArray_O:
{
    core::IntArray_O* obj_gc_safe = reinterpret_cast<core::IntArray_O*>(client);
    obj_gc_safe->~IntArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    obj_gc_safe->~SourceManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    obj_gc_safe->~Record_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Functor_O:
{
    core::Functor_O* obj_gc_safe = reinterpret_cast<core::Functor_O*>(client);
    obj_gc_safe->~Functor_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Closure_O:
{
    core::Closure_O* obj_gc_safe = reinterpret_cast<core::Closure_O*>(client);
    obj_gc_safe->~Closure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionClosure_O:
{
    core::FunctionClosure_O* obj_gc_safe = reinterpret_cast<core::FunctionClosure_O*>(client);
    obj_gc_safe->~FunctionClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__InstanceClosure_O:
{
    core::InstanceClosure_O* obj_gc_safe = reinterpret_cast<core::InstanceClosure_O*>(client);
    obj_gc_safe->~InstanceClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledClosure_O:
{
    core::CompiledClosure_O* obj_gc_safe = reinterpret_cast<core::CompiledClosure_O*>(client);
    obj_gc_safe->~CompiledClosure_O();
    return;
}
obj_finalize_KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O:
{
    core::BuiltinClosure_O* obj_gc_safe = reinterpret_cast<core::BuiltinClosure_O*>(client);
    obj_gc_safe->~BuiltinClosure_O();
}
obj_finalize_KIND_LISPALLOC_core__MacroClosure_O:
{
    core::MacroClosure_O* obj_gc_safe = reinterpret_cast<core::MacroClosure_O*>(client);
    obj_gc_safe->~MacroClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O:
{
    core::SingleDispatchGenericFunctionClosure_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunctionClosure_O*>(client);
    obj_gc_safe->~SingleDispatchGenericFunctionClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__InterpretedClosure_O:
{
    core::InterpretedClosure_O* obj_gc_safe = reinterpret_cast<core::InterpretedClosure_O*>(client);
    obj_gc_safe->~InterpretedClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LightUserData_O:
{
    core::LightUserData_O* obj_gc_safe = reinterpret_cast<core::LightUserData_O*>(client);
    obj_gc_safe->~LightUserData_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__UserData_O:
{
    core::UserData_O* obj_gc_safe = reinterpret_cast<core::UserData_O*>(client);
    obj_gc_safe->~UserData_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    obj_gc_safe->~Symbol_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Null_O:
{
    core::Null_O* obj_gc_safe = reinterpret_cast<core::Null_O*>(client);
    obj_gc_safe->~Null_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    obj_gc_safe->~SourcePosInfo_O();
    return;
}
obj_finalize_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    obj_gc_safe->~Iterator_O();
}
obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    obj_gc_safe->~DirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    obj_gc_safe->~RecursiveDirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Regex_O:
{
    core::Regex_O* obj_gc_safe = reinterpret_cast<core::Regex_O*>(client);
    obj_gc_safe->~Regex_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
    core::PosixTimeDuration_O* obj_gc_safe = reinterpret_cast<core::PosixTimeDuration_O*>(client);
    obj_gc_safe->~PosixTimeDuration_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    obj_gc_safe->~SymbolToEnumConverter_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    obj_gc_safe->~CandoException_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Stream_O:
{
    core::Stream_O* obj_gc_safe = reinterpret_cast<core::Stream_O*>(client);
    obj_gc_safe->~Stream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__AnsiStream_O:
{
    core::AnsiStream_O* obj_gc_safe = reinterpret_cast<core::AnsiStream_O*>(client);
    obj_gc_safe->~AnsiStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FileStream_O:
{
    core::FileStream_O* obj_gc_safe = reinterpret_cast<core::FileStream_O*>(client);
    obj_gc_safe->~FileStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__IOStreamStream_O:
{
    core::IOStreamStream_O* obj_gc_safe = reinterpret_cast<core::IOStreamStream_O*>(client);
    obj_gc_safe->~IOStreamStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__IOFileStream_O:
{
    core::IOFileStream_O* obj_gc_safe = reinterpret_cast<core::IOFileStream_O*>(client);
    obj_gc_safe->~IOFileStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
    core::ConcatenatedStream_O* obj_gc_safe = reinterpret_cast<core::ConcatenatedStream_O*>(client);
    obj_gc_safe->~ConcatenatedStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StringStream_O:
{
    core::StringStream_O* obj_gc_safe = reinterpret_cast<core::StringStream_O*>(client);
    obj_gc_safe->~StringStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StringInputStream_O:
{
    core::StringInputStream_O* obj_gc_safe = reinterpret_cast<core::StringInputStream_O*>(client);
    obj_gc_safe->~StringInputStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StringOutputStream_O:
{
    core::StringOutputStream_O* obj_gc_safe = reinterpret_cast<core::StringOutputStream_O*>(client);
    obj_gc_safe->~StringOutputStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SynonymStream_O:
{
    core::SynonymStream_O* obj_gc_safe = reinterpret_cast<core::SynonymStream_O*>(client);
    obj_gc_safe->~SynonymStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__EchoStream_O:
{
    core::EchoStream_O* obj_gc_safe = reinterpret_cast<core::EchoStream_O*>(client);
    obj_gc_safe->~EchoStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TwoWayStream_O:
{
    core::TwoWayStream_O* obj_gc_safe = reinterpret_cast<core::TwoWayStream_O*>(client);
    obj_gc_safe->~TwoWayStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BroadcastStream_O:
{
    core::BroadcastStream_O* obj_gc_safe = reinterpret_cast<core::BroadcastStream_O*>(client);
    obj_gc_safe->~BroadcastStream_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Reader_O:
{
    core::Reader_O* obj_gc_safe = reinterpret_cast<core::Reader_O*>(client);
    obj_gc_safe->~Reader_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SharpEqualWrapper_O:
{
    core::SharpEqualWrapper_O* obj_gc_safe = reinterpret_cast<core::SharpEqualWrapper_O*>(client);
    obj_gc_safe->~SharpEqualWrapper_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O:
{
    asttooling::RegMap::RegistryMaps_O* obj_gc_safe = reinterpret_cast<asttooling::RegMap::RegistryMaps_O*>(client);
    obj_gc_safe->~RegistryMaps_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Archive_O:
{
    core::Archive_O* obj_gc_safe = reinterpret_cast<core::Archive_O*>(client);
    obj_gc_safe->~Archive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SaveArchive_O:
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    obj_gc_safe->~SaveArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
    core::SexpSaveArchive_O* obj_gc_safe = reinterpret_cast<core::SexpSaveArchive_O*>(client);
    obj_gc_safe->~SexpSaveArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    obj_gc_safe->~LoadArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
    core::SexpLoadArchive_O* obj_gc_safe = reinterpret_cast<core::SexpLoadArchive_O*>(client);
    obj_gc_safe->~SexpLoadArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTable_O:
{
    core::HashTable_O* obj_gc_safe = reinterpret_cast<core::HashTable_O*>(client);
    obj_gc_safe->~HashTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTableEq_O:
{
    core::HashTableEq_O* obj_gc_safe = reinterpret_cast<core::HashTableEq_O*>(client);
    obj_gc_safe->~HashTableEq_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    core::HashTableEqualp_O* obj_gc_safe = reinterpret_cast<core::HashTableEqualp_O*>(client);
    obj_gc_safe->~HashTableEqualp_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTableEql_O:
{
    core::HashTableEql_O* obj_gc_safe = reinterpret_cast<core::HashTableEql_O*>(client);
    obj_gc_safe->~HashTableEql_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTableEqual_O:
{
    core::HashTableEqual_O* obj_gc_safe = reinterpret_cast<core::HashTableEqual_O*>(client);
    obj_gc_safe->~HashTableEqual_O();
    return;
}
obj_finalize_KIND_TEMPLATED_LISPALLOC_core__Creator_O:
{
    core::Creator_O* obj_gc_safe = reinterpret_cast<core::Creator_O*>(client);
    obj_gc_safe->~Creator_O();
}
obj_finalize_KIND_LISPALLOC_clbind__DummyCreator_O:
{
    clbind::DummyCreator_O* obj_gc_safe = reinterpret_cast<clbind::DummyCreator_O*>(client);
    obj_gc_safe->~DummyCreator_O();
    return;
}
obj_finalize_KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O:
{
    clbind::ConstructorCreator_O* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator_O*>(client);
    obj_gc_safe->~ConstructorCreator_O();
}
obj_finalize_KIND_LISPALLOC_core__InstanceCreator_O:
{
    core::InstanceCreator_O* obj_gc_safe = reinterpret_cast<core::InstanceCreator_O*>(client);
    obj_gc_safe->~InstanceCreator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_cffi__Pointer_O:
{
    cffi::Pointer_O* obj_gc_safe = reinterpret_cast<cffi::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    obj_gc_safe->~CxxObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
    core::WeakKeyMapping_O* obj_gc_safe = reinterpret_cast<core::WeakKeyMapping_O*>(client);
    obj_gc_safe->~WeakKeyMapping_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Cache_O:
{
    core::Cache_O* obj_gc_safe = reinterpret_cast<core::Cache_O*>(client);
    obj_gc_safe->~Cache_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    core::LambdaListHandler_O* obj_gc_safe = reinterpret_cast<core::LambdaListHandler_O*>(client);
    obj_gc_safe->~LambdaListHandler_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
    llvmo::InsertPoint_O* obj_gc_safe = reinterpret_cast<llvmo::InsertPoint_O*>(client);
    obj_gc_safe->~InsertPoint_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    obj_gc_safe->~SourceFileInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SNode_O:
{
    core::SNode_O* obj_gc_safe = reinterpret_cast<core::SNode_O*>(client);
    obj_gc_safe->~SNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LeafSNode_O:
{
    core::LeafSNode_O* obj_gc_safe = reinterpret_cast<core::LeafSNode_O*>(client);
    obj_gc_safe->~LeafSNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BranchSNode_O:
{
    core::BranchSNode_O* obj_gc_safe = reinterpret_cast<core::BranchSNode_O*>(client);
    obj_gc_safe->~BranchSNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    obj_gc_safe->~Path_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
    asttooling::AstVisitor_O* obj_gc_safe = reinterpret_cast<asttooling::AstVisitor_O*>(client);
    obj_gc_safe->~AstVisitor_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    llvmo::AttributeSet_O* obj_gc_safe = reinterpret_cast<llvmo::AttributeSet_O*>(client);
    obj_gc_safe->~AttributeSet_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StructureObject_O:
{
    core::StructureObject_O* obj_gc_safe = reinterpret_cast<core::StructureObject_O*>(client);
    obj_gc_safe->~StructureObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    core::InvocationHistoryFrameIterator_O* obj_gc_safe = reinterpret_cast<core::InvocationHistoryFrameIterator_O*>(client);
    obj_gc_safe->~InvocationHistoryFrameIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Package_O:
{
    core::Package_O* obj_gc_safe = reinterpret_cast<core::Package_O*>(client);
    obj_gc_safe->~Package_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    obj_gc_safe->~DirectoryEntry_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Character_dummy_O:
{
    core::Character_dummy_O* obj_gc_safe = reinterpret_cast<core::Character_dummy_O*>(client);
    obj_gc_safe->~Character_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    obj_gc_safe->~CompiledFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
    core::SingleDispatchGenericFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunction_O*>(client);
    obj_gc_safe->~SingleDispatchGenericFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SpecialForm_O:
{
    core::SpecialForm_O* obj_gc_safe = reinterpret_cast<core::SpecialForm_O*>(client);
    obj_gc_safe->~SpecialForm_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
    core::SingleDispatchEffectiveMethodFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchEffectiveMethodFunction_O*>(client);
    obj_gc_safe->~SingleDispatchEffectiveMethodFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    obj_gc_safe->~Instance_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    clbind::ClassRegistry_O* obj_gc_safe = reinterpret_cast<clbind::ClassRegistry_O*>(client);
    obj_gc_safe->~ClassRegistry_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
    llvmo::DebugInfo_O* obj_gc_safe = reinterpret_cast<llvmo::DebugInfo_O*>(client);
    obj_gc_safe->~DebugInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
    llvmo::DIDerivedType_O* obj_gc_safe = reinterpret_cast<llvmo::DIDerivedType_O*>(client);
    obj_gc_safe->~DIDerivedType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIArray_O:
{
    llvmo::DIArray_O* obj_gc_safe = reinterpret_cast<llvmo::DIArray_O*>(client);
    obj_gc_safe->~DIArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
    llvmo::DIBasicType_O* obj_gc_safe = reinterpret_cast<llvmo::DIBasicType_O*>(client);
    obj_gc_safe->~DIBasicType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
    llvmo::DISubprogram_O* obj_gc_safe = reinterpret_cast<llvmo::DISubprogram_O*>(client);
    obj_gc_safe->~DISubprogram_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
    llvmo::DILexicalBlock_O* obj_gc_safe = reinterpret_cast<llvmo::DILexicalBlock_O*>(client);
    obj_gc_safe->~DILexicalBlock_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
    llvmo::DICompileUnit_O* obj_gc_safe = reinterpret_cast<llvmo::DICompileUnit_O*>(client);
    obj_gc_safe->~DICompileUnit_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
    llvmo::DIDescriptor_O* obj_gc_safe = reinterpret_cast<llvmo::DIDescriptor_O*>(client);
    obj_gc_safe->~DIDescriptor_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIType_O:
{
    llvmo::DIType_O* obj_gc_safe = reinterpret_cast<llvmo::DIType_O*>(client);
    obj_gc_safe->~DIType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
    llvmo::DISubroutineType_O* obj_gc_safe = reinterpret_cast<llvmo::DISubroutineType_O*>(client);
    obj_gc_safe->~DISubroutineType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
    llvmo::DICompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::DICompositeType_O*>(client);
    obj_gc_safe->~DICompositeType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
    llvmo::DITypeArray_O* obj_gc_safe = reinterpret_cast<llvmo::DITypeArray_O*>(client);
    obj_gc_safe->~DITypeArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIFile_O:
{
    llvmo::DIFile_O* obj_gc_safe = reinterpret_cast<llvmo::DIFile_O*>(client);
    obj_gc_safe->~DIFile_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DIScope_O:
{
    llvmo::DIScope_O* obj_gc_safe = reinterpret_cast<llvmo::DIScope_O*>(client);
    obj_gc_safe->~DIScope_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SmallMultimap_O:
{
    core::SmallMultimap_O* obj_gc_safe = reinterpret_cast<core::SmallMultimap_O*>(client);
    obj_gc_safe->~SmallMultimap_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    obj_gc_safe->~Pathname_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    obj_gc_safe->~LogicalPathname_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__PosixTime_O:
{
    core::PosixTime_O* obj_gc_safe = reinterpret_cast<core::PosixTime_O*>(client);
    obj_gc_safe->~PosixTime_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SmallMap_O:
{
    core::SmallMap_O* obj_gc_safe = reinterpret_cast<core::SmallMap_O*>(client);
    obj_gc_safe->~SmallMap_O();
    return;
}
obj_finalize_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
    core::Lisp_O* obj_gc_safe = reinterpret_cast<core::Lisp_O*>(client);
    obj_gc_safe->~Lisp_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CoreExposer_O:
{
    core::CoreExposer_O* obj_gc_safe = reinterpret_cast<core::CoreExposer_O*>(client);
    obj_gc_safe->~CoreExposer_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::KeywordArgument>"));}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::RequiredArgument>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_finalize_KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O:
{
    asttooling::internal::VariadicOperatorMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::VariadicOperatorMatcherDescriptor_O*>(client);
    obj_gc_safe->~VariadicOperatorMatcherDescriptor_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolStorage>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ContextFrame>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::T_O *>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::AuxArgument>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ParserValue>"));}
obj_finalize_KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O:
{
    asttooling::internal::FreeFuncMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::FreeFuncMatcherDescriptor_O*>(client);
    obj_gc_safe->~FreeFuncMatcherDescriptor_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O:
{
    asttooling::internal::FixedArgCountMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::FixedArgCountMatcherDescriptor_O*>(client);
    obj_gc_safe->~FixedArgCountMatcherDescriptor_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>"));}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    asttooling::DerivableFrontendActionFactory* obj_gc_safe = reinterpret_cast<asttooling::DerivableFrontendActionFactory*>(client);
    obj_gc_safe->~DerivableFrontendActionFactory();
    return;
}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>"));}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    asttooling::DerivableMatchCallback* obj_gc_safe = reinterpret_cast<asttooling::DerivableMatchCallback*>(client);
    obj_gc_safe->~DerivableMatchCallback();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O:
{
    asttooling::internal::OverloadedMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::OverloadedMatcherDescriptor_O*>(client);
    obj_gc_safe->~OverloadedMatcherDescriptor_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ErrorContent>"));}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    asttooling::DerivableASTFrontendAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableASTFrontendAction*>(client);
    obj_gc_safe->~DerivableASTFrontendAction();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::Message>"));}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    asttooling::DerivableSyntaxOnlyAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableSyntaxOnlyAction*>(client);
    obj_gc_safe->~DerivableSyntaxOnlyAction();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::OptionalArgument>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>"));}
#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_HELPERS)

#endif // defined(GC_OBJ_FINALIZE_HELPERS)
#if defined(GC_OBJ_FINALIZE_TABLE)
static void* OBJ_FINALIZE_table[] = { 
  /* 5 */ &&obj_finalize_KIND_ROOTCLASSALLOC_clbind__detail__class_map,
  /* 6 */ &&obj_finalize_KIND_BOOTSTRAP_core__T_O,
  /* 7 */ &&obj_finalize_KIND_LISPALLOC_core__VaList_dummy_O,
  /* 8 */ &&obj_finalize_KIND_LISPALLOC_core__Cons_O,
  /* 9 */ &&obj_finalize_KIND_LISPALLOC_core__General_O,
  /* 10 */ &&obj_finalize_KIND_LISPALLOC_core__MultiStringBuffer_O,
  /* 11 */ &&obj_finalize_KIND_LISPALLOC_core__ReadTable_O,
  /* 12 */ &&obj_finalize_KIND_LISPALLOC_core__Number_O,
  /* 13 */ &&obj_finalize_KIND_LISPALLOC_core__Complex_O,
  /* 14 */ &&obj_finalize_KIND_LISPALLOC_core__Real_O,
  /* 15 */ &&obj_finalize_KIND_LISPALLOC_core__Rational_O,
  /* 16 */ &&obj_finalize_KIND_LISPALLOC_core__Integer_O,
  /* 17 */ &&obj_finalize_KIND_LISPALLOC_core__Bignum_O,
  /* 18 */ &&obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 19 */ &&obj_finalize_KIND_LISPALLOC_core__Ratio_O,
  /* 20 */ &&obj_finalize_KIND_LISPALLOC_core__Float_O,
  /* 21 */ &&obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 22 */ &&obj_finalize_KIND_LISPALLOC_core__LongFloat_O,
  /* 23 */ &&obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 24 */ &&obj_finalize_KIND_LISPALLOC_core__ShortFloat_O,
  /* 25 */ &&obj_finalize_KIND_LISPALLOC_core__FileStatus_O,
  /* 26 */ &&obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 27 */ &&obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 28 */ &&obj_finalize_KIND_LISPALLOC_core__Environment_O,
  /* 29 */ &&obj_finalize_KIND_LISPALLOC_core__ActivationFrame_O,
  /* 30 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 31 */ &&obj_finalize_KIND_LISPALLOC_core__ValueFrame_O,
  /* 32 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 33 */ &&obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 34 */ &&obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 35 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 36 */ &&obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 37 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 38 */ &&obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 39 */ &&obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 40 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 41 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 42 */ &&obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 43 */ &&obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 44 */ &&obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 45 */ &&obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 46 */ &&obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O,
  /* 47 */ &&obj_finalize_KIND_LISPALLOC_core__Array_O,
  /* 48 */ &&obj_finalize_KIND_LISPALLOC_core__ArrayObjects_O,
  /* 49 */ &&obj_finalize_KIND_LISPALLOC_core__ArrayDisplaced_O,
  /* 50 */ &&obj_finalize_KIND_LISPALLOC_core__Vector_O,
  /* 51 */ &&obj_finalize_KIND_LISPALLOC_core__BitVector_O,
  /* 52 */ &&obj_finalize_KIND_LISPALLOC_core__SimpleBitVector_O,
  /* 53 */ &&obj_finalize_KIND_LISPALLOC_core__BitVectorWithFillPtr_O,
  /* 54 */ &&obj_finalize_KIND_LISPALLOC_core__VectorDisplaced_O,
  /* 55 */ &&obj_finalize_KIND_LISPALLOC_core__String_O,
  /* 56 */ &&obj_finalize_KIND_BOOTSTRAP_core__Str_O,
  /* 57 */ &&obj_finalize_KIND_LISPALLOC_core__StrWithFillPtr_O,
  /* 58 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjects_O,
  /* 59 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O,
  /* 60 */ &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchMethod_O,
  /* 61 */ &&obj_finalize_KIND_LISPALLOC_core__RandomState_O,
  /* 62 */ &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
  /* 63 */ &&obj_finalize_KIND_LISPALLOC_core__SequenceStepper_O,
  /* 64 */ &&obj_finalize_KIND_LISPALLOC_core__ConsStepper_O,
  /* 65 */ &&obj_finalize_KIND_LISPALLOC_core__VectorStepper_O,
  /* 66 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DebugLoc_O,
  /* 67 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Attribute_O,
  /* 68 */ &&obj_finalize_KIND_LISPALLOC_core__RegexMatch_O,
  /* 69 */ &&obj_finalize_KIND_LISPALLOC_core__WeakPointer_O,
  /* 70 */ &&obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 71 */ &&obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 72 */ &&obj_finalize_KIND_BOOTSTRAP_core__Specializer_O,
  /* 73 */ &&obj_finalize_KIND_BOOTSTRAP_core__Class_O,
  /* 74 */ &&obj_finalize_KIND_BOOTSTRAP_core__StdClass_O,
  /* 75 */ &&obj_finalize_KIND_BOOTSTRAP_core__StandardClass_O,
  /* 76 */ &&obj_finalize_KIND_LISPALLOC_core__FuncallableStandardClass_O,
  /* 77 */ &&obj_finalize_KIND_BOOTSTRAP_core__StructureClass_O,
  /* 78 */ &&obj_finalize_KIND_LISPALLOC_core__ForwardReferencedClass_O,
  /* 79 */ &&obj_finalize_KIND_LISPALLOC_core__CxxClass_O,
  /* 80 */ &&obj_finalize_KIND_BOOTSTRAP_core__BuiltInClass_O,
  /* 81 */ &&obj_finalize_KIND_LISPALLOC_clbind__ClassRep_O,
  /* 82 */ &&obj_finalize_KIND_LISPALLOC_core__ExternalObject_O,
  /* 83 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Value_O,
  /* 84 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Argument_O,
  /* 85 */ &&obj_finalize_KIND_LISPALLOC_llvmo__User_O,
  /* 86 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Instruction_O,
  /* 87 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AtomicRMWInst_O,
  /* 88 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LandingPadInst_O,
  /* 89 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PHINode_O,
  /* 90 */ &&obj_finalize_KIND_LISPALLOC_llvmo__CallInst_O,
  /* 91 */ &&obj_finalize_KIND_LISPALLOC_llvmo__StoreInst_O,
  /* 92 */ &&obj_finalize_KIND_LISPALLOC_llvmo__UnaryInstruction_O,
  /* 93 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LoadInst_O,
  /* 94 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AllocaInst_O,
  /* 95 */ &&obj_finalize_KIND_LISPALLOC_llvmo__VAArgInst_O,
  /* 96 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O,
  /* 97 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TerminatorInst_O,
  /* 98 */ &&obj_finalize_KIND_LISPALLOC_llvmo__UnreachableInst_O,
  /* 99 */ &&obj_finalize_KIND_LISPALLOC_llvmo__SwitchInst_O,
  /* 100 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ReturnInst_O,
  /* 101 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ResumeInst_O,
  /* 102 */ &&obj_finalize_KIND_LISPALLOC_llvmo__BranchInst_O,
  /* 103 */ &&obj_finalize_KIND_LISPALLOC_llvmo__InvokeInst_O,
  /* 104 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IndirectBrInst_O,
  /* 105 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FenceInst_O,
  /* 106 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Constant_O,
  /* 107 */ &&obj_finalize_KIND_LISPALLOC_llvmo__BlockAddress_O,
  /* 108 */ &&obj_finalize_KIND_LISPALLOC_llvmo__GlobalValue_O,
  /* 109 */ &&obj_finalize_KIND_LISPALLOC_llvmo__GlobalVariable_O,
  /* 110 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Function_O,
  /* 111 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantArray_O,
  /* 112 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantInt_O,
  /* 113 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataSequential_O,
  /* 114 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataArray_O,
  /* 115 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantStruct_O,
  /* 116 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantFP_O,
  /* 117 */ &&obj_finalize_KIND_LISPALLOC_llvmo__UndefValue_O,
  /* 118 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantPointerNull_O,
  /* 119 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantExpr_O,
  /* 120 */ &&obj_finalize_KIND_LISPALLOC_llvmo__BasicBlock_O,
  /* 121 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IRBuilderBase_O,
  /* 122 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IRBuilder_O,
  /* 123 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIBuilder_O,
  /* 124 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Metadata_O,
  /* 125 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ValueAsMetadata_O,
  /* 126 */ &&obj_finalize_KIND_LISPALLOC_llvmo__MDNode_O,
  /* 127 */ &&obj_finalize_KIND_LISPALLOC_llvmo__MDString_O,
  /* 128 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ExecutionEngine_O,
  /* 129 */ &&obj_finalize_KIND_LISPALLOC_llvmo__APFloat_O,
  /* 130 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBuilder_O,
  /* 131 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DataLayout_O,
  /* 132 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Triple_O,
  /* 133 */ &&obj_finalize_KIND_LISPALLOC_llvmo__APInt_O,
  /* 134 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBase_O,
  /* 135 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionPassManager_O,
  /* 136 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PassManager_O,
  /* 137 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetMachine_O,
  /* 138 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O,
  /* 139 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetOptions_O,
  /* 140 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Type_O,
  /* 141 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IntegerType_O,
  /* 142 */ &&obj_finalize_KIND_LISPALLOC_llvmo__CompositeType_O,
  /* 143 */ &&obj_finalize_KIND_LISPALLOC_llvmo__SequentialType_O,
  /* 144 */ &&obj_finalize_KIND_LISPALLOC_llvmo__VectorType_O,
  /* 145 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PointerType_O,
  /* 146 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ArrayType_O,
  /* 147 */ &&obj_finalize_KIND_LISPALLOC_llvmo__StructType_O,
  /* 148 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionType_O,
  /* 149 */ &&obj_finalize_KIND_LISPALLOC_llvmo__NamedMDNode_O,
  /* 150 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Linker_O,
  /* 151 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Pass_O,
  /* 152 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionPass_O,
  /* 153 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ModulePass_O,
  /* 154 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ImmutablePass_O,
  /* 155 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DataLayoutPass_O,
  /* 156 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O,
  /* 157 */ &&obj_finalize_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O,
  /* 158 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O,
  /* 159 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Module_O,
  /* 160 */ &&obj_finalize_KIND_LISPALLOC_llvmo__EngineBuilder_O,
  /* 161 */ &&obj_finalize_KIND_LISPALLOC_core__ForeignData_O,
  /* 162 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LLVMContext_O,
  /* 163 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Target_O,
  /* 164 */ &&obj_finalize_KIND_LISPALLOC_core__LoadTimeValues_O,
  /* 165 */ &&obj_finalize_KIND_LISPALLOC_core__Binder_O,
  /* 166 */ &&obj_finalize_KIND_LISPALLOC_core__IntArray_O,
  /* 167 */ &&obj_finalize_KIND_LISPALLOC_core__SourceManager_O,
  /* 168 */ &&obj_finalize_KIND_LISPALLOC_core__Record_O,
  /* 169 */ &&obj_finalize_KIND_LISPALLOC_core__Functor_O,
  /* 170 */ &&obj_finalize_KIND_LISPALLOC_core__Closure_O,
  /* 171 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionClosure_O,
  /* 172 */ &&obj_finalize_KIND_LISPALLOC_core__InstanceClosure_O,
  /* 173 */ &&obj_finalize_KIND_LISPALLOC_core__CompiledClosure_O,
  /* 174 */ &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O,
  /* 175 */ &&obj_finalize_KIND_LISPALLOC_core__MacroClosure_O,
  /* 176 */ &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O,
  /* 177 */ &&obj_finalize_KIND_LISPALLOC_core__InterpretedClosure_O,
  /* 178 */ &&obj_finalize_KIND_LISPALLOC_core__LightUserData_O,
  /* 179 */ &&obj_finalize_KIND_LISPALLOC_core__UserData_O,
  /* 180 */ &&obj_finalize_KIND_BOOTSTRAP_core__Symbol_O,
  /* 181 */ &&obj_finalize_KIND_LISPALLOC_core__Null_O,
  /* 182 */ &&obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 183 */ &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__Iterator_O,
  /* 184 */ &&obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O,
  /* 185 */ &&obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O,
  /* 186 */ &&obj_finalize_KIND_LISPALLOC_core__Regex_O,
  /* 187 */ &&obj_finalize_KIND_LISPALLOC_core__PosixTimeDuration_O,
  /* 188 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 189 */ &&obj_finalize_KIND_LISPALLOC_core__CandoException_O,
  /* 190 */ &&obj_finalize_KIND_LISPALLOC_core__Stream_O,
  /* 191 */ &&obj_finalize_KIND_LISPALLOC_core__AnsiStream_O,
  /* 192 */ &&obj_finalize_KIND_LISPALLOC_core__FileStream_O,
  /* 193 */ &&obj_finalize_KIND_LISPALLOC_core__IOStreamStream_O,
  /* 194 */ &&obj_finalize_KIND_LISPALLOC_core__IOFileStream_O,
  /* 195 */ &&obj_finalize_KIND_LISPALLOC_core__ConcatenatedStream_O,
  /* 196 */ &&obj_finalize_KIND_LISPALLOC_core__StringStream_O,
  /* 197 */ &&obj_finalize_KIND_LISPALLOC_core__StringInputStream_O,
  /* 198 */ &&obj_finalize_KIND_LISPALLOC_core__StringOutputStream_O,
  /* 199 */ &&obj_finalize_KIND_LISPALLOC_core__SynonymStream_O,
  /* 200 */ &&obj_finalize_KIND_LISPALLOC_core__EchoStream_O,
  /* 201 */ &&obj_finalize_KIND_LISPALLOC_core__TwoWayStream_O,
  /* 202 */ &&obj_finalize_KIND_LISPALLOC_core__BroadcastStream_O,
  /* 203 */ &&obj_finalize_KIND_LISPALLOC_core__Reader_O,
  /* 204 */ &&obj_finalize_KIND_LISPALLOC_core__SharpEqualWrapper_O,
  /* 205 */ &&obj_finalize_KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O,
  /* 206 */ &&obj_finalize_KIND_LISPALLOC_core__Archive_O,
  /* 207 */ &&obj_finalize_KIND_LISPALLOC_core__SaveArchive_O,
  /* 208 */ &&obj_finalize_KIND_LISPALLOC_core__SexpSaveArchive_O,
  /* 209 */ &&obj_finalize_KIND_LISPALLOC_core__LoadArchive_O,
  /* 210 */ &&obj_finalize_KIND_LISPALLOC_core__SexpLoadArchive_O,
  /* 211 */ &&obj_finalize_KIND_LISPALLOC_core__HashTable_O,
  /* 212 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEq_O,
  /* 213 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEqualp_O,
  /* 214 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEql_O,
  /* 215 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEqual_O,
  /* 216 */ &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__Creator_O,
  /* 217 */ &&obj_finalize_KIND_LISPALLOC_clbind__DummyCreator_O,
  /* 218 */ &&obj_finalize_KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O,
  /* 219 */ &&obj_finalize_KIND_LISPALLOC_core__InstanceCreator_O,
  /* 220 */ &&obj_finalize_KIND_LISPALLOC_cffi__Pointer_O,
  /* 221 */ &&obj_finalize_KIND_LISPALLOC_core__CxxObject_O,
  /* 222 */ &&obj_finalize_KIND_LISPALLOC_core__WeakKeyMapping_O,
  /* 223 */ &&obj_finalize_KIND_LISPALLOC_core__Cache_O,
  /* 224 */ &&obj_finalize_KIND_LISPALLOC_core__LambdaListHandler_O,
  /* 225 */ &&obj_finalize_KIND_LISPALLOC_llvmo__InsertPoint_O,
  /* 226 */ &&obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 227 */ &&obj_finalize_KIND_LISPALLOC_core__SNode_O,
  /* 228 */ &&obj_finalize_KIND_LISPALLOC_core__LeafSNode_O,
  /* 229 */ &&obj_finalize_KIND_LISPALLOC_core__BranchSNode_O,
  /* 230 */ &&obj_finalize_KIND_LISPALLOC_core__Path_O,
  /* 231 */ &&obj_finalize_KIND_LISPALLOC_asttooling__AstVisitor_O,
  /* 232 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AttributeSet_O,
  /* 233 */ &&obj_finalize_KIND_LISPALLOC_core__StructureObject_O,
  /* 234 */ &&obj_finalize_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O,
  /* 235 */ &&obj_finalize_KIND_LISPALLOC_core__Package_O,
  /* 236 */ &&obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O,
  /* 237 */ &&obj_finalize_KIND_LISPALLOC_core__Character_dummy_O,
  /* 238 */ &&obj_finalize_KIND_LISPALLOC_core__Function_O,
  /* 239 */ &&obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 240 */ &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O,
  /* 241 */ &&obj_finalize_KIND_LISPALLOC_core__SpecialForm_O,
  /* 242 */ &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O,
  /* 243 */ &&obj_finalize_KIND_LISPALLOC_core__Instance_O,
  /* 244 */ &&obj_finalize_KIND_LISPALLOC_core__Pointer_O,
  /* 245 */ &&obj_finalize_KIND_LISPALLOC_clbind__ClassRegistry_O,
  /* 246 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DebugInfo_O,
  /* 247 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIDerivedType_O,
  /* 248 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIArray_O,
  /* 249 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIBasicType_O,
  /* 250 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DISubprogram_O,
  /* 251 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DILexicalBlock_O,
  /* 252 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DICompileUnit_O,
  /* 253 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIDescriptor_O,
  /* 254 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIType_O,
  /* 255 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DISubroutineType_O,
  /* 256 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DICompositeType_O,
  /* 257 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DITypeArray_O,
  /* 258 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIFile_O,
  /* 259 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DIScope_O,
  /* 260 */ &&obj_finalize_KIND_LISPALLOC_core__SmallMultimap_O,
  /* 261 */ &&obj_finalize_KIND_LISPALLOC_core__Pathname_O,
  /* 262 */ &&obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O,
  /* 263 */ &&obj_finalize_KIND_LISPALLOC_core__PosixTime_O,
  /* 264 */ &&obj_finalize_KIND_LISPALLOC_core__SmallMap_O,
  /* 265 */ &&obj_finalize_KIND_ROOTCLASSALLOC_core__Lisp_O,
  /* 266 */ &&obj_finalize_KIND_LISPALLOC_core__CoreExposer_O,
  /* 267 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_,
  /* 268 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 269 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
  /* 270 */ &&obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 271 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_,
  /* 272 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_,
  /* 273 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 274 */ &&obj_finalize_KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O,
  /* 275 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___,
  /* 276 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_,
  /* 277 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_,
  /* 278 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_,
  /* 279 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__,
  /* 280 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_,
  /* 281 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_,
  /* 282 */ &&obj_finalize_KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O,
  /* 283 */ &&obj_finalize_KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O,
  /* 284 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 285 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 286 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___,
  /* 287 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 288 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__,
  /* 289 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory,
  /* 290 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_,
  /* 291 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableMatchCallback,
  /* 292 */ &&obj_finalize_KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O,
  /* 293 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_,
  /* 294 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction,
  /* 295 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_,
  /* 296 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_,
  /* 297 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 298 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction,
  /* 299 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__,
  /* 300 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_,
  /* 301 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__,
  /* 302 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 303 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 304 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 305 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__,
   NULL
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_OBJ_DEALLOCATOR)
obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
    clbind::detail::class_map* obj_gc_safe = reinterpret_cast<clbind::detail::class_map*>(client);
    GC<clbind::detail::class_map>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__T_O:
{
    core::T_O* obj_gc_safe = reinterpret_cast<core::T_O*>(client);
    GC<core::T_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VaList_dummy_O:
{
    core::VaList_dummy_O* obj_gc_safe = reinterpret_cast<core::VaList_dummy_O*>(client);
    GC<core::VaList_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    GC<core::Cons_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    GC<core::General_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
    core::MultiStringBuffer_O* obj_gc_safe = reinterpret_cast<core::MultiStringBuffer_O*>(client);
    GC<core::MultiStringBuffer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ReadTable_O:
{
    core::ReadTable_O* obj_gc_safe = reinterpret_cast<core::ReadTable_O*>(client);
    GC<core::ReadTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    GC<core::Number_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    GC<core::Complex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    GC<core::Real_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    GC<core::Rational_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    GC<core::Integer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    GC<core::Bignum_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    GC<core::Fixnum_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    GC<core::Ratio_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    GC<core::Float_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    GC<core::DoubleFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    GC<core::LongFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    GC<core::SingleFloat_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    GC<core::ShortFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    GC<core::FileStatus_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    GC<core::WeakHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    GC<core::WeakKeyHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    GC<core::Environment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ActivationFrame_O:
{
    core::ActivationFrame_O* obj_gc_safe = reinterpret_cast<core::ActivationFrame_O*>(client);
    GC<core::ActivationFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    GC<core::TagbodyFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    GC<core::ValueFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    GC<core::FunctionFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    GC<core::LexicalEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    GC<core::RuntimeVisibleEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    GC<core::FunctionValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    GC<core::ValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    GC<core::TagbodyEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    GC<core::CompileTimeEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    GC<core::UnwindProtectEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    GC<core::SymbolMacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    GC<core::FunctionContainerEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    GC<core::StackValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    GC<core::BlockEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    GC<core::MacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    GC<core::CatchEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    GC<core::GlueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    GC<core::Array_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayObjects_O:
{
    core::ArrayObjects_O* obj_gc_safe = reinterpret_cast<core::ArrayObjects_O*>(client);
    GC<core::ArrayObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
    core::ArrayDisplaced_O* obj_gc_safe = reinterpret_cast<core::ArrayDisplaced_O*>(client);
    GC<core::ArrayDisplaced_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    GC<core::Vector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVector_O:
{
    core::BitVector_O* obj_gc_safe = reinterpret_cast<core::BitVector_O*>(client);
    GC<core::BitVector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SimpleBitVector_O:
{
    core::SimpleBitVector_O* obj_gc_safe = reinterpret_cast<core::SimpleBitVector_O*>(client);
    GC<core::SimpleBitVector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
    core::BitVectorWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::BitVectorWithFillPtr_O*>(client);
    GC<core::BitVectorWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorDisplaced_O:
{
    core::VectorDisplaced_O* obj_gc_safe = reinterpret_cast<core::VectorDisplaced_O*>(client);
    GC<core::VectorDisplaced_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    GC<core::String_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    GC<core::Str_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
    core::StrWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::StrWithFillPtr_O*>(client);
    GC<core::StrWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    GC<core::VectorObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    GC<core::VectorObjectsWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
    core::SingleDispatchMethod_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchMethod_O*>(client);
    GC<core::SingleDispatchMethod_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RandomState_O:
{
    core::RandomState_O* obj_gc_safe = reinterpret_cast<core::RandomState_O*>(client);
    GC<core::RandomState_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    GC<core::WrappedPointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SequenceStepper_O:
{
    core::SequenceStepper_O* obj_gc_safe = reinterpret_cast<core::SequenceStepper_O*>(client);
    GC<core::SequenceStepper_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ConsStepper_O:
{
    core::ConsStepper_O* obj_gc_safe = reinterpret_cast<core::ConsStepper_O*>(client);
    GC<core::ConsStepper_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorStepper_O:
{
    core::VectorStepper_O* obj_gc_safe = reinterpret_cast<core::VectorStepper_O*>(client);
    GC<core::VectorStepper_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
    llvmo::DebugLoc_O* obj_gc_safe = reinterpret_cast<llvmo::DebugLoc_O*>(client);
    GC<llvmo::DebugLoc_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Attribute_O:
{
    llvmo::Attribute_O* obj_gc_safe = reinterpret_cast<llvmo::Attribute_O*>(client);
    GC<llvmo::Attribute_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RegexMatch_O:
{
    core::RegexMatch_O* obj_gc_safe = reinterpret_cast<core::RegexMatch_O*>(client);
    GC<core::RegexMatch_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakPointer_O:
{
    core::WeakPointer_O* obj_gc_safe = reinterpret_cast<core::WeakPointer_O*>(client);
    GC<core::WeakPointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    GC<core::StandardObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    GC<core::Metaobject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    GC<core::Specializer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    GC<core::Class_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StdClass_O:
{
    core::StdClass_O* obj_gc_safe = reinterpret_cast<core::StdClass_O*>(client);
    GC<core::StdClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardClass_O:
{
    core::StandardClass_O* obj_gc_safe = reinterpret_cast<core::StandardClass_O*>(client);
    GC<core::StandardClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
    core::FuncallableStandardClass_O* obj_gc_safe = reinterpret_cast<core::FuncallableStandardClass_O*>(client);
    GC<core::FuncallableStandardClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StructureClass_O:
{
    core::StructureClass_O* obj_gc_safe = reinterpret_cast<core::StructureClass_O*>(client);
    GC<core::StructureClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
    core::ForwardReferencedClass_O* obj_gc_safe = reinterpret_cast<core::ForwardReferencedClass_O*>(client);
    GC<core::ForwardReferencedClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxClass_O:
{
    core::CxxClass_O* obj_gc_safe = reinterpret_cast<core::CxxClass_O*>(client);
    GC<core::CxxClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    core::BuiltInClass_O* obj_gc_safe = reinterpret_cast<core::BuiltInClass_O*>(client);
    GC<core::BuiltInClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRep_O:
{
    clbind::ClassRep_O* obj_gc_safe = reinterpret_cast<clbind::ClassRep_O*>(client);
    GC<clbind::ClassRep_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ExternalObject_O:
{
    core::ExternalObject_O* obj_gc_safe = reinterpret_cast<core::ExternalObject_O*>(client);
    GC<core::ExternalObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Value_O:
{
    llvmo::Value_O* obj_gc_safe = reinterpret_cast<llvmo::Value_O*>(client);
    GC<llvmo::Value_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Argument_O:
{
    llvmo::Argument_O* obj_gc_safe = reinterpret_cast<llvmo::Argument_O*>(client);
    GC<llvmo::Argument_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__User_O:
{
    llvmo::User_O* obj_gc_safe = reinterpret_cast<llvmo::User_O*>(client);
    GC<llvmo::User_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Instruction_O:
{
    llvmo::Instruction_O* obj_gc_safe = reinterpret_cast<llvmo::Instruction_O*>(client);
    GC<llvmo::Instruction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    llvmo::AtomicRMWInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicRMWInst_O*>(client);
    GC<llvmo::AtomicRMWInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    llvmo::LandingPadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LandingPadInst_O*>(client);
    GC<llvmo::LandingPadInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PHINode_O:
{
    llvmo::PHINode_O* obj_gc_safe = reinterpret_cast<llvmo::PHINode_O*>(client);
    GC<llvmo::PHINode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CallInst_O:
{
    llvmo::CallInst_O* obj_gc_safe = reinterpret_cast<llvmo::CallInst_O*>(client);
    GC<llvmo::CallInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    llvmo::StoreInst_O* obj_gc_safe = reinterpret_cast<llvmo::StoreInst_O*>(client);
    GC<llvmo::StoreInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    llvmo::UnaryInstruction_O* obj_gc_safe = reinterpret_cast<llvmo::UnaryInstruction_O*>(client);
    GC<llvmo::UnaryInstruction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    llvmo::LoadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LoadInst_O*>(client);
    GC<llvmo::LoadInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    llvmo::AllocaInst_O* obj_gc_safe = reinterpret_cast<llvmo::AllocaInst_O*>(client);
    GC<llvmo::AllocaInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    llvmo::VAArgInst_O* obj_gc_safe = reinterpret_cast<llvmo::VAArgInst_O*>(client);
    GC<llvmo::VAArgInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    llvmo::AtomicCmpXchgInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicCmpXchgInst_O*>(client);
    GC<llvmo::AtomicCmpXchgInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    llvmo::TerminatorInst_O* obj_gc_safe = reinterpret_cast<llvmo::TerminatorInst_O*>(client);
    GC<llvmo::TerminatorInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    llvmo::UnreachableInst_O* obj_gc_safe = reinterpret_cast<llvmo::UnreachableInst_O*>(client);
    GC<llvmo::UnreachableInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    llvmo::SwitchInst_O* obj_gc_safe = reinterpret_cast<llvmo::SwitchInst_O*>(client);
    GC<llvmo::SwitchInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    llvmo::ReturnInst_O* obj_gc_safe = reinterpret_cast<llvmo::ReturnInst_O*>(client);
    GC<llvmo::ReturnInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    llvmo::ResumeInst_O* obj_gc_safe = reinterpret_cast<llvmo::ResumeInst_O*>(client);
    GC<llvmo::ResumeInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    llvmo::BranchInst_O* obj_gc_safe = reinterpret_cast<llvmo::BranchInst_O*>(client);
    GC<llvmo::BranchInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    llvmo::InvokeInst_O* obj_gc_safe = reinterpret_cast<llvmo::InvokeInst_O*>(client);
    GC<llvmo::InvokeInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    llvmo::IndirectBrInst_O* obj_gc_safe = reinterpret_cast<llvmo::IndirectBrInst_O*>(client);
    GC<llvmo::IndirectBrInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    llvmo::FenceInst_O* obj_gc_safe = reinterpret_cast<llvmo::FenceInst_O*>(client);
    GC<llvmo::FenceInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Constant_O:
{
    llvmo::Constant_O* obj_gc_safe = reinterpret_cast<llvmo::Constant_O*>(client);
    GC<llvmo::Constant_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    llvmo::BlockAddress_O* obj_gc_safe = reinterpret_cast<llvmo::BlockAddress_O*>(client);
    GC<llvmo::BlockAddress_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    llvmo::GlobalValue_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalValue_O*>(client);
    GC<llvmo::GlobalValue_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    llvmo::GlobalVariable_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalVariable_O*>(client);
    GC<llvmo::GlobalVariable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Function_O:
{
    llvmo::Function_O* obj_gc_safe = reinterpret_cast<llvmo::Function_O*>(client);
    GC<llvmo::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    llvmo::ConstantArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantArray_O*>(client);
    GC<llvmo::ConstantArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    llvmo::ConstantInt_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantInt_O*>(client);
    GC<llvmo::ConstantInt_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    llvmo::ConstantDataSequential_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataSequential_O*>(client);
    GC<llvmo::ConstantDataSequential_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    llvmo::ConstantDataArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataArray_O*>(client);
    GC<llvmo::ConstantDataArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    llvmo::ConstantStruct_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantStruct_O*>(client);
    GC<llvmo::ConstantStruct_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    llvmo::ConstantFP_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantFP_O*>(client);
    GC<llvmo::ConstantFP_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    llvmo::UndefValue_O* obj_gc_safe = reinterpret_cast<llvmo::UndefValue_O*>(client);
    GC<llvmo::UndefValue_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    llvmo::ConstantPointerNull_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantPointerNull_O*>(client);
    GC<llvmo::ConstantPointerNull_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    llvmo::ConstantExpr_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantExpr_O*>(client);
    GC<llvmo::ConstantExpr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    llvmo::BasicBlock_O* obj_gc_safe = reinterpret_cast<llvmo::BasicBlock_O*>(client);
    GC<llvmo::BasicBlock_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    llvmo::IRBuilderBase_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilderBase_O*>(client);
    GC<llvmo::IRBuilderBase_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    llvmo::IRBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilder_O*>(client);
    GC<llvmo::IRBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
    llvmo::DIBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::DIBuilder_O*>(client);
    GC<llvmo::DIBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Metadata_O:
{
    llvmo::Metadata_O* obj_gc_safe = reinterpret_cast<llvmo::Metadata_O*>(client);
    GC<llvmo::Metadata_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    llvmo::ValueAsMetadata_O* obj_gc_safe = reinterpret_cast<llvmo::ValueAsMetadata_O*>(client);
    GC<llvmo::ValueAsMetadata_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDNode_O:
{
    llvmo::MDNode_O* obj_gc_safe = reinterpret_cast<llvmo::MDNode_O*>(client);
    GC<llvmo::MDNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDString_O:
{
    llvmo::MDString_O* obj_gc_safe = reinterpret_cast<llvmo::MDString_O*>(client);
    GC<llvmo::MDString_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    llvmo::ExecutionEngine_O* obj_gc_safe = reinterpret_cast<llvmo::ExecutionEngine_O*>(client);
    GC<llvmo::ExecutionEngine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APFloat_O:
{
    llvmo::APFloat_O* obj_gc_safe = reinterpret_cast<llvmo::APFloat_O*>(client);
    GC<llvmo::APFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    llvmo::PassManagerBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBuilder_O*>(client);
    GC<llvmo::PassManagerBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    llvmo::DataLayout_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayout_O*>(client);
    GC<llvmo::DataLayout_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Triple_O:
{
    llvmo::Triple_O* obj_gc_safe = reinterpret_cast<llvmo::Triple_O*>(client);
    GC<llvmo::Triple_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APInt_O:
{
    llvmo::APInt_O* obj_gc_safe = reinterpret_cast<llvmo::APInt_O*>(client);
    GC<llvmo::APInt_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    llvmo::PassManagerBase_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBase_O*>(client);
    GC<llvmo::PassManagerBase_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    llvmo::FunctionPassManager_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPassManager_O*>(client);
    GC<llvmo::FunctionPassManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManager_O:
{
    llvmo::PassManager_O* obj_gc_safe = reinterpret_cast<llvmo::PassManager_O*>(client);
    GC<llvmo::PassManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    llvmo::TargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::TargetMachine_O*>(client);
    GC<llvmo::TargetMachine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    llvmo::LLVMTargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMTargetMachine_O*>(client);
    GC<llvmo::LLVMTargetMachine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    llvmo::TargetOptions_O* obj_gc_safe = reinterpret_cast<llvmo::TargetOptions_O*>(client);
    GC<llvmo::TargetOptions_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Type_O:
{
    llvmo::Type_O* obj_gc_safe = reinterpret_cast<llvmo::Type_O*>(client);
    GC<llvmo::Type_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    llvmo::IntegerType_O* obj_gc_safe = reinterpret_cast<llvmo::IntegerType_O*>(client);
    GC<llvmo::IntegerType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    llvmo::CompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::CompositeType_O*>(client);
    GC<llvmo::CompositeType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    llvmo::SequentialType_O* obj_gc_safe = reinterpret_cast<llvmo::SequentialType_O*>(client);
    GC<llvmo::SequentialType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VectorType_O:
{
    llvmo::VectorType_O* obj_gc_safe = reinterpret_cast<llvmo::VectorType_O*>(client);
    GC<llvmo::VectorType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PointerType_O:
{
    llvmo::PointerType_O* obj_gc_safe = reinterpret_cast<llvmo::PointerType_O*>(client);
    GC<llvmo::PointerType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    llvmo::ArrayType_O* obj_gc_safe = reinterpret_cast<llvmo::ArrayType_O*>(client);
    GC<llvmo::ArrayType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StructType_O:
{
    llvmo::StructType_O* obj_gc_safe = reinterpret_cast<llvmo::StructType_O*>(client);
    GC<llvmo::StructType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    llvmo::FunctionType_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionType_O*>(client);
    GC<llvmo::FunctionType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    llvmo::NamedMDNode_O* obj_gc_safe = reinterpret_cast<llvmo::NamedMDNode_O*>(client);
    GC<llvmo::NamedMDNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Linker_O:
{
    llvmo::Linker_O* obj_gc_safe = reinterpret_cast<llvmo::Linker_O*>(client);
    GC<llvmo::Linker_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Pass_O:
{
    llvmo::Pass_O* obj_gc_safe = reinterpret_cast<llvmo::Pass_O*>(client);
    GC<llvmo::Pass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    llvmo::FunctionPass_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPass_O*>(client);
    GC<llvmo::FunctionPass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    llvmo::ModulePass_O* obj_gc_safe = reinterpret_cast<llvmo::ModulePass_O*>(client);
    GC<llvmo::ModulePass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    llvmo::ImmutablePass_O* obj_gc_safe = reinterpret_cast<llvmo::ImmutablePass_O*>(client);
    GC<llvmo::ImmutablePass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    llvmo::DataLayoutPass_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayoutPass_O*>(client);
    GC<llvmo::DataLayoutPass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    llvmo::TargetLibraryInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetLibraryInfo_O*>(client);
    GC<llvmo::TargetLibraryInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    llvmo::MCSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::MCSubtargetInfo_O*>(client);
    GC<llvmo::MCSubtargetInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    llvmo::TargetSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetSubtargetInfo_O*>(client);
    GC<llvmo::TargetSubtargetInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Module_O:
{
    llvmo::Module_O* obj_gc_safe = reinterpret_cast<llvmo::Module_O*>(client);
    GC<llvmo::Module_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    llvmo::EngineBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::EngineBuilder_O*>(client);
    GC<llvmo::EngineBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForeignData_O:
{
    core::ForeignData_O* obj_gc_safe = reinterpret_cast<core::ForeignData_O*>(client);
    GC<core::ForeignData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    llvmo::LLVMContext_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMContext_O*>(client);
    GC<llvmo::LLVMContext_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Target_O:
{
    llvmo::Target_O* obj_gc_safe = reinterpret_cast<llvmo::Target_O*>(client);
    GC<llvmo::Target_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadTimeValues_O:
{
    core::LoadTimeValues_O* obj_gc_safe = reinterpret_cast<core::LoadTimeValues_O*>(client);
    GC<core::LoadTimeValues_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Binder_O:
{
    core::Binder_O* obj_gc_safe = reinterpret_cast<core::Binder_O*>(client);
    GC<core::Binder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IntArray_O:
{
    core::IntArray_O* obj_gc_safe = reinterpret_cast<core::IntArray_O*>(client);
    GC<core::IntArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    GC<core::SourceManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    GC<core::Record_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Functor_O:
{
    core::Functor_O* obj_gc_safe = reinterpret_cast<core::Functor_O*>(client);
    GC<core::Functor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Closure_O:
{
    core::Closure_O* obj_gc_safe = reinterpret_cast<core::Closure_O*>(client);
    GC<core::Closure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionClosure_O:
{
    core::FunctionClosure_O* obj_gc_safe = reinterpret_cast<core::FunctionClosure_O*>(client);
    GC<core::FunctionClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InstanceClosure_O:
{
    core::InstanceClosure_O* obj_gc_safe = reinterpret_cast<core::InstanceClosure_O*>(client);
    GC<core::InstanceClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledClosure_O:
{
    core::CompiledClosure_O* obj_gc_safe = reinterpret_cast<core::CompiledClosure_O*>(client);
    GC<core::CompiledClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O:
{
    core::BuiltinClosure_O* obj_gc_safe = reinterpret_cast<core::BuiltinClosure_O*>(client);
    GC<core::BuiltinClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroClosure_O:
{
    core::MacroClosure_O* obj_gc_safe = reinterpret_cast<core::MacroClosure_O*>(client);
    GC<core::MacroClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O:
{
    core::SingleDispatchGenericFunctionClosure_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunctionClosure_O*>(client);
    GC<core::SingleDispatchGenericFunctionClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InterpretedClosure_O:
{
    core::InterpretedClosure_O* obj_gc_safe = reinterpret_cast<core::InterpretedClosure_O*>(client);
    GC<core::InterpretedClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LightUserData_O:
{
    core::LightUserData_O* obj_gc_safe = reinterpret_cast<core::LightUserData_O*>(client);
    GC<core::LightUserData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UserData_O:
{
    core::UserData_O* obj_gc_safe = reinterpret_cast<core::UserData_O*>(client);
    GC<core::UserData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    GC<core::Symbol_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Null_O:
{
    core::Null_O* obj_gc_safe = reinterpret_cast<core::Null_O*>(client);
    GC<core::Null_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    GC<core::SourcePosInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    GC<core::Iterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    GC<core::DirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    GC<core::RecursiveDirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Regex_O:
{
    core::Regex_O* obj_gc_safe = reinterpret_cast<core::Regex_O*>(client);
    GC<core::Regex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
    core::PosixTimeDuration_O* obj_gc_safe = reinterpret_cast<core::PosixTimeDuration_O*>(client);
    GC<core::PosixTimeDuration_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    GC<core::SymbolToEnumConverter_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    GC<core::CandoException_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Stream_O:
{
    core::Stream_O* obj_gc_safe = reinterpret_cast<core::Stream_O*>(client);
    GC<core::Stream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__AnsiStream_O:
{
    core::AnsiStream_O* obj_gc_safe = reinterpret_cast<core::AnsiStream_O*>(client);
    GC<core::AnsiStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStream_O:
{
    core::FileStream_O* obj_gc_safe = reinterpret_cast<core::FileStream_O*>(client);
    GC<core::FileStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOStreamStream_O:
{
    core::IOStreamStream_O* obj_gc_safe = reinterpret_cast<core::IOStreamStream_O*>(client);
    GC<core::IOStreamStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOFileStream_O:
{
    core::IOFileStream_O* obj_gc_safe = reinterpret_cast<core::IOFileStream_O*>(client);
    GC<core::IOFileStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
    core::ConcatenatedStream_O* obj_gc_safe = reinterpret_cast<core::ConcatenatedStream_O*>(client);
    GC<core::ConcatenatedStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringStream_O:
{
    core::StringStream_O* obj_gc_safe = reinterpret_cast<core::StringStream_O*>(client);
    GC<core::StringStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringInputStream_O:
{
    core::StringInputStream_O* obj_gc_safe = reinterpret_cast<core::StringInputStream_O*>(client);
    GC<core::StringInputStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringOutputStream_O:
{
    core::StringOutputStream_O* obj_gc_safe = reinterpret_cast<core::StringOutputStream_O*>(client);
    GC<core::StringOutputStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SynonymStream_O:
{
    core::SynonymStream_O* obj_gc_safe = reinterpret_cast<core::SynonymStream_O*>(client);
    GC<core::SynonymStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__EchoStream_O:
{
    core::EchoStream_O* obj_gc_safe = reinterpret_cast<core::EchoStream_O*>(client);
    GC<core::EchoStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TwoWayStream_O:
{
    core::TwoWayStream_O* obj_gc_safe = reinterpret_cast<core::TwoWayStream_O*>(client);
    GC<core::TwoWayStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BroadcastStream_O:
{
    core::BroadcastStream_O* obj_gc_safe = reinterpret_cast<core::BroadcastStream_O*>(client);
    GC<core::BroadcastStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Reader_O:
{
    core::Reader_O* obj_gc_safe = reinterpret_cast<core::Reader_O*>(client);
    GC<core::Reader_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SharpEqualWrapper_O:
{
    core::SharpEqualWrapper_O* obj_gc_safe = reinterpret_cast<core::SharpEqualWrapper_O*>(client);
    GC<core::SharpEqualWrapper_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O:
{
    asttooling::RegMap::RegistryMaps_O* obj_gc_safe = reinterpret_cast<asttooling::RegMap::RegistryMaps_O*>(client);
    GC<asttooling::RegMap::RegistryMaps_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O:
{
    core::Archive_O* obj_gc_safe = reinterpret_cast<core::Archive_O*>(client);
    GC<core::Archive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O:
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    GC<core::SaveArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
    core::SexpSaveArchive_O* obj_gc_safe = reinterpret_cast<core::SexpSaveArchive_O*>(client);
    GC<core::SexpSaveArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    GC<core::LoadArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
    core::SexpLoadArchive_O* obj_gc_safe = reinterpret_cast<core::SexpLoadArchive_O*>(client);
    GC<core::SexpLoadArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTable_O:
{
    core::HashTable_O* obj_gc_safe = reinterpret_cast<core::HashTable_O*>(client);
    GC<core::HashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEq_O:
{
    core::HashTableEq_O* obj_gc_safe = reinterpret_cast<core::HashTableEq_O*>(client);
    GC<core::HashTableEq_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    core::HashTableEqualp_O* obj_gc_safe = reinterpret_cast<core::HashTableEqualp_O*>(client);
    GC<core::HashTableEqualp_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEql_O:
{
    core::HashTableEql_O* obj_gc_safe = reinterpret_cast<core::HashTableEql_O*>(client);
    GC<core::HashTableEql_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O:
{
    core::HashTableEqual_O* obj_gc_safe = reinterpret_cast<core::HashTableEqual_O*>(client);
    GC<core::HashTableEqual_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__Creator_O:
{
    core::Creator_O* obj_gc_safe = reinterpret_cast<core::Creator_O*>(client);
    GC<core::Creator_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__DummyCreator_O:
{
    clbind::DummyCreator_O* obj_gc_safe = reinterpret_cast<clbind::DummyCreator_O*>(client);
    GC<clbind::DummyCreator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O:
{
    clbind::ConstructorCreator_O* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator_O*>(client);
    GC<clbind::ConstructorCreator_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InstanceCreator_O:
{
    core::InstanceCreator_O* obj_gc_safe = reinterpret_cast<core::InstanceCreator_O*>(client);
    GC<core::InstanceCreator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_cffi__Pointer_O:
{
    cffi::Pointer_O* obj_gc_safe = reinterpret_cast<cffi::Pointer_O*>(client);
    GC<cffi::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    GC<core::CxxObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
    core::WeakKeyMapping_O* obj_gc_safe = reinterpret_cast<core::WeakKeyMapping_O*>(client);
    GC<core::WeakKeyMapping_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cache_O:
{
    core::Cache_O* obj_gc_safe = reinterpret_cast<core::Cache_O*>(client);
    GC<core::Cache_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    core::LambdaListHandler_O* obj_gc_safe = reinterpret_cast<core::LambdaListHandler_O*>(client);
    GC<core::LambdaListHandler_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
    llvmo::InsertPoint_O* obj_gc_safe = reinterpret_cast<llvmo::InsertPoint_O*>(client);
    GC<llvmo::InsertPoint_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    GC<core::SourceFileInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SNode_O:
{
    core::SNode_O* obj_gc_safe = reinterpret_cast<core::SNode_O*>(client);
    GC<core::SNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LeafSNode_O:
{
    core::LeafSNode_O* obj_gc_safe = reinterpret_cast<core::LeafSNode_O*>(client);
    GC<core::LeafSNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BranchSNode_O:
{
    core::BranchSNode_O* obj_gc_safe = reinterpret_cast<core::BranchSNode_O*>(client);
    GC<core::BranchSNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    GC<core::Path_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
    asttooling::AstVisitor_O* obj_gc_safe = reinterpret_cast<asttooling::AstVisitor_O*>(client);
    GC<asttooling::AstVisitor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    llvmo::AttributeSet_O* obj_gc_safe = reinterpret_cast<llvmo::AttributeSet_O*>(client);
    GC<llvmo::AttributeSet_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StructureObject_O:
{
    core::StructureObject_O* obj_gc_safe = reinterpret_cast<core::StructureObject_O*>(client);
    GC<core::StructureObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    core::InvocationHistoryFrameIterator_O* obj_gc_safe = reinterpret_cast<core::InvocationHistoryFrameIterator_O*>(client);
    GC<core::InvocationHistoryFrameIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Package_O:
{
    core::Package_O* obj_gc_safe = reinterpret_cast<core::Package_O*>(client);
    GC<core::Package_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    GC<core::DirectoryEntry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Character_dummy_O:
{
    core::Character_dummy_O* obj_gc_safe = reinterpret_cast<core::Character_dummy_O*>(client);
    GC<core::Character_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    GC<core::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    GC<core::CompiledFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
    core::SingleDispatchGenericFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunction_O*>(client);
    GC<core::SingleDispatchGenericFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SpecialForm_O:
{
    core::SpecialForm_O* obj_gc_safe = reinterpret_cast<core::SpecialForm_O*>(client);
    GC<core::SpecialForm_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
    core::SingleDispatchEffectiveMethodFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchEffectiveMethodFunction_O*>(client);
    GC<core::SingleDispatchEffectiveMethodFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    GC<core::Instance_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    GC<core::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    clbind::ClassRegistry_O* obj_gc_safe = reinterpret_cast<clbind::ClassRegistry_O*>(client);
    GC<clbind::ClassRegistry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
    llvmo::DebugInfo_O* obj_gc_safe = reinterpret_cast<llvmo::DebugInfo_O*>(client);
    GC<llvmo::DebugInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
    llvmo::DIDerivedType_O* obj_gc_safe = reinterpret_cast<llvmo::DIDerivedType_O*>(client);
    GC<llvmo::DIDerivedType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIArray_O:
{
    llvmo::DIArray_O* obj_gc_safe = reinterpret_cast<llvmo::DIArray_O*>(client);
    GC<llvmo::DIArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
    llvmo::DIBasicType_O* obj_gc_safe = reinterpret_cast<llvmo::DIBasicType_O*>(client);
    GC<llvmo::DIBasicType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
    llvmo::DISubprogram_O* obj_gc_safe = reinterpret_cast<llvmo::DISubprogram_O*>(client);
    GC<llvmo::DISubprogram_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
    llvmo::DILexicalBlock_O* obj_gc_safe = reinterpret_cast<llvmo::DILexicalBlock_O*>(client);
    GC<llvmo::DILexicalBlock_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
    llvmo::DICompileUnit_O* obj_gc_safe = reinterpret_cast<llvmo::DICompileUnit_O*>(client);
    GC<llvmo::DICompileUnit_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
    llvmo::DIDescriptor_O* obj_gc_safe = reinterpret_cast<llvmo::DIDescriptor_O*>(client);
    GC<llvmo::DIDescriptor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIType_O:
{
    llvmo::DIType_O* obj_gc_safe = reinterpret_cast<llvmo::DIType_O*>(client);
    GC<llvmo::DIType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
    llvmo::DISubroutineType_O* obj_gc_safe = reinterpret_cast<llvmo::DISubroutineType_O*>(client);
    GC<llvmo::DISubroutineType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
    llvmo::DICompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::DICompositeType_O*>(client);
    GC<llvmo::DICompositeType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
    llvmo::DITypeArray_O* obj_gc_safe = reinterpret_cast<llvmo::DITypeArray_O*>(client);
    GC<llvmo::DITypeArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIFile_O:
{
    llvmo::DIFile_O* obj_gc_safe = reinterpret_cast<llvmo::DIFile_O*>(client);
    GC<llvmo::DIFile_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIScope_O:
{
    llvmo::DIScope_O* obj_gc_safe = reinterpret_cast<llvmo::DIScope_O*>(client);
    GC<llvmo::DIScope_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMultimap_O:
{
    core::SmallMultimap_O* obj_gc_safe = reinterpret_cast<core::SmallMultimap_O*>(client);
    GC<core::SmallMultimap_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    GC<core::Pathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    GC<core::LogicalPathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTime_O:
{
    core::PosixTime_O* obj_gc_safe = reinterpret_cast<core::PosixTime_O*>(client);
    GC<core::PosixTime_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMap_O:
{
    core::SmallMap_O* obj_gc_safe = reinterpret_cast<core::SmallMap_O*>(client);
    GC<core::SmallMap_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
    core::Lisp_O* obj_gc_safe = reinterpret_cast<core::Lisp_O*>(client);
    GC<core::Lisp_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CoreExposer_O:
{
    core::CoreExposer_O* obj_gc_safe = reinterpret_cast<core::CoreExposer_O*>(client);
    GC<core::CoreExposer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::KeywordArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never deallocate gcstrings gctools::GCString_moveable<char>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::RequiredArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O:
{
    asttooling::internal::VariadicOperatorMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::VariadicOperatorMatcherDescriptor_O*>(client);
    GC<asttooling::internal::VariadicOperatorMatcherDescriptor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolStorage>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ContextFrame>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::T_O *>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::AuxArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ParserValue>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O:
{
    asttooling::internal::FreeFuncMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::FreeFuncMatcherDescriptor_O*>(client);
    GC<asttooling::internal::FreeFuncMatcherDescriptor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O:
{
    asttooling::internal::FixedArgCountMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::FixedArgCountMatcherDescriptor_O*>(client);
    GC<asttooling::internal::FixedArgCountMatcherDescriptor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    asttooling::DerivableFrontendActionFactory* obj_gc_safe = reinterpret_cast<asttooling::DerivableFrontendActionFactory*>(client);
    GC<asttooling::DerivableFrontendActionFactory>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    asttooling::DerivableMatchCallback* obj_gc_safe = reinterpret_cast<asttooling::DerivableMatchCallback*>(client);
    GC<asttooling::DerivableMatchCallback>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O:
{
    asttooling::internal::OverloadedMatcherDescriptor_O* obj_gc_safe = reinterpret_cast<asttooling::internal::OverloadedMatcherDescriptor_O*>(client);
    GC<asttooling::internal::OverloadedMatcherDescriptor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ErrorContent>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    asttooling::DerivableASTFrontendAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableASTFrontendAction*>(client);
    GC<asttooling::DerivableASTFrontendAction>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::Message>"));}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    asttooling::DerivableSyntaxOnlyAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableSyntaxOnlyAction*>(client);
    GC<asttooling::DerivableSyntaxOnlyAction>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::OptionalArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>"));}
#endif // defined(GC_OBJ_DEALLOCATOR)
#if defined(GC_OBJ_DEALLOCATOR_HELPERS)

#endif // defined(GC_OBJ_DEALLOCATOR_HELPERS)
#if defined(GC_OBJ_DEALLOCATOR_TABLE)
static void* OBJ_DEALLOCATOR_table[] = { 
  /* 5 */ &&obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_clbind__detail__class_map,
  /* 6 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__T_O,
  /* 7 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VaList_dummy_O,
  /* 8 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O,
  /* 9 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O,
  /* 10 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MultiStringBuffer_O,
  /* 11 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ReadTable_O,
  /* 12 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O,
  /* 13 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O,
  /* 14 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O,
  /* 15 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O,
  /* 16 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O,
  /* 17 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O,
  /* 18 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 19 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O,
  /* 20 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O,
  /* 21 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 22 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O,
  /* 23 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 24 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O,
  /* 25 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O,
  /* 26 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 27 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 28 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O,
  /* 29 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ActivationFrame_O,
  /* 30 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 31 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O,
  /* 32 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 33 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 34 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 35 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 36 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 37 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 38 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 39 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 40 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 41 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 42 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 43 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 44 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 45 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 46 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O,
  /* 47 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O,
  /* 48 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayObjects_O,
  /* 49 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayDisplaced_O,
  /* 50 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O,
  /* 51 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVector_O,
  /* 52 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SimpleBitVector_O,
  /* 53 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVectorWithFillPtr_O,
  /* 54 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorDisplaced_O,
  /* 55 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O,
  /* 56 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O,
  /* 57 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StrWithFillPtr_O,
  /* 58 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O,
  /* 59 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O,
  /* 60 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchMethod_O,
  /* 61 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RandomState_O,
  /* 62 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
  /* 63 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SequenceStepper_O,
  /* 64 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ConsStepper_O,
  /* 65 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorStepper_O,
  /* 66 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugLoc_O,
  /* 67 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Attribute_O,
  /* 68 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RegexMatch_O,
  /* 69 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakPointer_O,
  /* 70 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 71 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 72 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O,
  /* 73 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O,
  /* 74 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StdClass_O,
  /* 75 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardClass_O,
  /* 76 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FuncallableStandardClass_O,
  /* 77 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StructureClass_O,
  /* 78 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForwardReferencedClass_O,
  /* 79 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxClass_O,
  /* 80 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__BuiltInClass_O,
  /* 81 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRep_O,
  /* 82 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ExternalObject_O,
  /* 83 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Value_O,
  /* 84 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Argument_O,
  /* 85 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__User_O,
  /* 86 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Instruction_O,
  /* 87 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicRMWInst_O,
  /* 88 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LandingPadInst_O,
  /* 89 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PHINode_O,
  /* 90 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CallInst_O,
  /* 91 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StoreInst_O,
  /* 92 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnaryInstruction_O,
  /* 93 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LoadInst_O,
  /* 94 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AllocaInst_O,
  /* 95 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VAArgInst_O,
  /* 96 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O,
  /* 97 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TerminatorInst_O,
  /* 98 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnreachableInst_O,
  /* 99 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SwitchInst_O,
  /* 100 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ReturnInst_O,
  /* 101 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ResumeInst_O,
  /* 102 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BranchInst_O,
  /* 103 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InvokeInst_O,
  /* 104 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IndirectBrInst_O,
  /* 105 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FenceInst_O,
  /* 106 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Constant_O,
  /* 107 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BlockAddress_O,
  /* 108 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalValue_O,
  /* 109 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalVariable_O,
  /* 110 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Function_O,
  /* 111 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantArray_O,
  /* 112 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantInt_O,
  /* 113 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataSequential_O,
  /* 114 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataArray_O,
  /* 115 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantStruct_O,
  /* 116 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantFP_O,
  /* 117 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UndefValue_O,
  /* 118 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantPointerNull_O,
  /* 119 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantExpr_O,
  /* 120 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BasicBlock_O,
  /* 121 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilderBase_O,
  /* 122 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilder_O,
  /* 123 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBuilder_O,
  /* 124 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Metadata_O,
  /* 125 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ValueAsMetadata_O,
  /* 126 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDNode_O,
  /* 127 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDString_O,
  /* 128 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ExecutionEngine_O,
  /* 129 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APFloat_O,
  /* 130 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBuilder_O,
  /* 131 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayout_O,
  /* 132 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Triple_O,
  /* 133 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APInt_O,
  /* 134 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBase_O,
  /* 135 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPassManager_O,
  /* 136 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManager_O,
  /* 137 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetMachine_O,
  /* 138 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O,
  /* 139 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetOptions_O,
  /* 140 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Type_O,
  /* 141 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IntegerType_O,
  /* 142 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CompositeType_O,
  /* 143 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SequentialType_O,
  /* 144 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VectorType_O,
  /* 145 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PointerType_O,
  /* 146 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ArrayType_O,
  /* 147 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StructType_O,
  /* 148 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionType_O,
  /* 149 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__NamedMDNode_O,
  /* 150 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Linker_O,
  /* 151 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Pass_O,
  /* 152 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPass_O,
  /* 153 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ModulePass_O,
  /* 154 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ImmutablePass_O,
  /* 155 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayoutPass_O,
  /* 156 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O,
  /* 157 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O,
  /* 158 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O,
  /* 159 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Module_O,
  /* 160 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__EngineBuilder_O,
  /* 161 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForeignData_O,
  /* 162 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMContext_O,
  /* 163 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Target_O,
  /* 164 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadTimeValues_O,
  /* 165 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Binder_O,
  /* 166 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IntArray_O,
  /* 167 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O,
  /* 168 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O,
  /* 169 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Functor_O,
  /* 170 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Closure_O,
  /* 171 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionClosure_O,
  /* 172 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InstanceClosure_O,
  /* 173 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledClosure_O,
  /* 174 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O,
  /* 175 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroClosure_O,
  /* 176 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O,
  /* 177 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InterpretedClosure_O,
  /* 178 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LightUserData_O,
  /* 179 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UserData_O,
  /* 180 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O,
  /* 181 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Null_O,
  /* 182 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 183 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__Iterator_O,
  /* 184 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O,
  /* 185 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O,
  /* 186 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Regex_O,
  /* 187 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTimeDuration_O,
  /* 188 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 189 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O,
  /* 190 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Stream_O,
  /* 191 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__AnsiStream_O,
  /* 192 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStream_O,
  /* 193 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOStreamStream_O,
  /* 194 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOFileStream_O,
  /* 195 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ConcatenatedStream_O,
  /* 196 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringStream_O,
  /* 197 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringInputStream_O,
  /* 198 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringOutputStream_O,
  /* 199 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SynonymStream_O,
  /* 200 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__EchoStream_O,
  /* 201 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TwoWayStream_O,
  /* 202 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BroadcastStream_O,
  /* 203 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Reader_O,
  /* 204 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SharpEqualWrapper_O,
  /* 205 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O,
  /* 206 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O,
  /* 207 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O,
  /* 208 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpSaveArchive_O,
  /* 209 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O,
  /* 210 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpLoadArchive_O,
  /* 211 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTable_O,
  /* 212 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEq_O,
  /* 213 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqualp_O,
  /* 214 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEql_O,
  /* 215 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O,
  /* 216 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__Creator_O,
  /* 217 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__DummyCreator_O,
  /* 218 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O,
  /* 219 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InstanceCreator_O,
  /* 220 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_cffi__Pointer_O,
  /* 221 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O,
  /* 222 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyMapping_O,
  /* 223 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cache_O,
  /* 224 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LambdaListHandler_O,
  /* 225 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InsertPoint_O,
  /* 226 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 227 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SNode_O,
  /* 228 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LeafSNode_O,
  /* 229 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BranchSNode_O,
  /* 230 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O,
  /* 231 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__AstVisitor_O,
  /* 232 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AttributeSet_O,
  /* 233 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StructureObject_O,
  /* 234 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O,
  /* 235 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Package_O,
  /* 236 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O,
  /* 237 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Character_dummy_O,
  /* 238 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O,
  /* 239 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 240 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O,
  /* 241 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SpecialForm_O,
  /* 242 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O,
  /* 243 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O,
  /* 244 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O,
  /* 245 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRegistry_O,
  /* 246 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugInfo_O,
  /* 247 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDerivedType_O,
  /* 248 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIArray_O,
  /* 249 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBasicType_O,
  /* 250 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubprogram_O,
  /* 251 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DILexicalBlock_O,
  /* 252 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompileUnit_O,
  /* 253 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDescriptor_O,
  /* 254 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIType_O,
  /* 255 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubroutineType_O,
  /* 256 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompositeType_O,
  /* 257 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DITypeArray_O,
  /* 258 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIFile_O,
  /* 259 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIScope_O,
  /* 260 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMultimap_O,
  /* 261 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O,
  /* 262 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O,
  /* 263 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTime_O,
  /* 264 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMap_O,
  /* 265 */ &&obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_core__Lisp_O,
  /* 266 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CoreExposer_O,
  /* 267 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_,
  /* 268 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 269 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
  /* 270 */ &&obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 271 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_,
  /* 272 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_,
  /* 273 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 274 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O,
  /* 275 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___,
  /* 276 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_,
  /* 277 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_,
  /* 278 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_,
  /* 279 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__,
  /* 280 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_,
  /* 281 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_,
  /* 282 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O,
  /* 283 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O,
  /* 284 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 285 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 286 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___,
  /* 287 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 288 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__,
  /* 289 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory,
  /* 290 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_,
  /* 291 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableMatchCallback,
  /* 292 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O,
  /* 293 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_,
  /* 294 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction,
  /* 295 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_,
  /* 296 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_,
  /* 297 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 298 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction,
  /* 299 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__,
  /* 300 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_,
  /* 301 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__,
  /* 302 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 303 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 304 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 305 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__,
   NULL
};
#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)
#if defined(GC_GLOBALS)
 SMART_PTR_FIX(llvmo::Function_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantExpr_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::NamedMDNode_O::static_class);
 SMART_PTR_FIX(llvmo::PointerType_O::static_class_symbol);
 SMART_PTR_FIX(core::WrappedPointer_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LandingPadInst_O::static_creator);
 SMART_PTR_FIX(core::SequenceStepper_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::MCSubtargetInfo_O::static_creator);
 SMART_PTR_FIX(core::SynonymStream_O::static_class);
 SMART_PTR_FIX(llvmo::MDString_O::static_class);
 SMART_PTR_FIX(llvmo::Attribute_O::static_class);
 SMART_PTR_FIX(llvmo::Target_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::GlobalVariable_O::static_creator);
 SMART_PTR_FIX(core::FunctionValueEnvironment_O::static_creator);
 SMART_PTR_FIX(core::TagbodyFrame_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::InvokeInst_O::static_class);
 SMART_PTR_FIX(llvmo::Triple_O::static_class);
 SMART_PTR_FIX(llvmo::FunctionType_O::static_class);
 SMART_PTR_FIX(core::FileStream_O::static_creator);
 SMART_PTR_FIX(llvmo::DICompileUnit_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantInt_O::static_creator);
 SMART_PTR_FIX(core::SymbolMacroletEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::SexpSaveArchive_O::static_creator);
 SMART_PTR_FIX(core::StructureClass_O::static_class);
 SMART_PTR_FIX(llvmo::BlockAddress_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantExpr_O::static_creator);
 SMART_PTR_FIX(llvmo::ResumeInst_O::static_class_symbol);
 SMART_PTR_FIX(core::WeakPointer_O::static_creator);
 SMART_PTR_FIX(llvmo::StructType_O::static_class);
 SMART_PTR_FIX(llvmo::DataLayout_O::static_class);
 SMART_PTR_FIX(core::CatchEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::Str_O::static_class_symbol);
 SMART_PTR_FIX(core::LeafSNode_O::static_class_symbol);
 SMART_PTR_FIX(core::Str_O::static_class);
 SMART_PTR_FIX(llvmo::UnaryInstruction_O::static_class);
 SMART_PTR_FIX(llvmo::FunctionPassManager_O::static_class);
 SMART_PTR_FIX(llvmo::CompositeType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IntegerType_O::static_creator);
 SMART_PTR_FIX(core::VectorObjectsWithFillPtr_O::static_class);
 SMART_PTR_FIX(core::StandardClass_O::static_class_symbol);
 SMART_PTR_FIX(core::Closure_O::static_class);
 SMART_PTR_FIX(core::FunctionContainerEnvironment_O::static_creator);
 SMART_PTR_FIX(core::SymbolToEnumConverter_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantInt_O::static_class);
 SMART_PTR_FIX(llvmo::IRBuilderBase_O::static_class);
 SMART_PTR_FIX(core::Environment_O::static_class_symbol);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_unbound);
 SMART_PTR_FIX(llvmo::Instruction_O::static_class);
 SMART_PTR_FIX(core::LogicalPathname_O::static_class);
 SMART_PTR_FIX(llvmo::PassManager_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::MDString_O::static_creator);
 SMART_PTR_FIX(core::FuncallableStandardClass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::VectorType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ValueAsMetadata_O::static_creator);
 SMART_PTR_FIX(core::VectorDisplaced_O::static_class);
 SMART_PTR_FIX(llvmo::ImmutablePass_O::static_class_symbol);
 SMART_PTR_FIX(core::BitVector_O::static_creator);
 SMART_PTR_FIX(llvmo::AttributeSet_O::static_creator);
 SMART_PTR_FIX(core::Archive_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIDerivedType_O::static_class);
 SMART_PTR_FIX(core::BuiltInClass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DataLayoutPass_O::static_class_symbol);
 TAGGED_POINTER_FIX(clbind::globalClassMap);
 SMART_PTR_FIX(core::ExternalObject_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTableEql_O::static_class);
 SMART_PTR_FIX(llvmo::DILexicalBlock_O::static_class);
 SMART_PTR_FIX(llvmo::StoreInst_O::static_class);
 SMART_PTR_FIX(core::CandoException_O::static_creator);
 SMART_PTR_FIX(core::SingleDispatchMethod_O::static_class_symbol);
 SMART_PTR_FIX(core::BroadcastStream_O::static_class);
 SMART_PTR_FIX(llvmo::FunctionPass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LandingPadInst_O::static_class_symbol);
 SMART_PTR_FIX(core::BroadcastStream_O::static_creator);
 SMART_PTR_FIX(llvmo::DIType_O::static_creator);
 SMART_PTR_FIX(llvmo::TargetSubtargetInfo_O::static_class);
 SMART_PTR_FIX(core::String_O::static_class_symbol);
 SMART_PTR_FIX(core::UserData_O::static_class);
 SMART_PTR_FIX(core::Cons_O::static_creator);
 SMART_PTR_FIX(llvmo::AttributeSet_O::static_class);
 SMART_PTR_FIX(core::Environment_O::static_creator);
 SMART_PTR_FIX(llvmo::User_O::static_class_symbol);
 SMART_PTR_FIX(core::General_O::static_class);
 SMART_PTR_FIX(llvmo::LLVMContext_O::static_class);
 SMART_PTR_FIX(core::ArrayObjects_O::static_class);
 SMART_PTR_FIX(core::HashTableEq_O::static_class_symbol);
 SMART_PTR_FIX(core::SequenceStepper_O::static_class);
 SMART_PTR_FIX(llvmo::ReturnInst_O::static_creator);
 SMART_PTR_FIX(core::FileStream_O::static_class);
 SMART_PTR_FIX(core::FunctionFrame_O::static_class);
 SMART_PTR_FIX(core::ConsStepper_O::static_class);
 SMART_PTR_FIX(core::BuiltinClosure_O::static_creator);
 SMART_PTR_FIX(core::Cons_O::static_class_symbol);
 SMART_PTR_FIX(core::WeakKeyHashTable_O::static_class_symbol);
 SMART_PTR_FIX(core::TwoWayStream_O::static_class);
 SMART_PTR_FIX(core::WeakKeyHashTable_O::static_class);
 SMART_PTR_FIX(core::UserData_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::SequentialType_O::static_creator);
 SMART_PTR_FIX(core::HashTableEqual_O::static_class);
 SMART_PTR_FIX(core::ReadTable_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Target_O::static_class);
 SMART_PTR_FIX(llvmo::VectorType_O::static_class);
 SMART_PTR_FIX(asttooling::RegMap::RegistryData);
 SMART_PTR_FIX(llvmo::Function_O::static_creator);
 SMART_PTR_FIX(llvmo::FunctionPassManager_O::static_class_symbol);
 SMART_PTR_FIX(core::DoubleFloat_O::static_creator);
 SMART_PTR_FIX(core::FunctionContainerEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::FileStatus_O::static_class_symbol);
 SMART_PTR_FIX(core::DirectoryIterator_O::static_class);
 SMART_PTR_FIX(core::Record_O::static_class_symbol);
 SMART_PTR_FIX(core::StdClass_O::static_class_symbol);
 SMART_PTR_FIX(core::SingleFloat_dummy_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LLVMTargetMachine_O::static_creator);
 SMART_PTR_FIX(core::StringStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LLVMTargetMachine_O::static_class);
 SMART_PTR_FIX(core::Binder_O::static_class_symbol);
 SMART_PTR_FIX(core::VectorObjectsWithFillPtr_O::static_creator);
 SMART_PTR_FIX(core::Fixnum_dummy_O::static_class);
 SMART_PTR_FIX(llvmo::ArrayType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::AtomicCmpXchgInst_O::static_creator);
 SMART_PTR_FIX(core::BlockEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::InsertPoint_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIFile_O::static_class_symbol);
 SMART_PTR_FIX(core::Function_O::static_creator);
 SMART_PTR_FIX(core::Str_O::static_creator);
 SMART_PTR_FIX(llvmo::DebugInfo_O::static_class_symbol);
 SMART_PTR_FIX(core::FuncallableStandardClass_O::static_class);
 SMART_PTR_FIX(core::Float_O::static_class);
 SMART_PTR_FIX(core::Iterator_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DebugInfo_O::static_creator);
 SMART_PTR_FIX(llvmo::EngineBuilder_O::static_creator);
 SMART_PTR_FIX(core::PosixTime_O::static_class_symbol);
 SMART_PTR_FIX(core::SharpEqualWrapper_O::static_class_symbol);
 SMART_PTR_FIX(core::CompileTimeEnvironment_O::static_class);
 SMART_PTR_FIX(core::Symbol_O::static_creator);
 SMART_PTR_FIX(core::BuiltInClass_O::static_creator);
 SMART_PTR_FIX(llvmo::TargetOptions_O::static_creator);
 SMART_PTR_FIX(core::LogicalPathname_O::static_class_symbol);
 SMART_PTR_FIX(core::T_O::static_creator);
 SMART_PTR_FIX(core::IOStreamStream_O::static_class);
 SMART_PTR_FIX(core::String_O::static_class);
 SMART_PTR_FIX(clbind::ClassRep_O::static_creator);
 SMART_PTR_FIX(core::SourceManager_O::static_creator);
 SMART_PTR_FIX(core::Archive_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantDataSequential_O::static_class);
 SMART_PTR_FIX(core::Path_O::static_class_symbol);
 SMART_PTR_FIX(core::RandomState_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetOptions_O::static_class);
 SMART_PTR_FIX(llvmo::DISubprogram_O::static_class);
 SMART_PTR_FIX(core::Reader_O::static_class_symbol);
 SMART_PTR_FIX(core::Symbol_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantDataArray_O::static_creator);
 SMART_PTR_FIX(core::ForwardReferencedClass_O::static_creator);
 SMART_PTR_FIX(core::IOStreamStream_O::static_creator);
 SMART_PTR_FIX(core::InstanceClosure_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantFP_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::UnaryInstruction_O::static_creator);
 SMART_PTR_FIX(llvmo::UndefValue_O::static_class);
 SMART_PTR_FIX(cffi::Pointer_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DICompositeType_O::static_class_symbol);
 SMART_PTR_FIX(core::PosixTime_O::static_creator);
 SMART_PTR_FIX(core::CatchEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::InvokeInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Specializer_O::static_class_symbol);
 SMART_PTR_FIX(core::Reader_O::static_class);
 SMART_PTR_FIX(core::Vector_O::static_class_symbol);
 SMART_PTR_FIX(core::BuiltinClosure_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DataLayoutPass_O::static_class);
 SMART_PTR_FIX(llvmo::PHINode_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTable_O::static_class);
 SMART_PTR_FIX(core::FunctionClosure_O::static_creator);
 SMART_PTR_FIX(llvmo::MDNode_O::static_class);
 SMART_PTR_FIX(llvmo::CallInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DataLayout_O::static_creator);
 SMART_PTR_FIX(core::PosixTimeDuration_O::static_class_symbol);
 SMART_PTR_FIX(core::Package_O::static_creator);
 SMART_PTR_FIX(core::SpecialForm_O::static_class);
 SMART_PTR_FIX(core::AnsiStream_O::static_creator);
 SMART_PTR_FIX(core::Number_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LandingPadInst_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantDataSequential_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::APFloat_O::static_creator);
 SMART_PTR_FIX(core::Binder_O::static_class);
 SMART_PTR_FIX(core::DirectoryEntry_O::static_class_symbol);
 SMART_PTR_FIX(core::Vector_O::static_class);
 SMART_PTR_FIX(core::SingleDispatchGenericFunction_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IRBuilder_O::static_class);
 SMART_PTR_FIX(core::LexicalEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTableEqualp_O::static_creator);
 SMART_PTR_FIX(llvmo::CompositeType_O::static_class);
 SMART_PTR_FIX(cffi::Pointer_O::static_class);
 SMART_PTR_FIX(llvmo::Attribute_O::static_class_symbol);
 SMART_PTR_FIX(core::Pointer_O::static_creator);
 SMART_PTR_FIX(core::HashTableEqualp_O::static_class_symbol);
 SMART_PTR_FIX(core::Integer_O::static_class);
 SMART_PTR_FIX(llvmo::ExecutionEngine_O::static_class_symbol);
 SMART_PTR_FIX(core::SaveArchive_O::static_class_symbol);
 SMART_PTR_FIX(core::AnsiStream_O::static_class_symbol);
 SMART_PTR_FIX(core::SharpEqualWrapper_O::static_class);
 SMART_PTR_FIX(core::CompiledFunction_O::static_creator);
 SMART_PTR_FIX(core::Array_O::static_class);
 SMART_PTR_FIX(core::LoadTimeValues_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IndirectBrInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetSubtargetInfo_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Constant_O::static_creator);
 SMART_PTR_FIX(core::RegexMatch_O::static_creator);
 SMART_PTR_FIX(core::LightUserData_O::static_creator);
 SMART_PTR_FIX(core::LeafSNode_O::static_creator);
 SMART_PTR_FIX(llvmo::DISubroutineType_O::static_creator);
 SMART_PTR_FIX(llvmo::AllocaInst_O::static_class);
 SMART_PTR_FIX(llvmo::UndefValue_O::static_class_symbol);
 SMART_PTR_FIX(core::WeakHashTable_O::static_class_symbol);
 SMART_PTR_FIX(core::Array_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIDerivedType_O::static_creator);
 SMART_PTR_FIX(core::SexpLoadArchive_O::static_class);
 SMART_PTR_FIX(llvmo::GlobalValue_O::static_creator);
 SMART_PTR_FIX(llvmo::ExecutionEngine_O::static_creator);
 SMART_PTR_FIX(llvmo::Value_O::static_class_symbol);
 SMART_PTR_FIX(core::SingleDispatchEffectiveMethodFunction_O::static_class_symbol);
 SMART_PTR_FIX(core::Null_O::static_class);
 SMART_PTR_FIX(core::SpecialForm_O::static_creator);
 SMART_PTR_FIX(core::FunctionValueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DITypeArray_O::static_class_symbol);
 SMART_PTR_FIX(core::SmallMultimap_O::static_class);
 SMART_PTR_FIX(core::IntArray_O::static_class);
 SMART_PTR_FIX(core::ConcatenatedStream_O::static_class);
 SMART_PTR_FIX(core::ConcatenatedStream_O::static_creator);
 SMART_PTR_FIX(core::LeafSNode_O::static_class);
 SMART_PTR_FIX(core::WrappedPointer_O::static_class);
 SMART_PTR_FIX(core::SingleFloat_dummy_O::static_creator);
 SMART_PTR_FIX(core::LambdaListHandler_O::static_class_symbol);
 SMART_PTR_FIX(core::StrWithFillPtr_O::static_class_symbol);
 SMART_PTR_FIX(core::ConsStepper_O::static_creator);
 SMART_PTR_FIX(core::SaveArchive_O::static_class);
 SMART_PTR_FIX(llvmo::Argument_O::static_creator);
 SMART_PTR_FIX(llvmo::Type_O::static_class);
 SMART_PTR_FIX(llvmo::DIFile_O::static_class);
 SMART_PTR_FIX(core::MultiStringBuffer_O::static_creator);
 SMART_PTR_FIX(core::ArrayDisplaced_O::static_class);
 SMART_PTR_FIX(core::StrWithFillPtr_O::static_creator);
 SMART_PTR_FIX(llvmo::VAArgInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::AttributeSet_O::static_class_symbol);
 SMART_PTR_FIX(core::ArrayDisplaced_O::static_creator);
 SMART_PTR_FIX(core::ValueFrame_O::static_creator);
 SMART_PTR_FIX(core::ActivationFrame_O::static_class);
 SMART_PTR_FIX(llvmo::PassManagerBuilder_O::static_class);
 SMART_PTR_FIX(core::BitVector_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LLVMContext_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::VAArgInst_O::static_creator);
 SMART_PTR_FIX(core::CompiledClosure_O::static_class);
 SMART_PTR_FIX(core::PosixTimeDuration_O::static_creator);
 SMART_PTR_FIX(core::RuntimeVisibleEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::Complex_O::static_creator);
 SMART_PTR_FIX(llvmo::APFloat_O::static_class);
 SMART_PTR_FIX(core::StackValueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::CatchEnvironment_O::static_class);
 SMART_PTR_FIX(core::Specializer_O::static_class);
 SMART_PTR_FIX(core::SmallMultimap_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::SequentialType_O::static_class_symbol);
 SMART_PTR_FIX(core::ReadTable_O::static_creator);
 SMART_PTR_FIX(llvmo::DICompositeType_O::static_creator);
 SMART_PTR_FIX(llvmo::AtomicCmpXchgInst_O::static_class);
 SMART_PTR_FIX(core::StringOutputStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::FenceInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetSubtargetInfo_O::static_creator);
 SMART_PTR_FIX(core::PosixTimeDuration_O::static_class);
 SMART_PTR_FIX(core::VectorObjectsWithFillPtr_O::static_class_symbol);
 SMART_PTR_FIX(core::BranchSNode_O::static_creator);
 SMART_PTR_FIX(core::General_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Pass_O::static_creator);
 SMART_PTR_FIX(core::SymbolMacroletEnvironment_O::static_class);
 SMART_PTR_FIX(core::BuiltInClass_O::static_class);
 SMART_PTR_FIX(core::UnwindProtectEnvironment_O::static_creator);
 SMART_PTR_FIX(core::AnsiStream_O::static_class);
 SMART_PTR_FIX(core::HashTableEq_O::static_class);
 SMART_PTR_FIX(llvmo::User_O::static_class);
 SMART_PTR_FIX(llvmo::Argument_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantPointerNull_O::static_class);
 SMART_PTR_FIX(core::CxxObject_O::static_class);
 SMART_PTR_FIX(core::Metaobject_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::FunctionPass_O::static_class);
 SMART_PTR_FIX(llvmo::InsertPoint_O::static_class);
 SMART_PTR_FIX(core::FileStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Linker_O::static_class_symbol);
 SMART_PTR_FIX(core::Stream_O::static_class_symbol);
 SMART_PTR_FIX(core::Character_dummy_O::static_class_symbol);
 SMART_PTR_FIX(core::StringOutputStream_O::static_creator);
 SMART_PTR_FIX(core::ConsStepper_O::static_class_symbol);
 SMART_PTR_FIX(core::MacroletEnvironment_O::static_class);
 SMART_PTR_FIX(core::LexicalEnvironment_O::static_class);
 SMART_PTR_FIX(core::HashTableEql_O::static_creator);
 SMART_PTR_FIX(core::WeakHashTable_O::static_class);
 SMART_PTR_FIX(llvmo::PassManagerBase_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIBasicType_O::static_class);
 SMART_PTR_FIX(clbind::ClassRep_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetLibraryInfo_O::static_creator);
 SMART_PTR_FIX(core::HashTableEq_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantPointerNull_O::static_creator);
 SMART_PTR_FIX(llvmo::DIDescriptor_O::static_class);
 SMART_PTR_FIX(core::Environment_O::static_class);
 SMART_PTR_FIX(llvmo::APFloat_O::static_class_symbol);
 SMART_PTR_FIX(core::SexpLoadArchive_O::static_class_symbol);
 SMART_PTR_FIX(core::CompiledClosure_O::static_creator);
 SMART_PTR_FIX(llvmo::FunctionType_O::static_class_symbol);
 SMART_PTR_FIX(core::SimpleBitVector_O::static_class);
 SMART_PTR_FIX(llvmo::Metadata_O::static_class);
 SMART_PTR_FIX(core::RecursiveDirectoryIterator_O::static_creator);
 SMART_PTR_FIX(llvmo::LoadInst_O::static_creator);
 SMART_PTR_FIX(core::HashTableEqual_O::static_class_symbol);
 SMART_PTR_FIX(core::WeakKeyMapping_O::static_creator);
 SMART_PTR_FIX(llvmo::BlockAddress_O::static_class);
 SMART_PTR_FIX(core::InstanceClosure_O::static_class);
 SMART_PTR_FIX(core::SmallMultimap_O::static_creator);
 SMART_PTR_FIX(core::TagbodyFrame_O::static_class);
 SMART_PTR_FIX(llvmo::FenceInst_O::static_class);
 SMART_PTR_FIX(core::WeakPointer_O::static_class_symbol);
 SMART_PTR_FIX(core::Real_O::static_class_symbol);
 SMART_PTR_FIX(core::BitVectorWithFillPtr_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantStruct_O::static_creator);
 SMART_PTR_FIX(core::RecursiveDirectoryIterator_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::CallInst_O::static_creator);
 SMART_PTR_FIX(llvmo::AllocaInst_O::static_creator);
 SMART_PTR_FIX(core::FuncallableStandardClass_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantDataArray_O::static_class);
 SMART_PTR_FIX(llvmo::UnaryInstruction_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DILexicalBlock_O::static_creator);
 SMART_PTR_FIX(clbind::ClassRegistry_O::static_class);
 SMART_PTR_FIX(llvmo::CompositeType_O::static_creator);
 SMART_PTR_FIX(llvmo::IRBuilderBase_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::StoreInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Ratio_O::static_class_symbol);
 SMART_PTR_FIX(core::ValueFrame_O::static_class_symbol);
 SMART_PTR_FIX(core::TagbodyEnvironment_O::static_class);
 SMART_PTR_FIX(core::UnwindProtectEnvironment_O::static_class);
 SMART_PTR_FIX(core::LambdaListHandler_O::static_creator);
 SMART_PTR_FIX(llvmo::DIScope_O::static_creator);
 SMART_PTR_FIX(core::Complex_O::static_class_symbol);
 SMART_PTR_FIX(core::Pathname_O::static_creator);
 SMART_PTR_FIX(llvmo::DIArray_O::static_class);
 SMART_PTR_FIX(core::T_O::static_class_symbol);
 SMART_PTR_FIX(core::DoubleFloat_O::static_class);
 SMART_PTR_FIX(core::Pathname_O::static_class);
 SMART_PTR_FIX(llvmo::ResumeInst_O::static_creator);
 SMART_PTR_FIX(core::Specializer_O::static_creator);
 SMART_PTR_FIX(core::LongFloat_O::static_class_symbol);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_sameAsKey);
 SMART_PTR_FIX(core::Reader_O::static_creator);
 SMART_PTR_FIX(core::VectorObjects_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::AllocaInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::StoreInst_O::static_creator);
 SMART_PTR_FIX(llvmo::Constant_O::static_class);
 SMART_PTR_FIX(llvmo::Pass_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantArray_O::static_creator);
 TAGGED_POINTER_FIX(_lisp);
 SMART_PTR_FIX(llvmo::MDNode_O::static_class_symbol);
 SMART_PTR_FIX(core::LambdaListHandler_O::static_class);
 SMART_PTR_FIX(core::LoadArchive_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantExpr_O::static_class);
 SMART_PTR_FIX(llvmo::Instruction_O::static_class_symbol);
 SMART_PTR_FIX(core::FunctionValueEnvironment_O::static_class);
 SMART_PTR_FIX(core::Pointer_O::static_class_symbol);
 SMART_PTR_FIX(core::WrappedPointer_O::static_creator);
 SMART_PTR_FIX(core::Function_O::static_class_symbol);
 SMART_PTR_FIX(core::BranchSNode_O::static_class_symbol);
 SMART_PTR_FIX(core::T_O::static_class);
 SMART_PTR_FIX(core::LoadArchive_O::static_creator);
 SMART_PTR_FIX(core::CandoException_O::static_class_symbol);
 SMART_PTR_FIX(core::Creator_O::static_creator);
 SMART_PTR_FIX(core::InstanceCreator_O::static_creator);
 SMART_PTR_FIX(core::IntArray_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::PointerType_O::static_creator);
 SMART_PTR_FIX(core::BranchSNode_O::static_class);
 SMART_PTR_FIX(llvmo::DataLayoutPass_O::static_creator);
 SMART_PTR_FIX(core::Stream_O::static_creator);
 SMART_PTR_FIX(core::StdClass_O::static_class);
 SMART_PTR_FIX(llvmo::DIScope_O::static_class);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_deleted);
 SMART_PTR_FIX(core::Closure_O::static_class_symbol);
 SMART_PTR_FIX(core::GlueEnvironment_O::static_creator);
 SMART_PTR_FIX(core::Array_O::static_creator);
 SMART_PTR_FIX(llvmo::Linker_O::static_creator);
 SMART_PTR_FIX(core::ShortFloat_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTable_O::static_class_symbol);
 SMART_PTR_FIX(core::Integer_O::static_class_symbol);
 SMART_PTR_FIX(core::EchoStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DebugLoc_O::static_class);
 SMART_PTR_FIX(llvmo::Module_O::static_creator);
 SMART_PTR_FIX(core::SourcePosInfo_O::static_creator);
 SMART_PTR_FIX(core::StdClass_O::static_creator);
 SMART_PTR_FIX(llvmo::MCSubtargetInfo_O::static_class_symbol);
 SMART_PTR_FIX(core::CompiledClosure_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IRBuilderBase_O::static_creator);
 SMART_PTR_FIX(core::SexpSaveArchive_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::SequentialType_O::static_class);
 SMART_PTR_FIX(llvmo::APInt_O::static_creator);
 SMART_PTR_FIX(core::ArrayObjects_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Module_O::static_class);
 SMART_PTR_FIX(core::BlockEnvironment_O::static_creator);
 SMART_PTR_FIX(core::SingleDispatchGenericFunction_O::static_class);
 SMART_PTR_FIX(core::MacroletEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::Metaobject_O::static_class);
 SMART_PTR_FIX(llvmo::DIType_O::static_class);
 SMART_PTR_FIX(core::Number_O::static_class);
 SMART_PTR_FIX(core::BlockEnvironment_O::static_class);
 SMART_PTR_FIX(core::VectorStepper_O::static_class_symbol);
 SMART_PTR_FIX(core::StructureClass_O::static_creator);
 SMART_PTR_FIX(core::SimpleBitVector_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Pass_O::static_class_symbol);
 SMART_PTR_FIX(core::SequenceStepper_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantArray_O::static_class);
 SMART_PTR_FIX(llvmo::PassManagerBase_O::static_class);
 SMART_PTR_FIX(core::Null_O::static_creator);
 SMART_PTR_FIX(llvmo::IRBuilder_O::static_class_symbol);
 SMART_PTR_FIX(core::EchoStream_O::static_creator);
 SMART_PTR_FIX(llvmo::User_O::static_creator);
 SMART_PTR_FIX(llvmo::AtomicRMWInst_O::static_class_symbol);
 SMART_PTR_FIX(core::CompileTimeEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::DebugLoc_O::static_class_symbol);
 SMART_PTR_FIX(core::VaList_dummy_O::static_class_symbol);
 SMART_PTR_FIX(core::StringStream_O::static_class);
 SMART_PTR_FIX(core::VectorObjects_O::static_class);
 SMART_PTR_FIX(core::StringInputStream_O::static_class);
 SMART_PTR_FIX(core::SourcePosInfo_O::static_class);
 SMART_PTR_FIX(core::Real_O::static_creator);
 SMART_PTR_FIX(llvmo::TargetMachine_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Module_O::static_class_symbol);
 SMART_PTR_FIX(core::Number_O::static_creator);
 SMART_PTR_FIX(llvmo::DITypeArray_O::static_creator);
 SMART_PTR_FIX(llvmo::StructType_O::static_creator);
 SMART_PTR_FIX(core::Complex_O::static_class);
 SMART_PTR_FIX(llvmo::LLVMTargetMachine_O::static_class_symbol);
 SMART_PTR_FIX(core::ForeignData_O::static_creator);
 SMART_PTR_FIX(llvmo::PassManagerBuilder_O::static_creator);
 SMART_PTR_FIX(llvmo::APInt_O::static_class);
 SMART_PTR_FIX(llvmo::MDString_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Value_O::static_class);
 SMART_PTR_FIX(llvmo::UnreachableInst_O::static_creator);
 SMART_PTR_FIX(core::WeakKeyHashTable_O::static_creator);
 SMART_PTR_FIX(llvmo::Constant_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetLibraryInfo_O::static_class);
 SMART_PTR_FIX(core::WeakPointer_O::static_class);
 SMART_PTR_FIX(core::StringStream_O::static_creator);
 SMART_PTR_FIX(core::Ratio_O::static_creator);
 SMART_PTR_FIX(asttooling::AstVisitor_O::static_class);
 SMART_PTR_FIX(core::IOFileStream_O::static_creator);
 SMART_PTR_FIX(core::SexpSaveArchive_O::static_class);
 SMART_PTR_FIX(core::IOFileStream_O::static_class_symbol);
 SMART_PTR_FIX(core::Fixnum_dummy_O::static_class_symbol);
 SMART_PTR_FIX(core::BroadcastStream_O::static_class_symbol);
 SMART_PTR_FIX(core::Float_O::static_creator);
 SMART_PTR_FIX(llvmo::DIBuilder_O::static_class);
 SMART_PTR_FIX(clbind::ClassRegistry_O::static_creator);
 SMART_PTR_FIX(llvmo::ExecutionEngine_O::static_class);
 SMART_PTR_FIX(core::SingleDispatchEffectiveMethodFunction_O::static_class);
 SMART_PTR_FIX(llvmo::UndefValue_O::static_creator);
 SMART_PTR_FIX(core::Regex_O::static_class);
 SMART_PTR_FIX(llvmo::DIBuilder_O::static_creator);
 SMART_PTR_FIX(core::SingleDispatchMethod_O::static_creator);
 SMART_PTR_FIX(core::StandardClass_O::static_class);
 SMART_PTR_FIX(core::CxxClass_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantFP_O::static_creator);
 SMART_PTR_FIX(core::ForeignData_O::static_class);
 SMART_PTR_FIX(core::Package_O::static_class);
 SMART_PTR_FIX(core::StructureObject_O::static_class_symbol);
 SMART_PTR_FIX(core::StructureObject_O::static_class);
 SMART_PTR_FIX(core::VectorObjects_O::static_creator);
 SMART_PTR_FIX(core::SexpLoadArchive_O::static_creator);
 SMART_PTR_FIX(core::Function_O::static_class);
 SMART_PTR_FIX(llvmo::UnreachableInst_O::static_class);
 SMART_PTR_FIX(core::StrWithFillPtr_O::static_class);
 SMART_PTR_FIX(llvmo::AtomicRMWInst_O::static_creator);
 SMART_PTR_FIX(core::Record_O::static_class);
 SMART_PTR_FIX(llvmo::DIScope_O::static_class_symbol);
 SMART_PTR_FIX(core::StructureClass_O::static_class_symbol);
 SMART_PTR_FIX(core::MultiStringBuffer_O::static_class);
 SMART_PTR_FIX(core::FileStatus_O::static_class);
 SMART_PTR_FIX(llvmo::BasicBlock_O::static_creator);
 SMART_PTR_FIX(core::Float_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::PassManagerBase_O::static_creator);
 SMART_PTR_FIX(core::LightUserData_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantDataSequential_O::static_creator);
 SMART_PTR_FIX(llvmo::DataLayout_O::static_class_symbol);
 SMART_PTR_FIX(core::InterpretedClosure_O::static_creator);
 SMART_PTR_FIX(asttooling::AstVisitor_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TerminatorInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Path_O::static_creator);
 SMART_PTR_FIX(core::Null_O::static_class_symbol);
 SMART_PTR_FIX(core::BitVectorWithFillPtr_O::static_creator);
 SMART_PTR_FIX(core::SmallMap_O::static_class_symbol);
 SMART_PTR_FIX(core::FunctionClosure_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::BranchInst_O::static_class);
 SMART_PTR_FIX(core::IOFileStream_O::static_class);
 SMART_PTR_FIX(core::WeakKeyMapping_O::static_class);
 SMART_PTR_FIX(llvmo::IndirectBrInst_O::static_class);
 SMART_PTR_FIX(llvmo::APInt_O::static_class_symbol);
 SMART_PTR_FIX(core::SNode_O::static_creator);
 SMART_PTR_FIX(llvmo::Triple_O::static_creator);
 SMART_PTR_FIX(llvmo::PHINode_O::static_creator);
 SMART_PTR_FIX(llvmo::DIFile_O::static_creator);
 SMART_PTR_FIX(llvmo::ValueAsMetadata_O::static_class_symbol);
 SMART_PTR_FIX(core::SourceFileInfo_O::static_class);
 SMART_PTR_FIX(llvmo::BasicBlock_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DILexicalBlock_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantPointerNull_O::static_class_symbol);
 SMART_PTR_FIX(core::Closure_O::static_creator);
 SMART_PTR_FIX(core::StringInputStream_O::static_creator);
 SMART_PTR_FIX(core::HashTable_O::static_creator);
 SMART_PTR_FIX(core::FunctionFrame_O::static_class_symbol);
 SMART_PTR_FIX(core::Iterator_O::static_class);
 SMART_PTR_FIX(core::RegexMatch_O::static_class);
 SMART_PTR_FIX(llvmo::Argument_O::static_class);
 SMART_PTR_FIX(asttooling::AstVisitor_O::static_creator);
 SMART_PTR_FIX(llvmo::DICompileUnit_O::static_class);
 SMART_PTR_FIX(llvmo::ImmutablePass_O::static_class);
 SMART_PTR_FIX(core::Character_dummy_O::static_class);
 SMART_PTR_FIX(core::Pathname_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::SwitchInst_O::static_class);
 SMART_PTR_FIX(core::Record_O::static_creator);
 SMART_PTR_FIX(llvmo::DIArray_O::static_creator);
 SMART_PTR_FIX(core::MacroletEnvironment_O::static_creator);
 SMART_PTR_FIX(core::SimpleBitVector_O::static_creator);
 SMART_PTR_FIX(core::ArrayObjects_O::static_creator);
 SMART_PTR_FIX(llvmo::NamedMDNode_O::static_class_symbol);
 SMART_PTR_FIX(core::InvocationHistoryFrameIterator_O::static_class);
 SMART_PTR_FIX(core::SourcePosInfo_O::static_class_symbol);
 SMART_PTR_FIX(core::LightUserData_O::static_class);
 SMART_PTR_FIX(cffi::Pointer_O::static_creator);
 SMART_PTR_FIX(llvmo::Target_O::static_creator);
 SMART_PTR_FIX(core::Rational_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::MDNode_O::static_creator);
 SMART_PTR_FIX(core::BitVector_O::static_class);
 SMART_PTR_FIX(core::Archive_O::static_class);
 SMART_PTR_FIX(llvmo::BranchInst_O::static_class_symbol);
 SMART_PTR_FIX(core::FunctionContainerEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::AtomicRMWInst_O::static_class);
 SMART_PTR_FIX(llvmo::ModulePass_O::static_creator);
 SMART_PTR_FIX(llvmo::Linker_O::static_class);
 SMART_PTR_FIX(core::SNode_O::static_class_symbol);
 SMART_PTR_FIX(core::FunctionClosure_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantFP_O::static_class);
 SMART_PTR_FIX(core::Package_O::static_class_symbol);
 SMART_PTR_FIX(clbind::ClassRegistry_O::static_class_symbol);
 SMART_PTR_FIX(core::Metaobject_O::static_creator);
 SMART_PTR_FIX(llvmo::ArrayType_O::static_creator);
 SMART_PTR_FIX(core::ValueEnvironment_O::static_class);
 SMART_PTR_FIX(core::PosixTime_O::static_class);
 SMART_PTR_FIX(core::LoadTimeValues_O::static_creator);
 SMART_PTR_FIX(llvmo::IntegerType_O::static_class);
 SMART_PTR_FIX(core::Integer_O::static_creator);
 SMART_PTR_FIX(core::WeakHashTable_O::static_creator);
 SMART_PTR_FIX(core::ShortFloat_O::static_class);
 SMART_PTR_FIX(llvmo::FunctionType_O::static_creator);
 SMART_PTR_FIX(core::DirectoryIterator_O::static_creator);
 SMART_PTR_FIX(core::DirectoryIterator_O::static_class_symbol);
 SMART_PTR_FIX(core::LexicalEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::TargetMachine_O::static_class);
 SMART_PTR_FIX(core::TagbodyEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DebugInfo_O::static_class);
 SMART_PTR_FIX(llvmo::Triple_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ImmutablePass_O::static_creator);
 SMART_PTR_FIX(core::StandardClass_O::static_creator);
 SMART_PTR_FIX(llvmo::FunctionPassManager_O::static_creator);
 SMART_PTR_FIX(core::LoadArchive_O::static_class);
 SMART_PTR_FIX(core::RegexMatch_O::static_class_symbol);
 SMART_PTR_FIX(core::LoadTimeValues_O::static_class);
 SMART_PTR_FIX(llvmo::EngineBuilder_O::static_class_symbol);
 SMART_PTR_FIX(core::ValueFrame_O::static_class);
 SMART_PTR_FIX(llvmo::Function_O::static_class);
 SMART_PTR_FIX(core::TwoWayStream_O::static_class_symbol);
 SMART_PTR_FIX(core::ReadTable_O::static_class);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_nil);
 SMART_PTR_FIX(core::Functor_O::static_creator);
 SMART_PTR_FIX(core::SmallMap_O::static_creator);
 SMART_PTR_FIX(core::SmallMap_O::static_class);
 SMART_PTR_FIX(core::SynonymStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::BlockAddress_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ResumeInst_O::static_class);
 SMART_PTR_FIX(core::SourceManager_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantArray_O::static_class_symbol);
 SMART_PTR_FIX(core::Ratio_O::static_class);
 SMART_PTR_FIX(llvmo::LLVMContext_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantStruct_O::static_class);
 SMART_PTR_FIX(core::DoubleFloat_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetMachine_O::static_creator);
 SMART_PTR_FIX(core::RandomState_O::static_class);
 SMART_PTR_FIX(core::Cons_O::static_class);
 SMART_PTR_FIX(llvmo::LoadInst_O::static_class);
 SMART_PTR_FIX(llvmo::Type_O::static_class_symbol);
 SMART_PTR_FIX(core::ForeignData_O::static_class_symbol);
 SMART_PTR_FIX(core::VectorDisplaced_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::BasicBlock_O::static_class);
 SMART_PTR_FIX(core::Character_dummy_O::static_creator);
 SMART_PTR_FIX(llvmo::PassManagerBuilder_O::static_class_symbol);
 SMART_PTR_FIX(core::Vector_O::static_creator);
 SMART_PTR_FIX(core::LongFloat_O::static_class);
 SMART_PTR_FIX(llvmo::AtomicCmpXchgInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Symbol_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Metadata_O::static_creator);
 SMART_PTR_FIX(llvmo::DIBuilder_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ModulePass_O::static_class);
 SMART_PTR_FIX(llvmo::ValueAsMetadata_O::static_class);
 SMART_PTR_FIX(llvmo::GlobalVariable_O::static_class_symbol);
 SMART_PTR_FIX(core::SynonymStream_O::static_creator);
 SMART_PTR_FIX(core::ForwardReferencedClass_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTableEqualp_O::static_class);
 SMART_PTR_FIX(core::Pointer_O::static_class);
 SMART_PTR_FIX(core::UserData_O::static_creator);
 SMART_PTR_FIX(llvmo::DISubprogram_O::static_creator);
 SMART_PTR_FIX(llvmo::BranchInst_O::static_creator);
 SMART_PTR_FIX(core::StackValueEnvironment_O::static_class);
 SMART_PTR_FIX(core::CompileTimeEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::StringInputStream_O::static_class_symbol);
 SMART_PTR_FIX(core::ExternalObject_O::static_class);
 SMART_PTR_FIX(core::FileStatus_O::static_creator);
 SMART_PTR_FIX(core::DirectoryEntry_O::static_creator);
 SMART_PTR_FIX(core::BitVectorWithFillPtr_O::static_class_symbol);
 SMART_PTR_FIX(core::Class_O::static_class);
 SMART_PTR_FIX(llvmo::TerminatorInst_O::static_class);
 SMART_PTR_FIX(core::RandomState_O::static_creator);
 SMART_PTR_FIX(core::SpecialForm_O::static_class_symbol);
 SMART_PTR_FIX(core::TagbodyEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::DISubroutineType_O::static_class);
 SMART_PTR_FIX(core::SingleDispatchEffectiveMethodFunction_O::static_creator);
 SMART_PTR_FIX(core::VectorStepper_O::static_creator);
 SMART_PTR_FIX(llvmo::PassManager_O::static_creator);
 SMART_PTR_FIX(core::SourceFileInfo_O::static_creator);
 SMART_PTR_FIX(core::GlueEnvironment_O::static_class);
 SMART_PTR_FIX(core::Bignum_O::static_creator);
 SMART_PTR_FIX(core::RuntimeVisibleEnvironment_O::static_class);
 SMART_PTR_FIX(core::CxxObject_O::static_creator);
 SIMPLE_POINTER_FIX(globalTaggedRunTimeValues);
 SMART_PTR_FIX(core::Real_O::static_class);
 SMART_PTR_FIX(core::RecursiveDirectoryIterator_O::static_class);
 SMART_PTR_FIX(core::BuiltinClosure_O::static_class);
 SMART_PTR_FIX(llvmo::VAArgInst_O::static_class);
 SMART_PTR_FIX(core::SNode_O::static_class);
 SMART_PTR_FIX(core::EchoStream_O::static_class);
 SMART_PTR_FIX(core::Creator_O::static_class_symbol);
 SMART_PTR_FIX(core::InstanceCreator_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTableEqual_O::static_creator);
 SMART_PTR_FIX(core::ActivationFrame_O::static_creator);
 SMART_PTR_FIX(core::StackValueEnvironment_O::static_creator);
 SMART_PTR_FIX(core::StandardObject_O::static_class);
 SMART_PTR_FIX(core::SaveArchive_O::static_creator);
 SMART_PTR_FIX(core::CompiledFunction_O::static_class);
 SMART_PTR_FIX(llvmo::DIDescriptor_O::static_class_symbol);
 SMART_PTR_FIX(core::String_O::static_creator);
 SMART_PTR_FIX(llvmo::ModulePass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Type_O::static_creator);
 SMART_PTR_FIX(llvmo::StructType_O::static_class_symbol);
 SMART_PTR_FIX(core::SourceFileInfo_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetOptions_O::static_class_symbol);
 SMART_PTR_FIX(core::StandardObject_O::static_creator);
 SMART_PTR_FIX(llvmo::ArrayType_O::static_class);
 SMART_PTR_FIX(llvmo::IndirectBrInst_O::static_creator);
 SMART_PTR_FIX(core::StringOutputStream_O::static_class);
 SMART_PTR_FIX(core::Rational_O::static_class);
 SMART_PTR_FIX(core::TagbodyFrame_O::static_creator);
 SMART_PTR_FIX(core::Fixnum_dummy_O::static_creator);
 SMART_PTR_FIX(core::Functor_O::static_class);
 SMART_PTR_FIX(core::VectorDisplaced_O::static_creator);
 SMART_PTR_FIX(core::InvocationHistoryFrameIterator_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantInt_O::static_class_symbol);
 SMART_PTR_FIX(core::Instance_O::static_creator);
 SMART_PTR_FIX(core::CxxClass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIBasicType_O::static_creator);
 SMART_PTR_FIX(llvmo::DIType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIArray_O::static_class_symbol);
 SMART_PTR_FIX(core::Binder_O::static_creator);
 SMART_PTR_FIX(core::Creator_O::static_class);
 SMART_PTR_FIX(core::InstanceCreator_O::static_class);
 SMART_PTR_FIX(core::StructureObject_O::static_creator);
 SMART_PTR_FIX(llvmo::PassManager_O::static_class);
 SMART_PTR_FIX(core::SymbolMacroletEnvironment_O::static_creator);
 SMART_PTR_FIX(core::ValueEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::InsertPoint_O::static_creator);
 SMART_PTR_FIX(core::ValueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetLibraryInfo_O::static_class_symbol);
 SMART_PTR_FIX(core::DirectoryEntry_O::static_class);
 SMART_PTR_FIX(llvmo::SwitchInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Path_O::static_class);
 SMART_PTR_FIX(core::VaList_dummy_O::static_class);
 SMART_PTR_FIX(core::SymbolToEnumConverter_O::static_class);
 SMART_PTR_FIX(core::CandoException_O::static_class);
 SMART_PTR_FIX(llvmo::PHINode_O::static_class);
 SMART_PTR_FIX(core::Class_O::static_creator);
 SMART_PTR_FIX(core::ForwardReferencedClass_O::static_class);
 SMART_PTR_FIX(llvmo::FenceInst_O::static_creator);
 SMART_PTR_FIX(core::SharpEqualWrapper_O::static_creator);
 SMART_PTR_FIX(core::FunctionFrame_O::static_creator);
 SMART_PTR_FIX(llvmo::NamedMDNode_O::static_creator);
 SMART_PTR_FIX(llvmo::DISubroutineType_O::static_class_symbol);
 SMART_PTR_FIX(core::ShortFloat_O::static_creator);
 SMART_PTR_FIX(core::Functor_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IntegerType_O::static_class_symbol);
 SMART_PTR_FIX(core::TwoWayStream_O::static_creator);
 SMART_PTR_FIX(llvmo::MCSubtargetInfo_O::static_class);
 SMART_PTR_FIX(llvmo::DISubprogram_O::static_class_symbol);
 SMART_PTR_FIX(core::ActivationFrame_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIBasicType_O::static_class_symbol);
 SMART_PTR_FIX(core::IOStreamStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantStruct_O::static_class_symbol);
 SMART_PTR_FIX(core::ConcatenatedStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantDataArray_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::CallInst_O::static_class);
 SMART_PTR_FIX(core::Bignum_O::static_class_symbol);
 SMART_PTR_FIX(core::LogicalPathname_O::static_creator);
 SMART_PTR_FIX(core::GlueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::Rational_O::static_creator);
 SMART_PTR_FIX(llvmo::UnreachableInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::PointerType_O::static_class);
 SMART_PTR_FIX(llvmo::IRBuilder_O::static_creator);
 SMART_PTR_FIX(llvmo::TerminatorInst_O::static_creator);
 SMART_PTR_FIX(llvmo::DIDescriptor_O::static_creator);
 SMART_PTR_FIX(core::Instance_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Value_O::static_creator);
 SMART_PTR_FIX(core::InterpretedClosure_O::static_class_symbol);
 SMART_PTR_FIX(core::LongFloat_O::static_creator);
 SMART_PTR_FIX(core::UnwindProtectEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::VaList_dummy_O::static_creator);
 SMART_PTR_FIX(core::CxxClass_O::static_class);
 SMART_PTR_FIX(core::Instance_O::static_class);
 SMART_PTR_FIX(llvmo::Attribute_O::static_creator);
 SMART_PTR_FIX(core::ArrayDisplaced_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Instruction_O::static_creator);
 SMART_PTR_FIX(core::InvocationHistoryFrameIterator_O::static_creator);
 SMART_PTR_FIX(llvmo::FunctionPass_O::static_creator);
 SMART_PTR_FIX(llvmo::ReturnInst_O::static_class);
 SMART_PTR_FIX(llvmo::DICompileUnit_O::static_class_symbol);
 SMART_PTR_FIX(core::RuntimeVisibleEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::GlobalValue_O::static_class);
 SMART_PTR_FIX(llvmo::LoadInst_O::static_class_symbol);
 SMART_PTR_FIX(core::SingleDispatchGenericFunction_O::static_creator);
 SMART_PTR_FIX(core::WeakKeyMapping_O::static_class_symbol);
 SMART_PTR_FIX(core::CxxObject_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::GlobalValue_O::static_class_symbol);
 SMART_PTR_FIX(core::Class_O::static_class_symbol);
 SMART_PTR_FIX(core::VectorStepper_O::static_class);
 SMART_PTR_FIX(llvmo::SwitchInst_O::static_creator);
 SMART_PTR_FIX(llvmo::Metadata_O::static_class_symbol);
 SMART_PTR_FIX(core::SourceManager_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DITypeArray_O::static_class);
 SMART_PTR_FIX(core::StandardObject_O::static_class_symbol);
 SMART_PTR_FIX(core::InterpretedClosure_O::static_class);
 SMART_PTR_FIX(llvmo::GlobalVariable_O::static_class);
 SMART_PTR_FIX(llvmo::InvokeInst_O::static_creator);
 SMART_PTR_FIX(llvmo::VectorType_O::static_creator);
 SMART_PTR_FIX(core::Bignum_O::static_class);
 SMART_PTR_FIX(core::Regex_O::static_class_symbol);
 SMART_PTR_FIX(core::Iterator_O::static_creator);
 SMART_PTR_FIX(llvmo::DICompositeType_O::static_class);
 SMART_PTR_FIX(core::SingleFloat_dummy_O::static_class);
 SMART_PTR_FIX(core::ExternalObject_O::static_creator);
 SMART_PTR_FIX(core::SymbolToEnumConverter_O::static_creator);
 SMART_PTR_FIX(clbind::ClassRep_O::static_class);
 SMART_PTR_FIX(core::HashTableEql_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::EngineBuilder_O::static_class);
 SMART_PTR_FIX(llvmo::DIDerivedType_O::static_class_symbol);
 SMART_PTR_FIX(core::MultiStringBuffer_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DebugLoc_O::static_creator);
 SMART_PTR_FIX(core::InstanceClosure_O::static_class_symbol);
 SMART_PTR_FIX(core::Regex_O::static_creator);
 SMART_PTR_FIX(core::SingleDispatchMethod_O::static_class);
 SMART_PTR_FIX(core::CompiledFunction_O::static_class_symbol);
 SMART_PTR_FIX(core::IntArray_O::static_creator);
 SMART_PTR_FIX(core::Stream_O::static_class);
 SMART_PTR_FIX(core::General_O::static_creator);
 SMART_PTR_FIX(llvmo::ReturnInst_O::static_class_symbol);
#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
