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
        class RegistryMaps;
        class SymbolMatcherDescriptorPair;
    };
    namespace internal {
        class FreeFuncMatcherDescriptor;
        class FixedArgCountMatcherDescriptor;
        class OverloadedMatcherDescriptor;
        class VariadicOperatorMatcherDescriptor;
    };
 };
 namespace cffi {
    class Pointer_O;
 };
 namespace clbind {
    class DummyCreator;
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
    class CompiledClosure;
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
    class VectorStepper;
    class FileStream_O;
    class BitVector_O;
    class InterpretedClosure;
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
    class SpecialForm_O;
    class AnsiStream_O;
    class Rational_O;
    class ConcatenatedStream_O;
    class CoreExposer;
    class Str_O;
    class ActivationFrame_O;
    class Environment_O;
    class Array_O;
    class StringStream_O;
    class KeywordArgument;
    class TagbodyFrame_O;
    class SingleDispatchMethod_O;
    class SexpSaveArchive_O;
    class RandomState_O;
    class UserData_O;
    class StdClass_O;
    class FunctionContainerEnvironment_O;
    class HashTableEqualp_O;
    class InstanceClosure;
    class StructureClass_O;
    class Integer_O;
    class FunctionValueEnvironment_O;
    class RegexMatch_O;
    class WeakPointer_O;
    class VaList_dummy_O;
    class ValueFrame_O;
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
    class InstanceCreator;
    class SourceManager_O;
    class SaveArchive_O;
    class Record_O;
    class StackValueEnvironment_O;
    class Specializer_O;
    class Null_O;
    class T_O;
    class LightUserData_O;
    class Symbol_O;
    class DoubleFloat_O;
    class SourcePosInfo_O;
    class VectorObjectsWithFillPtr_O;
    class RequiredArgument;
    class Float_O;
    class SymbolClassPair;
    class ConsStepper;
    class MacroClosure;
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
    class Cons_O;
    class Archive_O;
    class HashTable_O;
    class MacroletEnvironment_O;
    class CxxObject_O;
    class WeakKeyMapping_O;
    class Metaobject_O;
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
    class Cache;
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
    class WeakKeyHashTable_O;
    class ForeignData_O;
    class CacheRecord;
    class Instance_O;
    class BroadcastStream_O;
    class CatchEnvironment_O;
    class LexicalEnvironment_O;
    class Pathname_O;
    class SingleDispatchGenericFunctionClosure;
    class PosixTime_O;
    class SmallMap_O;
    class ShortFloat_O;
    class Lisp_O;
    class ExceptionEntry;
    class GlueEnvironment_O;
    class VectorObjects_O;
    class Ratio_O;
    class LoadArchive_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_FIXNUM = 1, 
KIND_SINGLE_FLOAT = 2, 
KIND_CHARACTER = 3, 
KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps = 4,
KIND_ROOTCLASSALLOC_clbind__detail__class_map = 5,
KIND_TEMPLATED_CLASSALLOC_core__Creator = 6,
KIND_CLASSALLOC_clbind__DummyCreator = 7,
KIND_CLASSALLOC_core__InstanceCreator = 8,
KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator = 9,
KIND_BOOTSTRAP_core__T_O = 10,
KIND_LISPALLOC_core__MultiStringBuffer_O = 11,
KIND_LISPALLOC_core__ReadTable_O = 12,
KIND_LISPALLOC_core__Number_O = 13,
KIND_LISPALLOC_core__Complex_O = 14,
KIND_LISPALLOC_core__Real_O = 15,
KIND_LISPALLOC_core__Rational_O = 16,
KIND_LISPALLOC_core__Integer_O = 17,
KIND_LISPALLOC_core__Bignum_O = 18,
KIND_LISPALLOC_core__Fixnum_dummy_O = 19,
KIND_LISPALLOC_core__Ratio_O = 20,
KIND_LISPALLOC_core__Float_O = 21,
KIND_LISPALLOC_core__DoubleFloat_O = 22,
KIND_LISPALLOC_core__LongFloat_O = 23,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 24,
KIND_LISPALLOC_core__ShortFloat_O = 25,
KIND_LISPALLOC_core__FileStatus_O = 26,
KIND_LISPALLOC_core__WeakHashTable_O = 27,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 28,
KIND_LISPALLOC_core__Environment_O = 29,
KIND_LISPALLOC_core__ActivationFrame_O = 30,
KIND_LISPALLOC_core__TagbodyFrame_O = 31,
KIND_LISPALLOC_core__ValueFrame_O = 32,
KIND_LISPALLOC_core__FunctionFrame_O = 33,
KIND_LISPALLOC_core__LexicalEnvironment_O = 34,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 35,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 36,
KIND_LISPALLOC_core__ValueEnvironment_O = 37,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 38,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 39,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 40,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 41,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 42,
KIND_LISPALLOC_core__StackValueEnvironment_O = 43,
KIND_LISPALLOC_core__BlockEnvironment_O = 44,
KIND_LISPALLOC_core__MacroletEnvironment_O = 45,
KIND_LISPALLOC_core__CatchEnvironment_O = 46,
KIND_LISPALLOC_core__GlueEnvironment_O = 47,
KIND_LISPALLOC_core__Array_O = 48,
KIND_LISPALLOC_core__ArrayObjects_O = 49,
KIND_LISPALLOC_core__ArrayDisplaced_O = 50,
KIND_LISPALLOC_core__Vector_O = 51,
KIND_LISPALLOC_core__BitVector_O = 52,
KIND_LISPALLOC_core__SimpleBitVector_O = 53,
KIND_LISPALLOC_core__BitVectorWithFillPtr_O = 54,
KIND_LISPALLOC_core__VectorDisplaced_O = 55,
KIND_LISPALLOC_core__String_O = 56,
KIND_BOOTSTRAP_core__Str_O = 57,
KIND_LISPALLOC_core__StrWithFillPtr_O = 58,
KIND_LISPALLOC_core__VectorObjects_O = 59,
KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O = 60,
KIND_LISPALLOC_core__SingleDispatchMethod_O = 61,
KIND_LISPALLOC_core__RandomState_O = 62,
KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O = 63,
KIND_LISPALLOC_llvmo__DebugLoc_O = 64,
KIND_LISPALLOC_llvmo__Attribute_O = 65,
KIND_LISPALLOC_core__RegexMatch_O = 66,
KIND_LISPALLOC_core__WeakPointer_O = 67,
KIND_LISPALLOC_core__VaList_dummy_O = 68,
KIND_BOOTSTRAP_core__StandardObject_O = 69,
KIND_BOOTSTRAP_core__Metaobject_O = 70,
KIND_BOOTSTRAP_core__Specializer_O = 71,
KIND_BOOTSTRAP_core__Class_O = 72,
KIND_BOOTSTRAP_core__StdClass_O = 73,
KIND_BOOTSTRAP_core__StandardClass_O = 74,
KIND_LISPALLOC_core__FuncallableStandardClass_O = 75,
KIND_BOOTSTRAP_core__StructureClass_O = 76,
KIND_LISPALLOC_core__ForwardReferencedClass_O = 77,
KIND_LISPALLOC_core__CxxClass_O = 78,
KIND_BOOTSTRAP_core__BuiltInClass_O = 79,
KIND_LISPALLOC_clbind__ClassRep_O = 80,
KIND_LISPALLOC_core__ExternalObject_O = 81,
KIND_LISPALLOC_llvmo__Value_O = 82,
KIND_LISPALLOC_llvmo__Argument_O = 83,
KIND_LISPALLOC_llvmo__User_O = 84,
KIND_LISPALLOC_llvmo__Instruction_O = 85,
KIND_LISPALLOC_llvmo__AtomicRMWInst_O = 86,
KIND_LISPALLOC_llvmo__LandingPadInst_O = 87,
KIND_LISPALLOC_llvmo__PHINode_O = 88,
KIND_LISPALLOC_llvmo__CallInst_O = 89,
KIND_LISPALLOC_llvmo__StoreInst_O = 90,
KIND_LISPALLOC_llvmo__UnaryInstruction_O = 91,
KIND_LISPALLOC_llvmo__LoadInst_O = 92,
KIND_LISPALLOC_llvmo__AllocaInst_O = 93,
KIND_LISPALLOC_llvmo__VAArgInst_O = 94,
KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O = 95,
KIND_LISPALLOC_llvmo__TerminatorInst_O = 96,
KIND_LISPALLOC_llvmo__UnreachableInst_O = 97,
KIND_LISPALLOC_llvmo__SwitchInst_O = 98,
KIND_LISPALLOC_llvmo__ReturnInst_O = 99,
KIND_LISPALLOC_llvmo__ResumeInst_O = 100,
KIND_LISPALLOC_llvmo__BranchInst_O = 101,
KIND_LISPALLOC_llvmo__InvokeInst_O = 102,
KIND_LISPALLOC_llvmo__IndirectBrInst_O = 103,
KIND_LISPALLOC_llvmo__FenceInst_O = 104,
KIND_LISPALLOC_llvmo__Constant_O = 105,
KIND_LISPALLOC_llvmo__BlockAddress_O = 106,
KIND_LISPALLOC_llvmo__GlobalValue_O = 107,
KIND_LISPALLOC_llvmo__GlobalVariable_O = 108,
KIND_LISPALLOC_llvmo__Function_O = 109,
KIND_LISPALLOC_llvmo__ConstantArray_O = 110,
KIND_LISPALLOC_llvmo__ConstantInt_O = 111,
KIND_LISPALLOC_llvmo__ConstantDataSequential_O = 112,
KIND_LISPALLOC_llvmo__ConstantDataArray_O = 113,
KIND_LISPALLOC_llvmo__ConstantStruct_O = 114,
KIND_LISPALLOC_llvmo__ConstantFP_O = 115,
KIND_LISPALLOC_llvmo__UndefValue_O = 116,
KIND_LISPALLOC_llvmo__ConstantPointerNull_O = 117,
KIND_LISPALLOC_llvmo__ConstantExpr_O = 118,
KIND_LISPALLOC_llvmo__BasicBlock_O = 119,
KIND_LISPALLOC_llvmo__IRBuilderBase_O = 120,
KIND_LISPALLOC_llvmo__IRBuilder_O = 121,
KIND_LISPALLOC_llvmo__DIBuilder_O = 122,
KIND_LISPALLOC_llvmo__Metadata_O = 123,
KIND_LISPALLOC_llvmo__ValueAsMetadata_O = 124,
KIND_LISPALLOC_llvmo__MDNode_O = 125,
KIND_LISPALLOC_llvmo__MDString_O = 126,
KIND_LISPALLOC_llvmo__ExecutionEngine_O = 127,
KIND_LISPALLOC_llvmo__APFloat_O = 128,
KIND_LISPALLOC_llvmo__PassManagerBuilder_O = 129,
KIND_LISPALLOC_llvmo__DataLayout_O = 130,
KIND_LISPALLOC_llvmo__Triple_O = 131,
KIND_LISPALLOC_llvmo__APInt_O = 132,
KIND_LISPALLOC_llvmo__PassManagerBase_O = 133,
KIND_LISPALLOC_llvmo__FunctionPassManager_O = 134,
KIND_LISPALLOC_llvmo__PassManager_O = 135,
KIND_LISPALLOC_llvmo__TargetMachine_O = 136,
KIND_LISPALLOC_llvmo__LLVMTargetMachine_O = 137,
KIND_LISPALLOC_llvmo__TargetOptions_O = 138,
KIND_LISPALLOC_llvmo__Type_O = 139,
KIND_LISPALLOC_llvmo__IntegerType_O = 140,
KIND_LISPALLOC_llvmo__CompositeType_O = 141,
KIND_LISPALLOC_llvmo__SequentialType_O = 142,
KIND_LISPALLOC_llvmo__VectorType_O = 143,
KIND_LISPALLOC_llvmo__PointerType_O = 144,
KIND_LISPALLOC_llvmo__ArrayType_O = 145,
KIND_LISPALLOC_llvmo__StructType_O = 146,
KIND_LISPALLOC_llvmo__FunctionType_O = 147,
KIND_LISPALLOC_llvmo__NamedMDNode_O = 148,
KIND_LISPALLOC_llvmo__Linker_O = 149,
KIND_LISPALLOC_llvmo__Pass_O = 150,
KIND_LISPALLOC_llvmo__FunctionPass_O = 151,
KIND_LISPALLOC_llvmo__ModulePass_O = 152,
KIND_LISPALLOC_llvmo__ImmutablePass_O = 153,
KIND_LISPALLOC_llvmo__DataLayoutPass_O = 154,
KIND_LISPALLOC_llvmo__TargetLibraryInfo_O = 155,
KIND_LISPALLOC_llvmo__MCSubtargetInfo_O = 156,
KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O = 157,
KIND_LISPALLOC_llvmo__Module_O = 158,
KIND_LISPALLOC_llvmo__EngineBuilder_O = 159,
KIND_LISPALLOC_core__ForeignData_O = 160,
KIND_LISPALLOC_llvmo__LLVMContext_O = 161,
KIND_LISPALLOC_llvmo__Target_O = 162,
KIND_LISPALLOC_core__LoadTimeValues_O = 163,
KIND_LISPALLOC_core__Binder_O = 164,
KIND_LISPALLOC_core__IntArray_O = 165,
KIND_LISPALLOC_core__SourceManager_O = 166,
KIND_LISPALLOC_core__Record_O = 167,
KIND_LISPALLOC_core__LightUserData_O = 168,
KIND_LISPALLOC_core__UserData_O = 169,
KIND_BOOTSTRAP_core__Symbol_O = 170,
KIND_LISPALLOC_core__Null_O = 171,
KIND_LISPALLOC_core__SourcePosInfo_O = 172,
KIND_TEMPLATED_LISPALLOC_core__Iterator_O = 173,
KIND_LISPALLOC_core__DirectoryIterator_O = 174,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 175,
KIND_LISPALLOC_core__Regex_O = 176,
KIND_LISPALLOC_core__PosixTimeDuration_O = 177,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 178,
KIND_LISPALLOC_core__CandoException_O = 179,
KIND_LISPALLOC_core__Stream_O = 180,
KIND_LISPALLOC_core__AnsiStream_O = 181,
KIND_LISPALLOC_core__FileStream_O = 182,
KIND_LISPALLOC_core__IOStreamStream_O = 183,
KIND_LISPALLOC_core__IOFileStream_O = 184,
KIND_LISPALLOC_core__ConcatenatedStream_O = 185,
KIND_LISPALLOC_core__StringStream_O = 186,
KIND_LISPALLOC_core__StringInputStream_O = 187,
KIND_LISPALLOC_core__StringOutputStream_O = 188,
KIND_LISPALLOC_core__SynonymStream_O = 189,
KIND_LISPALLOC_core__EchoStream_O = 190,
KIND_LISPALLOC_core__TwoWayStream_O = 191,
KIND_LISPALLOC_core__BroadcastStream_O = 192,
KIND_LISPALLOC_core__Reader_O = 193,
KIND_LISPALLOC_core__Cons_O = 194,
KIND_LISPALLOC_core__Archive_O = 195,
KIND_LISPALLOC_core__SaveArchive_O = 196,
KIND_LISPALLOC_core__SexpSaveArchive_O = 197,
KIND_LISPALLOC_core__LoadArchive_O = 198,
KIND_LISPALLOC_core__SexpLoadArchive_O = 199,
KIND_LISPALLOC_core__HashTable_O = 200,
KIND_LISPALLOC_core__HashTableEq_O = 201,
KIND_LISPALLOC_core__HashTableEqualp_O = 202,
KIND_LISPALLOC_core__HashTableEql_O = 203,
KIND_LISPALLOC_core__HashTableEqual_O = 204,
KIND_LISPALLOC_cffi__Pointer_O = 205,
KIND_LISPALLOC_core__CxxObject_O = 206,
KIND_LISPALLOC_core__WeakKeyMapping_O = 207,
KIND_LISPALLOC_core__LambdaListHandler_O = 208,
KIND_LISPALLOC_llvmo__InsertPoint_O = 209,
KIND_LISPALLOC_core__SourceFileInfo_O = 210,
KIND_LISPALLOC_core__SNode_O = 211,
KIND_LISPALLOC_core__LeafSNode_O = 212,
KIND_LISPALLOC_core__BranchSNode_O = 213,
KIND_LISPALLOC_core__Path_O = 214,
KIND_LISPALLOC_asttooling__AstVisitor_O = 215,
KIND_LISPALLOC_llvmo__AttributeSet_O = 216,
KIND_LISPALLOC_core__StructureObject_O = 217,
KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O = 218,
KIND_LISPALLOC_core__Package_O = 219,
KIND_LISPALLOC_core__DirectoryEntry_O = 220,
KIND_LISPALLOC_core__Character_dummy_O = 221,
KIND_LISPALLOC_core__Function_O = 222,
KIND_LISPALLOC_core__CompiledFunction_O = 223,
KIND_LISPALLOC_core__SingleDispatchGenericFunction_O = 224,
KIND_LISPALLOC_core__SpecialForm_O = 225,
KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O = 226,
KIND_LISPALLOC_core__Instance_O = 227,
KIND_LISPALLOC_core__Pointer_O = 228,
KIND_LISPALLOC_clbind__ClassRegistry_O = 229,
KIND_LISPALLOC_llvmo__DebugInfo_O = 230,
KIND_LISPALLOC_llvmo__DIDerivedType_O = 231,
KIND_LISPALLOC_llvmo__DIArray_O = 232,
KIND_LISPALLOC_llvmo__DIBasicType_O = 233,
KIND_LISPALLOC_llvmo__DISubprogram_O = 234,
KIND_LISPALLOC_llvmo__DILexicalBlock_O = 235,
KIND_LISPALLOC_llvmo__DICompileUnit_O = 236,
KIND_LISPALLOC_llvmo__DIDescriptor_O = 237,
KIND_LISPALLOC_llvmo__DIType_O = 238,
KIND_LISPALLOC_llvmo__DISubroutineType_O = 239,
KIND_LISPALLOC_llvmo__DICompositeType_O = 240,
KIND_LISPALLOC_llvmo__DITypeArray_O = 241,
KIND_LISPALLOC_llvmo__DIFile_O = 242,
KIND_LISPALLOC_llvmo__DIScope_O = 243,
KIND_LISPALLOC_core__SmallMultimap_O = 244,
KIND_LISPALLOC_core__Pathname_O = 245,
KIND_LISPALLOC_core__LogicalPathname_O = 246,
KIND_LISPALLOC_core__PosixTime_O = 247,
KIND_LISPALLOC_core__SmallMap_O = 248,
KIND_CLASSALLOC_core__Cache = 249,
KIND_ROOTCLASSALLOC_core__Lisp_O = 250,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__ = 251,
KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ = 252,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 253,
KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure = 254,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 255,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 256,
KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ = 257,
KIND_CLASSALLOC_llvmo__CompiledClosure = 258,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 259,
KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor = 260,
KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___ = 261,
KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor = 262,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_ = 263,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ = 264,
KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor = 265,
KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_ = 266,
KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor = 267,
KIND_CLASSALLOC_core__MacroClosure = 268,
KIND_CLASSALLOC_core__ConsStepper = 269,
KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ = 270,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_ = 271,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 272,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 273,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_ = 274,
KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___ = 275,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 276,
KIND_CLASSALLOC_core__InstanceClosure = 277,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ = 278,
KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory = 279,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_ = 280,
KIND_LISPALLOC_asttooling__DerivableMatchCallback = 281,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_ = 282,
KIND_LISPALLOC_asttooling__DerivableASTFrontendAction = 283,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ = 284,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__ = 285,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_ = 286,
KIND_CLASSALLOC_core__CoreExposer = 287,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 288,
KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction = 289,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__ = 290,
KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ = 291,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 292,
KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure = 293,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 294,
KIND_CLASSALLOC_core__InterpretedClosure = 295,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 296,
KIND_CLASSALLOC_core__VectorStepper = 297,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__ = 298,
  KIND_max = 298
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct Cast<llvmo::VAArgInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 94 94 
      return (kindVal == 94);
  };
};
template <typename FP> struct Cast<core::LoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 198 199 
      return ((198 <= kindVal) && (kindVal <= 199));
  };
};
template <typename FP> struct Cast<core::Ratio_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 20 20 
      return (kindVal == 20);
  };
};
template <typename FP> struct Cast<core::VectorObjects_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 59 60 
      return ((59 <= kindVal) && (kindVal <= 60));
  };
};
template <typename FP> struct Cast<core::GlueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 47 47 
      return (kindVal == 47);
  };
};
template <typename FP> struct Cast<llvmo::Target_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 162 162 
      return (kindVal == 162);
  };
};
template <typename FP> struct Cast<llvmo::FunctionType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 147 147 
      return (kindVal == 147);
  };
};
template <typename FP> struct Cast<llvmo::ConstantExpr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 118 118 
      return (kindVal == 118);
  };
};
template <typename FP> struct Cast<llvmo::LLVMContext_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 161 161 
      return (kindVal == 161);
  };
};
template <typename FP> struct Cast<core::Lisp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 250 250 
      return (kindVal == 250);
  };
};
template <typename FP> struct Cast<llvmo::TargetSubtargetInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 157 157 
      return (kindVal == 157);
  };
};
template <typename FP> struct Cast<core::ShortFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 25 25 
      return (kindVal == 25);
  };
};
template <typename FP> struct Cast<core::SmallMap_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 248 248 
      return (kindVal == 248);
  };
};
template <typename FP> struct Cast<llvmo::FenceInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 104 104 
      return (kindVal == 104);
  };
};
template <typename FP> struct Cast<core::PosixTime_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 247 247 
      return (kindVal == 247);
  };
};
template <typename FP> struct Cast<core::Pathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 245 246 
      return ((245 <= kindVal) && (kindVal <= 246));
  };
};
template <typename FP> struct Cast<core::LexicalEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 34 46 
      return ((34 <= kindVal) && (kindVal <= 46));
  };
};
template <typename FP> struct Cast<core::CatchEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 46 46 
      return (kindVal == 46);
  };
};
template <typename FP> struct Cast<core::BroadcastStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 192 192 
      return (kindVal == 192);
  };
};
template <typename FP> struct Cast<core::Instance_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 227 227 
      return (kindVal == 227);
  };
};
template <typename FP> struct Cast<llvmo::DIScope_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 243 243 
      return (kindVal == 243);
  };
};
template <typename FP> struct Cast<core::ForeignData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 160 160 
      return (kindVal == 160);
  };
};
template <typename FP> struct Cast<core::WeakKeyHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 28 28 
      return (kindVal == 28);
  };
};
template <typename FP> struct Cast<llvmo::IndirectBrInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 103 103 
      return (kindVal == 103);
  };
};
template <typename FP> struct Cast<llvmo::ConstantPointerNull_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 117 117 
      return (kindVal == 117);
  };
};
template <typename FP> struct Cast<llvmo::EngineBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 159 159 
      return (kindVal == 159);
  };
};
template <typename FP> struct Cast<llvmo::Module_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 158 158 
      return (kindVal == 158);
  };
};
template <typename FP> struct Cast<core::SmallMultimap_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 244 244 
      return (kindVal == 244);
  };
};
template <typename FP> struct Cast<core::LogicalPathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 246 246 
      return (kindVal == 246);
  };
};
template <typename FP> struct Cast<llvmo::DebugInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 230 243 
      return ((230 <= kindVal) && (kindVal <= 243));
  };
};
template <typename FP> struct Cast<clbind::ClassRegistry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 229 229 
      return (kindVal == 229);
  };
};
template <typename FP> struct Cast<core::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 228 228 
      return (kindVal == 228);
  };
};
template <typename FP> struct Cast<core::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 222 227 
      return ((222 <= kindVal) && (kindVal <= 227));
  };
};
template <typename FP> struct Cast<core::HashTableEqual_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 204 204 
      return (kindVal == 204);
  };
};
template <typename FP> struct Cast<core::Character_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 221 221 
      return (kindVal == 221);
  };
};
template <typename FP> struct Cast<core::DirectoryEntry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 220 220 
      return (kindVal == 220);
  };
};
template <typename FP> struct Cast<clbind::ConstructorCreator*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 9 9 
      return (kindVal == 9);
  };
};
template <typename FP> struct Cast<llvmo::MCSubtargetInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 156 157 
      return ((156 <= kindVal) && (kindVal <= 157));
  };
};
template <typename FP> struct Cast<llvmo::MDString_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 126 126 
      return (kindVal == 126);
  };
};
template <typename FP> struct Cast<core::Class_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 72 80 
      return ((72 <= kindVal) && (kindVal <= 80));
  };
};
template <typename FP> struct Cast<core::FuncallableStandardClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 75 75 
      return (kindVal == 75);
  };
};
template <typename FP> struct Cast<llvmo::DIFile_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 242 242 
      return (kindVal == 242);
  };
};
template <typename FP> struct Cast<llvmo::Pass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 150 155 
      return ((150 <= kindVal) && (kindVal <= 155));
  };
};
template <typename FP> struct Cast<llvmo::Linker_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 149 149 
      return (kindVal == 149);
  };
};
template <typename FP> struct Cast<llvmo::TerminatorInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 96 103 
      return ((96 <= kindVal) && (kindVal <= 103));
  };
};
template <typename FP> struct Cast<core::FunctionFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 33 33 
      return (kindVal == 33);
  };
};
template <typename FP> struct Cast<llvmo::UndefValue_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 116 116 
      return (kindVal == 116);
  };
};
template <typename FP> struct Cast<core::TwoWayStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 191 191 
      return (kindVal == 191);
  };
};
template <typename FP> struct Cast<core::IOFileStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 184 184 
      return (kindVal == 184);
  };
};
template <typename FP> struct Cast<llvmo::NamedMDNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 148 148 
      return (kindVal == 148);
  };
};
template <typename FP> struct Cast<llvmo::ArrayType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 145 145 
      return (kindVal == 145);
  };
};
template <typename FP> struct Cast<core::RecursiveDirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 175 175 
      return (kindVal == 175);
  };
};
template <typename FP> struct Cast<llvmo::Type_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 139 147 
      return ((139 <= kindVal) && (kindVal <= 147));
  };
};
template <typename FP> struct Cast<llvmo::AtomicCmpXchgInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 95 95 
      return (kindVal == 95);
  };
};
template <typename FP> struct Cast<llvmo::AllocaInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 93 93 
      return (kindVal == 93);
  };
};
template <typename FP> struct Cast<core::Package_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 219 219 
      return (kindVal == 219);
  };
};
template <typename FP> struct Cast<core::Fixnum_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 19 19 
      return (kindVal == 19);
  };
};
template <typename FP> struct Cast<llvmo::BasicBlock_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 119 119 
      return (kindVal == 119);
  };
};
template <typename FP> struct Cast<llvmo::CompositeType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 141 146 
      return ((141 <= kindVal) && (kindVal <= 146));
  };
};
template <typename FP> struct Cast<core::CompileTimeEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 39 46 
      return ((39 <= kindVal) && (kindVal <= 46));
  };
};
template <typename FP> struct Cast<llvmo::PassManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 135 135 
      return (kindVal == 135);
  };
};
template <typename FP> struct Cast<core::Cache*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 249 249 
      return (kindVal == 249);
  };
};
template <typename FP> struct Cast<core::TagbodyEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 38 38 
      return (kindVal == 38);
  };
};
template <typename FP> struct Cast<core::IOStreamStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 183 183 
      return (kindVal == 183);
  };
};
template <typename FP> struct Cast<core::InvocationHistoryFrameIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 218 218 
      return (kindVal == 218);
  };
};
template <typename FP> struct Cast<core::StrWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 58 58 
      return (kindVal == 58);
  };
};
template <typename FP> struct Cast<core::StructureObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 217 217 
      return (kindVal == 217);
  };
};
template <typename FP> struct Cast<core::ValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 37 37 
      return (kindVal == 37);
  };
};
template <typename FP> struct Cast<llvmo::AttributeSet_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 216 216 
      return (kindVal == 216);
  };
};
template <typename FP> struct Cast<core::BuiltInClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 79 80 
      return ((79 <= kindVal) && (kindVal <= 80));
  };
};
template <typename FP> struct Cast<core::CxxClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 78 78 
      return (kindVal == 78);
  };
};
template <typename FP> struct Cast<core::SingleFloat_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 24 24 
      return (kindVal == 24);
  };
};
template <typename FP> struct Cast<core::DirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 174 174 
      return (kindVal == 174);
  };
};
template <typename FP> struct Cast<core::SexpLoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 199 199 
      return (kindVal == 199);
  };
};
template <typename FP> struct Cast<core::StringOutputStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 188 188 
      return (kindVal == 188);
  };
};
template <typename FP> struct Cast<asttooling::AstVisitor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 215 215 
      return (kindVal == 215);
  };
};
template <typename FP> struct Cast<core::Vector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 51 60 
      return ((51 <= kindVal) && (kindVal <= 60));
  };
};
template <typename FP> struct Cast<core::Path_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 214 214 
      return (kindVal == 214);
  };
};
template <typename FP> struct Cast<llvmo::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 109 109 
      return (kindVal == 109);
  };
};
template <typename FP> struct Cast<llvmo::InvokeInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 102 102 
      return (kindVal == 102);
  };
};
template <typename FP> struct Cast<core::SNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 211 213 
      return ((211 <= kindVal) && (kindVal <= 213));
  };
};
template <typename FP> struct Cast<core::SourceFileInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 210 210 
      return (kindVal == 210);
  };
};
template <typename FP> struct Cast<llvmo::InsertPoint_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 209 209 
      return (kindVal == 209);
  };
};
template <typename FP> struct Cast<llvmo::DITypeArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 241 241 
      return (kindVal == 241);
  };
};
template <typename FP> struct Cast<core::ForwardReferencedClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 77 77 
      return (kindVal == 77);
  };
};
template <typename FP> struct Cast<core::HashTableEql_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 203 203 
      return (kindVal == 203);
  };
};
template <typename FP> struct Cast<core::LambdaListHandler_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 208 208 
      return (kindVal == 208);
  };
};
template <typename FP> struct Cast<core::ArrayDisplaced_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 50 50 
      return (kindVal == 50);
  };
};
template <typename FP> struct Cast<llvmo::TargetOptions_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 138 138 
      return (kindVal == 138);
  };
};
template <typename FP> struct Cast<core::Metaobject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 70 80 
      return ((70 <= kindVal) && (kindVal <= 80));
  };
};
template <typename FP> struct Cast<llvmo::PointerType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 144 144 
      return (kindVal == 144);
  };
};
template <typename FP> struct Cast<core::WeakKeyMapping_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 207 207 
      return (kindVal == 207);
  };
};
template <typename FP> struct Cast<core::CxxObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 206 206 
      return (kindVal == 206);
  };
};
template <typename FP> struct Cast<cffi::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 205 205 
      return (kindVal == 205);
  };
};
template <typename FP> struct Cast<llvmo::DICompositeType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 240 240 
      return (kindVal == 240);
  };
};
template <typename FP> struct Cast<llvmo::BranchInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 101 101 
      return (kindVal == 101);
  };
};
template <typename FP> struct Cast<core::MacroletEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 45 45 
      return (kindVal == 45);
  };
};
template <typename FP> struct Cast<core::HashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 200 204 
      return ((200 <= kindVal) && (kindVal <= 204));
  };
};
template <typename FP> struct Cast<core::Archive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 195 199 
      return ((195 <= kindVal) && (kindVal <= 199));
  };
};
template <typename FP> struct Cast<core::Cons_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 194 194 
      return (kindVal == 194);
  };
};
template <typename FP> struct Cast<core::Reader_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 193 193 
      return (kindVal == 193);
  };
};
template <typename FP> struct Cast<core::Stream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 180 192 
      return ((180 <= kindVal) && (kindVal <= 192));
  };
};
template <typename FP> struct Cast<llvmo::UnaryInstruction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 91 94 
      return ((91 <= kindVal) && (kindVal <= 94));
  };
};
template <typename FP> struct Cast<core::BranchSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 213 213 
      return (kindVal == 213);
  };
};
template <typename FP> struct Cast<llvmo::FunctionPassManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 134 134 
      return (kindVal == 134);
  };
};
template <typename FP> struct Cast<core::SingleDispatchEffectiveMethodFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 226 226 
      return (kindVal == 226);
  };
};
template <typename FP> struct Cast<core::StringInputStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 187 187 
      return (kindVal == 187);
  };
};
template <typename FP> struct Cast<llvmo::TargetMachine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 136 137 
      return ((136 <= kindVal) && (kindVal <= 137));
  };
};
template <typename FP> struct Cast<core::CandoException_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 179 179 
      return (kindVal == 179);
  };
};
template <typename FP> struct Cast<llvmo::GlobalVariable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 108 108 
      return (kindVal == 108);
  };
};
template <typename FP> struct Cast<core::EchoStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 190 190 
      return (kindVal == 190);
  };
};
template <typename FP> struct Cast<core::SymbolToEnumConverter_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 178 178 
      return (kindVal == 178);
  };
};
template <typename FP> struct Cast<core::PosixTimeDuration_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 177 177 
      return (kindVal == 177);
  };
};
template <typename FP> struct Cast<core::StandardClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 74 74 
      return (kindVal == 74);
  };
};
template <typename FP> struct Cast<core::Regex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 176 176 
      return (kindVal == 176);
  };
};
template <typename FP> struct Cast<llvmo::PassManagerBase_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 133 135 
      return ((133 <= kindVal) && (kindVal <= 135));
  };
};
template <typename FP> struct Cast<core::Iterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 173 175 
      return ((173 <= kindVal) && (kindVal <= 175));
  };
};
template <typename FP> struct Cast<clbind::ClassRep_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 80 80 
      return (kindVal == 80);
  };
};
template <typename FP> struct Cast<llvmo::IRBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 121 121 
      return (kindVal == 121);
  };
};
template <typename FP> struct Cast<llvmo::StoreInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 90 90 
      return (kindVal == 90);
  };
};
template <typename FP> struct Cast<core::LongFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 23 23 
      return (kindVal == 23);
  };
};
template <typename FP> struct Cast<core::BlockEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 44 44 
      return (kindVal == 44);
  };
};
template <typename FP> struct Cast<llvmo::ResumeInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 100 100 
      return (kindVal == 100);
  };
};
template <typename FP> struct Cast<llvmo::TargetLibraryInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 155 155 
      return (kindVal == 155);
  };
};
template <typename FP> struct Cast<llvmo::DISubroutineType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 239 239 
      return (kindVal == 239);
  };
};
template <typename FP> struct Cast<llvmo::DIType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 238 238 
      return (kindVal == 238);
  };
};
template <typename FP> struct Cast<core::Float_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 21 25 
      return ((21 <= kindVal) && (kindVal <= 25));
  };
};
template <typename FP> struct Cast<core::VectorObjectsWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 60 60 
      return (kindVal == 60);
  };
};
template <typename FP> struct Cast<llvmo::MDNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 125 125 
      return (kindVal == 125);
  };
};
template <typename FP> struct Cast<core::SourcePosInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 172 172 
      return (kindVal == 172);
  };
};
template <typename FP> struct Cast<llvmo::DIDescriptor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 237 237 
      return (kindVal == 237);
  };
};
template <typename FP> struct Cast<llvmo::LLVMTargetMachine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 137 137 
      return (kindVal == 137);
  };
};
template <typename FP> struct Cast<llvmo::APInt_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 132 132 
      return (kindVal == 132);
  };
};
template <typename FP> struct Cast<llvmo::ReturnInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 99 99 
      return (kindVal == 99);
  };
};
template <typename FP> struct Cast<llvmo::Triple_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 131 131 
      return (kindVal == 131);
  };
};
template <typename FP> struct Cast<core::DoubleFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 22 22 
      return (kindVal == 22);
  };
};
template <typename FP> struct Cast<core::Symbol_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 170 171 
      return ((170 <= kindVal) && (kindVal <= 171));
  };
};
template <typename FP> struct Cast<llvmo::DataLayout_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 130 130 
      return (kindVal == 130);
  };
};
template <typename FP> struct Cast<core::LightUserData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 168 169 
      return ((168 <= kindVal) && (kindVal <= 169));
  };
};
template <typename FP> struct Cast<core::T_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 10 248 
      return ((10 <= kindVal) && (kindVal <= 248));
  };
};
template <typename FP> struct Cast<core::Null_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 171 171 
      return (kindVal == 171);
  };
};
template <typename FP> struct Cast<llvmo::PassManagerBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 129 129 
      return (kindVal == 129);
  };
};
template <typename FP> struct Cast<core::Creator*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 6 9 
      return ((6 <= kindVal) && (kindVal <= 9));
  };
};
template <typename FP> struct Cast<core::Specializer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 71 80 
      return ((71 <= kindVal) && (kindVal <= 80));
  };
};
template <typename FP> struct Cast<core::StackValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 43 43 
      return (kindVal == 43);
  };
};
template <typename FP> struct Cast<llvmo::ConstantFP_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 115 115 
      return (kindVal == 115);
  };
};
template <typename FP> struct Cast<llvmo::LoadInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 92 92 
      return (kindVal == 92);
  };
};
template <typename FP> struct Cast<llvmo::APFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 128 128 
      return (kindVal == 128);
  };
};
template <typename FP> struct Cast<core::Record_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 167 167 
      return (kindVal == 167);
  };
};
template <typename FP> struct Cast<core::SaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 196 197 
      return ((196 <= kindVal) && (kindVal <= 197));
  };
};
template <typename FP> struct Cast<core::SourceManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 166 166 
      return (kindVal == 166);
  };
};
template <typename FP> struct Cast<core::InstanceCreator*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 8 8 
      return (kindVal == 8);
  };
};
template <typename FP> struct Cast<core::IntArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 165 165 
      return (kindVal == 165);
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
      // low high --> 54 54 
      return (kindVal == 54);
  };
};
template <typename FP> struct Cast<core::LeafSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 212 212 
      return (kindVal == 212);
  };
};
template <typename FP> struct Cast<llvmo::Constant_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 105 118 
      return ((105 <= kindVal) && (kindVal <= 118));
  };
};
template <typename FP> struct Cast<core::Binder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 164 164 
      return (kindVal == 164);
  };
};
template <typename FP> struct Cast<core::String_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 56 58 
      return ((56 <= kindVal) && (kindVal <= 58));
  };
};
template <typename FP> struct Cast<core::VectorDisplaced_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 55 55 
      return (kindVal == 55);
  };
};
template <typename FP> struct Cast<core::SynonymStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 189 189 
      return (kindVal == 189);
  };
};
template <typename FP> struct Cast<core::LoadTimeValues_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 163 163 
      return (kindVal == 163);
  };
};
template <typename FP> struct Cast<core::ExternalObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 81 162 
      return ((81 <= kindVal) && (kindVal <= 162));
  };
};
template <typename FP> struct Cast<core::SimpleBitVector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 53 53 
      return (kindVal == 53);
  };
};
template <typename FP> struct Cast<core::StandardObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 69 80 
      return ((69 <= kindVal) && (kindVal <= 80));
  };
};
template <typename FP> struct Cast<llvmo::ImmutablePass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 153 155 
      return ((153 <= kindVal) && (kindVal <= 155));
  };
};
template <typename FP> struct Cast<core::RuntimeVisibleEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 35 38 
      return ((35 <= kindVal) && (kindVal <= 38));
  };
};
template <typename FP> struct Cast<core::ValueFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 32 32 
      return (kindVal == 32);
  };
};
template <typename FP> struct Cast<core::VaList_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 68 68 
      return (kindVal == 68);
  };
};
template <typename FP> struct Cast<llvmo::StructType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 146 146 
      return (kindVal == 146);
  };
};
template <typename FP> struct Cast<core::WeakPointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 67 67 
      return (kindVal == 67);
  };
};
template <typename FP> struct Cast<llvmo::ExecutionEngine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 127 127 
      return (kindVal == 127);
  };
};
template <typename FP> struct Cast<llvmo::ConstantDataArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 113 113 
      return (kindVal == 113);
  };
};
template <typename FP> struct Cast<core::RegexMatch_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 66 66 
      return (kindVal == 66);
  };
};
template <typename FP> struct Cast<core::FunctionValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 36 36 
      return (kindVal == 36);
  };
};
template <typename FP> struct Cast<core::Integer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 17 19 
      return ((17 <= kindVal) && (kindVal <= 19));
  };
};
template <typename FP> struct Cast<core::StructureClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 76 76 
      return (kindVal == 76);
  };
};
template <typename FP> struct Cast<llvmo::IntegerType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 140 140 
      return (kindVal == 140);
  };
};
template <typename FP> struct Cast<llvmo::Attribute_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 65 65 
      return (kindVal == 65);
  };
};
template <typename FP> struct Cast<core::HashTableEqualp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 202 202 
      return (kindVal == 202);
  };
};
template <typename FP> struct Cast<llvmo::DebugLoc_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 64 64 
      return (kindVal == 64);
  };
};
template <typename FP> struct Cast<core::FunctionContainerEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 42 42 
      return (kindVal == 42);
  };
};
template <typename FP> struct Cast<llvmo::DICompileUnit_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 236 236 
      return (kindVal == 236);
  };
};
template <typename FP> struct Cast<llvmo::VectorType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 143 143 
      return (kindVal == 143);
  };
};
template <typename FP> struct Cast<core::StdClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 73 75 
      return ((73 <= kindVal) && (kindVal <= 75));
  };
};
template <typename FP> struct Cast<llvmo::DILexicalBlock_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 235 235 
      return (kindVal == 235);
  };
};
template <typename FP> struct Cast<llvmo::Metadata_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 123 126 
      return ((123 <= kindVal) && (kindVal <= 126));
  };
};
template <typename FP> struct Cast<clbind::DummyCreator*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 7 7 
      return (kindVal == 7);
  };
};
template <typename FP> struct Cast<llvmo::User_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 84 118 
      return ((84 <= kindVal) && (kindVal <= 118));
  };
};
template <typename FP> struct Cast<core::WrappedPointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 63 63 
      return (kindVal == 63);
  };
};
template <typename FP> struct Cast<core::UserData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 169 169 
      return (kindVal == 169);
  };
};
template <typename FP> struct Cast<core::RandomState_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 62 62 
      return (kindVal == 62);
  };
};
template <typename FP> struct Cast<llvmo::SequentialType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 142 145 
      return ((142 <= kindVal) && (kindVal <= 145));
  };
};
template <typename FP> struct Cast<llvmo::ConstantStruct_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 114 114 
      return (kindVal == 114);
  };
};
template <typename FP> struct Cast<llvmo::CallInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 89 89 
      return (kindVal == 89);
  };
};
template <typename FP> struct Cast<llvmo::SwitchInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 98 98 
      return (kindVal == 98);
  };
};
template <typename FP> struct Cast<core::SexpSaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 197 197 
      return (kindVal == 197);
  };
};
template <typename FP> struct Cast<core::SingleDispatchMethod_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 61 61 
      return (kindVal == 61);
  };
};
template <typename FP> struct Cast<core::TagbodyFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 31 31 
      return (kindVal == 31);
  };
};
template <typename FP> struct Cast<asttooling::RegMap::RegistryMaps*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 4 4 
      return (kindVal == 4);
  };
};
template <typename FP> struct Cast<core::StringStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 186 188 
      return ((186 <= kindVal) && (kindVal <= 188));
  };
};
template <typename FP> struct Cast<llvmo::DIBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 122 122 
      return (kindVal == 122);
  };
};
template <typename FP> struct Cast<core::Array_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 48 60 
      return ((48 <= kindVal) && (kindVal <= 60));
  };
};
template <typename FP> struct Cast<llvmo::ConstantDataSequential_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 112 113 
      return ((112 <= kindVal) && (kindVal <= 113));
  };
};
template <typename FP> struct Cast<llvmo::ConstantInt_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 111 111 
      return (kindVal == 111);
  };
};
template <typename FP> struct Cast<core::Environment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 29 47 
      return ((29 <= kindVal) && (kindVal <= 47));
  };
};
template <typename FP> struct Cast<llvmo::ValueAsMetadata_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 124 124 
      return (kindVal == 124);
  };
};
template <typename FP> struct Cast<core::ActivationFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 30 33 
      return ((30 <= kindVal) && (kindVal <= 33));
  };
};
template <typename FP> struct Cast<core::Str_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 57 58 
      return ((57 <= kindVal) && (kindVal <= 58));
  };
};
template <typename FP> struct Cast<llvmo::UnreachableInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 97 97 
      return (kindVal == 97);
  };
};
template <typename FP> struct Cast<core::ConcatenatedStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 185 185 
      return (kindVal == 185);
  };
};
template <typename FP> struct Cast<llvmo::PHINode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 88 88 
      return (kindVal == 88);
  };
};
template <typename FP> struct Cast<llvmo::Instruction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 85 104 
      return ((85 <= kindVal) && (kindVal <= 104));
  };
};
template <typename FP> struct Cast<core::Rational_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 16 20 
      return ((16 <= kindVal) && (kindVal <= 20));
  };
};
template <typename FP> struct Cast<core::AnsiStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 181 192 
      return ((181 <= kindVal) && (kindVal <= 192));
  };
};
template <typename FP> struct Cast<core::SpecialForm_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 225 225 
      return (kindVal == 225);
  };
};
template <typename FP> struct Cast<llvmo::ConstantArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 110 110 
      return (kindVal == 110);
  };
};
template <typename FP> struct Cast<core::WeakHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 27 28 
      return ((27 <= kindVal) && (kindVal <= 28));
  };
};
template <typename FP> struct Cast<core::Real_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 15 25 
      return ((15 <= kindVal) && (kindVal <= 25));
  };
};
template <typename FP> struct Cast<llvmo::DISubprogram_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 234 234 
      return (kindVal == 234);
  };
};
template <typename FP> struct Cast<llvmo::DIBasicType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 233 233 
      return (kindVal == 233);
  };
};
template <typename FP> struct Cast<core::FileStatus_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 26 26 
      return (kindVal == 26);
  };
};
template <typename FP> struct Cast<llvmo::GlobalValue_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 107 109 
      return ((107 <= kindVal) && (kindVal <= 109));
  };
};
template <typename FP> struct Cast<llvmo::LandingPadInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 87 87 
      return (kindVal == 87);
  };
};
template <typename FP> struct Cast<llvmo::IRBuilderBase_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 120 121 
      return ((120 <= kindVal) && (kindVal <= 121));
  };
};
template <typename FP> struct Cast<core::SingleDispatchGenericFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 224 224 
      return (kindVal == 224);
  };
};
template <typename FP> struct Cast<core::SymbolMacroletEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 41 41 
      return (kindVal == 41);
  };
};
template <typename FP> struct Cast<llvmo::Value_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 82 119 
      return ((82 <= kindVal) && (kindVal <= 119));
  };
};
template <typename FP> struct Cast<core::Number_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 13 25 
      return ((13 <= kindVal) && (kindVal <= 25));
  };
};
template <typename FP> struct Cast<core::ReadTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 12 12 
      return (kindVal == 12);
  };
};
template <typename FP> struct Cast<core::UnwindProtectEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 40 40 
      return (kindVal == 40);
  };
};
template <typename FP> struct Cast<core::Bignum_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 18 18 
      return (kindVal == 18);
  };
};
template <typename FP> struct Cast<core::CompiledFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 223 223 
      return (kindVal == 223);
  };
};
template <typename FP> struct Cast<core::ArrayObjects_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 49 49 
      return (kindVal == 49);
  };
};
template <typename FP> struct Cast<llvmo::DIArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 232 232 
      return (kindVal == 232);
  };
};
template <typename FP> struct Cast<llvmo::ModulePass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 152 155 
      return ((152 <= kindVal) && (kindVal <= 155));
  };
};
template <typename FP> struct Cast<llvmo::DIDerivedType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 231 231 
      return (kindVal == 231);
  };
};
template <typename FP> struct Cast<llvmo::Argument_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 83 83 
      return (kindVal == 83);
  };
};
template <typename FP> struct Cast<core::Complex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 14 14 
      return (kindVal == 14);
  };
};
template <typename FP> struct Cast<llvmo::BlockAddress_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 106 106 
      return (kindVal == 106);
  };
};
template <typename FP> struct Cast<core::BitVector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 52 54 
      return ((52 <= kindVal) && (kindVal <= 54));
  };
};
template <typename FP> struct Cast<llvmo::FunctionPass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 151 151 
      return (kindVal == 151);
  };
};
template <typename FP> struct Cast<core::FileStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 182 184 
      return ((182 <= kindVal) && (kindVal <= 184));
  };
};
template <typename FP> struct Cast<llvmo::AtomicRMWInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 86 86 
      return (kindVal == 86);
  };
};
template <typename FP> struct Cast<core::HashTableEq_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 201 201 
      return (kindVal == 201);
  };
};
template <typename FP> struct Cast<llvmo::DataLayoutPass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 154 154 
      return (kindVal == 154);
  };
};
template <typename FP> struct Cast<core::MultiStringBuffer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 11 11 
      return (kindVal == 11);
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
template <> class gctools::GCKind<core::VectorObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjects_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__ ;
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
template <> class gctools::GCKind<core::SingleDispatchGenericFunctionClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure ;
};
template <> class gctools::GCKind<core::Pathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pathname_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ ;
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
template <> class gctools::GCKind<llvmo::DIScope_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIScope_O ;
};
template <> class gctools::GCKind<core::ForeignData_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ForeignData_O ;
};
template <> class gctools::GCKind<llvmo::CompiledClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_llvmo__CompiledClosure ;
};
template <> class gctools::GCKind<core::WeakKeyHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyHashTable_O ;
};
template <> class gctools::GCKind<llvmo::IndirectBrInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IndirectBrInst_O ;
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
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<clbind::ClassRegistry_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_clbind__ClassRegistry_O ;
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
template <> class gctools::GCKind<clbind::ConstructorCreator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator ;
};
template <> class gctools::GCKind<llvmo::MCSubtargetInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MCSubtargetInfo_O ;
};
template <> class gctools::GCKind<llvmo::MDString_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MDString_O ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<asttooling::internal::VariadicOperatorMatcherDescriptor> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor ;
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
template <> class gctools::GCKind<core::Cache> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__Cache ;
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
template <> class gctools::GCKind<asttooling::internal::OverloadedMatcherDescriptor> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor ;
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
template <> class gctools::GCKind<llvmo::DICompositeType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DICompositeType_O ;
};
template <> class gctools::GCKind<llvmo::BranchInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BranchInst_O ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ContextFrame>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ ;
};
template <> class gctools::GCKind<core::HashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTable_O ;
};
template <> class gctools::GCKind<core::Archive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Archive_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
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
template <> class gctools::GCKind<asttooling::internal::FixedArgCountMatcherDescriptor> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor ;
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
template <> class gctools::GCKind<asttooling::internal::FreeFuncMatcherDescriptor> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor ;
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
template <> class gctools::GCKind<core::MacroClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__MacroClosure ;
};
template <> class gctools::GCKind<llvmo::TargetLibraryInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetLibraryInfo_O ;
};
template <> class gctools::GCKind<core::ConsStepper> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__ConsStepper ;
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
template <> class gctools::GCKind<core::Creator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__Creator ;
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
template <> class gctools::GCKind<core::SaveArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SaveArchive_O ;
};
template <> class gctools::GCKind<core::SourceManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceManager_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ParserValue>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_ ;
};
template <> class gctools::GCKind<core::InstanceCreator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__InstanceCreator ;
};
template <> class gctools::GCKind<core::IntArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__IntArray_O ;
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
template <> class gctools::GCKind<core::VectorDisplaced_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorDisplaced_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolClassPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ ;
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
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::VaList_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VaList_dummy_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_ ;
};
template <> class gctools::GCKind<llvmo::StructType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__StructType_O ;
};
template <> class gctools::GCKind<core::WeakPointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakPointer_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___ ;
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
template <> class gctools::GCKind<core::InstanceClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__InstanceClosure ;
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
template <> class gctools::GCKind<llvmo::DILexicalBlock_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DILexicalBlock_O ;
};
template <> class gctools::GCKind<llvmo::Metadata_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Metadata_O ;
};
template <> class gctools::GCKind<clbind::DummyCreator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_clbind__DummyCreator ;
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
template <> class gctools::GCKind<asttooling::RegMap::RegistryMaps> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps ;
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
template <> class gctools::GCKind<llvmo::ConstantDataSequential_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantDataSequential_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::Message>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ ;
};
template <> class gctools::GCKind<llvmo::ConstantInt_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantInt_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__ ;
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
template <> class gctools::GCKind<core::CoreExposer> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__CoreExposer ;
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
template <> class gctools::GCKind<llvmo::IRBuilderBase_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IRBuilderBase_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
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
template <> class gctools::GCKind<core::BuiltinClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure ;
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
template <> class gctools::GCKind<core::InterpretedClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__InterpretedClosure ;
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
template <> class gctools::GCKind<core::VectorStepper> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__VectorStepper ;
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
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_KIND_NAME_MAP)
kind_name_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps:
{
return "KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps";
}
kind_name_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
return "KIND_ROOTCLASSALLOC_clbind__detail__class_map";
}
kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
return "KIND_TEMPLATED_CLASSALLOC_core__Creator";
}
kind_name_KIND_CLASSALLOC_clbind__DummyCreator:
{
return "KIND_CLASSALLOC_clbind__DummyCreator";
}
kind_name_KIND_CLASSALLOC_core__InstanceCreator:
{
return "KIND_CLASSALLOC_core__InstanceCreator";
}
kind_name_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
return "KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator";
}
kind_name_KIND_BOOTSTRAP_core__T_O:
{
return "KIND_BOOTSTRAP_core__T_O";
}
kind_name_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
return "KIND_LISPALLOC_core__MultiStringBuffer_O";
}
kind_name_KIND_LISPALLOC_core__ReadTable_O:
{
return "KIND_LISPALLOC_core__ReadTable_O";
}
kind_name_KIND_LISPALLOC_core__Number_O:
{
return "KIND_LISPALLOC_core__Number_O";
}
kind_name_KIND_LISPALLOC_core__Complex_O:
{
return "KIND_LISPALLOC_core__Complex_O";
}
kind_name_KIND_LISPALLOC_core__Real_O:
{
return "KIND_LISPALLOC_core__Real_O";
}
kind_name_KIND_LISPALLOC_core__Rational_O:
{
return "KIND_LISPALLOC_core__Rational_O";
}
kind_name_KIND_LISPALLOC_core__Integer_O:
{
return "KIND_LISPALLOC_core__Integer_O";
}
kind_name_KIND_LISPALLOC_core__Bignum_O:
{
return "KIND_LISPALLOC_core__Bignum_O";
}
kind_name_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
return "KIND_LISPALLOC_core__Fixnum_dummy_O";
}
kind_name_KIND_LISPALLOC_core__Ratio_O:
{
return "KIND_LISPALLOC_core__Ratio_O";
}
kind_name_KIND_LISPALLOC_core__Float_O:
{
return "KIND_LISPALLOC_core__Float_O";
}
kind_name_KIND_LISPALLOC_core__DoubleFloat_O:
{
return "KIND_LISPALLOC_core__DoubleFloat_O";
}
kind_name_KIND_LISPALLOC_core__LongFloat_O:
{
return "KIND_LISPALLOC_core__LongFloat_O";
}
kind_name_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
return "KIND_LISPALLOC_core__SingleFloat_dummy_O";
}
kind_name_KIND_LISPALLOC_core__ShortFloat_O:
{
return "KIND_LISPALLOC_core__ShortFloat_O";
}
kind_name_KIND_LISPALLOC_core__FileStatus_O:
{
return "KIND_LISPALLOC_core__FileStatus_O";
}
kind_name_KIND_LISPALLOC_core__WeakHashTable_O:
{
return "KIND_LISPALLOC_core__WeakHashTable_O";
}
kind_name_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
return "KIND_LISPALLOC_core__WeakKeyHashTable_O";
}
kind_name_KIND_LISPALLOC_core__Environment_O:
{
return "KIND_LISPALLOC_core__Environment_O";
}
kind_name_KIND_LISPALLOC_core__ActivationFrame_O:
{
return "KIND_LISPALLOC_core__ActivationFrame_O";
}
kind_name_KIND_LISPALLOC_core__TagbodyFrame_O:
{
return "KIND_LISPALLOC_core__TagbodyFrame_O";
}
kind_name_KIND_LISPALLOC_core__ValueFrame_O:
{
return "KIND_LISPALLOC_core__ValueFrame_O";
}
kind_name_KIND_LISPALLOC_core__FunctionFrame_O:
{
return "KIND_LISPALLOC_core__FunctionFrame_O";
}
kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
return "KIND_LISPALLOC_core__LexicalEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
return "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
return "KIND_LISPALLOC_core__FunctionValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__ValueEnvironment_O:
{
return "KIND_LISPALLOC_core__ValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
return "KIND_LISPALLOC_core__TagbodyEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
return "KIND_LISPALLOC_core__CompileTimeEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
return "KIND_LISPALLOC_core__UnwindProtectEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
return "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
return "KIND_LISPALLOC_core__FunctionContainerEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
return "KIND_LISPALLOC_core__StackValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__BlockEnvironment_O:
{
return "KIND_LISPALLOC_core__BlockEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
return "KIND_LISPALLOC_core__MacroletEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__CatchEnvironment_O:
{
return "KIND_LISPALLOC_core__CatchEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__GlueEnvironment_O:
{
return "KIND_LISPALLOC_core__GlueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Array_O:
{
return "KIND_LISPALLOC_core__Array_O";
}
kind_name_KIND_LISPALLOC_core__ArrayObjects_O:
{
return "KIND_LISPALLOC_core__ArrayObjects_O";
}
kind_name_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
return "KIND_LISPALLOC_core__ArrayDisplaced_O";
}
kind_name_KIND_LISPALLOC_core__Vector_O:
{
return "KIND_LISPALLOC_core__Vector_O";
}
kind_name_KIND_LISPALLOC_core__BitVector_O:
{
return "KIND_LISPALLOC_core__BitVector_O";
}
kind_name_KIND_LISPALLOC_core__SimpleBitVector_O:
{
return "KIND_LISPALLOC_core__SimpleBitVector_O";
}
kind_name_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
return "KIND_LISPALLOC_core__BitVectorWithFillPtr_O";
}
kind_name_KIND_LISPALLOC_core__VectorDisplaced_O:
{
return "KIND_LISPALLOC_core__VectorDisplaced_O";
}
kind_name_KIND_LISPALLOC_core__String_O:
{
return "KIND_LISPALLOC_core__String_O";
}
kind_name_KIND_BOOTSTRAP_core__Str_O:
{
return "KIND_BOOTSTRAP_core__Str_O";
}
kind_name_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
return "KIND_LISPALLOC_core__StrWithFillPtr_O";
}
kind_name_KIND_LISPALLOC_core__VectorObjects_O:
{
return "KIND_LISPALLOC_core__VectorObjects_O";
}
kind_name_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
return "KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O";
}
kind_name_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
return "KIND_LISPALLOC_core__SingleDispatchMethod_O";
}
kind_name_KIND_LISPALLOC_core__RandomState_O:
{
return "KIND_LISPALLOC_core__RandomState_O";
}
kind_name_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
return "KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O";
}
kind_name_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
return "KIND_LISPALLOC_llvmo__DebugLoc_O";
}
kind_name_KIND_LISPALLOC_llvmo__Attribute_O:
{
return "KIND_LISPALLOC_llvmo__Attribute_O";
}
kind_name_KIND_LISPALLOC_core__RegexMatch_O:
{
return "KIND_LISPALLOC_core__RegexMatch_O";
}
kind_name_KIND_LISPALLOC_core__WeakPointer_O:
{
return "KIND_LISPALLOC_core__WeakPointer_O";
}
kind_name_KIND_LISPALLOC_core__VaList_dummy_O:
{
return "KIND_LISPALLOC_core__VaList_dummy_O";
}
kind_name_KIND_BOOTSTRAP_core__StandardObject_O:
{
return "KIND_BOOTSTRAP_core__StandardObject_O";
}
kind_name_KIND_BOOTSTRAP_core__Metaobject_O:
{
return "KIND_BOOTSTRAP_core__Metaobject_O";
}
kind_name_KIND_BOOTSTRAP_core__Specializer_O:
{
return "KIND_BOOTSTRAP_core__Specializer_O";
}
kind_name_KIND_BOOTSTRAP_core__Class_O:
{
return "KIND_BOOTSTRAP_core__Class_O";
}
kind_name_KIND_BOOTSTRAP_core__StdClass_O:
{
return "KIND_BOOTSTRAP_core__StdClass_O";
}
kind_name_KIND_BOOTSTRAP_core__StandardClass_O:
{
return "KIND_BOOTSTRAP_core__StandardClass_O";
}
kind_name_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
return "KIND_LISPALLOC_core__FuncallableStandardClass_O";
}
kind_name_KIND_BOOTSTRAP_core__StructureClass_O:
{
return "KIND_BOOTSTRAP_core__StructureClass_O";
}
kind_name_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
return "KIND_LISPALLOC_core__ForwardReferencedClass_O";
}
kind_name_KIND_LISPALLOC_core__CxxClass_O:
{
return "KIND_LISPALLOC_core__CxxClass_O";
}
kind_name_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
return "KIND_BOOTSTRAP_core__BuiltInClass_O";
}
kind_name_KIND_LISPALLOC_clbind__ClassRep_O:
{
return "KIND_LISPALLOC_clbind__ClassRep_O";
}
kind_name_KIND_LISPALLOC_core__ExternalObject_O:
{
return "KIND_LISPALLOC_core__ExternalObject_O";
}
kind_name_KIND_LISPALLOC_llvmo__Value_O:
{
return "KIND_LISPALLOC_llvmo__Value_O";
}
kind_name_KIND_LISPALLOC_llvmo__Argument_O:
{
return "KIND_LISPALLOC_llvmo__Argument_O";
}
kind_name_KIND_LISPALLOC_llvmo__User_O:
{
return "KIND_LISPALLOC_llvmo__User_O";
}
kind_name_KIND_LISPALLOC_llvmo__Instruction_O:
{
return "KIND_LISPALLOC_llvmo__Instruction_O";
}
kind_name_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
return "KIND_LISPALLOC_llvmo__AtomicRMWInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
return "KIND_LISPALLOC_llvmo__LandingPadInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__PHINode_O:
{
return "KIND_LISPALLOC_llvmo__PHINode_O";
}
kind_name_KIND_LISPALLOC_llvmo__CallInst_O:
{
return "KIND_LISPALLOC_llvmo__CallInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__StoreInst_O:
{
return "KIND_LISPALLOC_llvmo__StoreInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
return "KIND_LISPALLOC_llvmo__UnaryInstruction_O";
}
kind_name_KIND_LISPALLOC_llvmo__LoadInst_O:
{
return "KIND_LISPALLOC_llvmo__LoadInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
return "KIND_LISPALLOC_llvmo__AllocaInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
return "KIND_LISPALLOC_llvmo__VAArgInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
return "KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
return "KIND_LISPALLOC_llvmo__TerminatorInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
return "KIND_LISPALLOC_llvmo__UnreachableInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
return "KIND_LISPALLOC_llvmo__SwitchInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
return "KIND_LISPALLOC_llvmo__ReturnInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
return "KIND_LISPALLOC_llvmo__ResumeInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__BranchInst_O:
{
return "KIND_LISPALLOC_llvmo__BranchInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
return "KIND_LISPALLOC_llvmo__InvokeInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
return "KIND_LISPALLOC_llvmo__IndirectBrInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__FenceInst_O:
{
return "KIND_LISPALLOC_llvmo__FenceInst_O";
}
kind_name_KIND_LISPALLOC_llvmo__Constant_O:
{
return "KIND_LISPALLOC_llvmo__Constant_O";
}
kind_name_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
return "KIND_LISPALLOC_llvmo__BlockAddress_O";
}
kind_name_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
return "KIND_LISPALLOC_llvmo__GlobalValue_O";
}
kind_name_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
return "KIND_LISPALLOC_llvmo__GlobalVariable_O";
}
kind_name_KIND_LISPALLOC_llvmo__Function_O:
{
return "KIND_LISPALLOC_llvmo__Function_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
return "KIND_LISPALLOC_llvmo__ConstantArray_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
return "KIND_LISPALLOC_llvmo__ConstantInt_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
return "KIND_LISPALLOC_llvmo__ConstantDataSequential_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
return "KIND_LISPALLOC_llvmo__ConstantDataArray_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
return "KIND_LISPALLOC_llvmo__ConstantStruct_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
return "KIND_LISPALLOC_llvmo__ConstantFP_O";
}
kind_name_KIND_LISPALLOC_llvmo__UndefValue_O:
{
return "KIND_LISPALLOC_llvmo__UndefValue_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
return "KIND_LISPALLOC_llvmo__ConstantPointerNull_O";
}
kind_name_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
return "KIND_LISPALLOC_llvmo__ConstantExpr_O";
}
kind_name_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
return "KIND_LISPALLOC_llvmo__BasicBlock_O";
}
kind_name_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
return "KIND_LISPALLOC_llvmo__IRBuilderBase_O";
}
kind_name_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
return "KIND_LISPALLOC_llvmo__IRBuilder_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
return "KIND_LISPALLOC_llvmo__DIBuilder_O";
}
kind_name_KIND_LISPALLOC_llvmo__Metadata_O:
{
return "KIND_LISPALLOC_llvmo__Metadata_O";
}
kind_name_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
return "KIND_LISPALLOC_llvmo__ValueAsMetadata_O";
}
kind_name_KIND_LISPALLOC_llvmo__MDNode_O:
{
return "KIND_LISPALLOC_llvmo__MDNode_O";
}
kind_name_KIND_LISPALLOC_llvmo__MDString_O:
{
return "KIND_LISPALLOC_llvmo__MDString_O";
}
kind_name_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
return "KIND_LISPALLOC_llvmo__ExecutionEngine_O";
}
kind_name_KIND_LISPALLOC_llvmo__APFloat_O:
{
return "KIND_LISPALLOC_llvmo__APFloat_O";
}
kind_name_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
return "KIND_LISPALLOC_llvmo__PassManagerBuilder_O";
}
kind_name_KIND_LISPALLOC_llvmo__DataLayout_O:
{
return "KIND_LISPALLOC_llvmo__DataLayout_O";
}
kind_name_KIND_LISPALLOC_llvmo__Triple_O:
{
return "KIND_LISPALLOC_llvmo__Triple_O";
}
kind_name_KIND_LISPALLOC_llvmo__APInt_O:
{
return "KIND_LISPALLOC_llvmo__APInt_O";
}
kind_name_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
return "KIND_LISPALLOC_llvmo__PassManagerBase_O";
}
kind_name_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
return "KIND_LISPALLOC_llvmo__FunctionPassManager_O";
}
kind_name_KIND_LISPALLOC_llvmo__PassManager_O:
{
return "KIND_LISPALLOC_llvmo__PassManager_O";
}
kind_name_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
return "KIND_LISPALLOC_llvmo__TargetMachine_O";
}
kind_name_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
return "KIND_LISPALLOC_llvmo__LLVMTargetMachine_O";
}
kind_name_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
return "KIND_LISPALLOC_llvmo__TargetOptions_O";
}
kind_name_KIND_LISPALLOC_llvmo__Type_O:
{
return "KIND_LISPALLOC_llvmo__Type_O";
}
kind_name_KIND_LISPALLOC_llvmo__IntegerType_O:
{
return "KIND_LISPALLOC_llvmo__IntegerType_O";
}
kind_name_KIND_LISPALLOC_llvmo__CompositeType_O:
{
return "KIND_LISPALLOC_llvmo__CompositeType_O";
}
kind_name_KIND_LISPALLOC_llvmo__SequentialType_O:
{
return "KIND_LISPALLOC_llvmo__SequentialType_O";
}
kind_name_KIND_LISPALLOC_llvmo__VectorType_O:
{
return "KIND_LISPALLOC_llvmo__VectorType_O";
}
kind_name_KIND_LISPALLOC_llvmo__PointerType_O:
{
return "KIND_LISPALLOC_llvmo__PointerType_O";
}
kind_name_KIND_LISPALLOC_llvmo__ArrayType_O:
{
return "KIND_LISPALLOC_llvmo__ArrayType_O";
}
kind_name_KIND_LISPALLOC_llvmo__StructType_O:
{
return "KIND_LISPALLOC_llvmo__StructType_O";
}
kind_name_KIND_LISPALLOC_llvmo__FunctionType_O:
{
return "KIND_LISPALLOC_llvmo__FunctionType_O";
}
kind_name_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
return "KIND_LISPALLOC_llvmo__NamedMDNode_O";
}
kind_name_KIND_LISPALLOC_llvmo__Linker_O:
{
return "KIND_LISPALLOC_llvmo__Linker_O";
}
kind_name_KIND_LISPALLOC_llvmo__Pass_O:
{
return "KIND_LISPALLOC_llvmo__Pass_O";
}
kind_name_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
return "KIND_LISPALLOC_llvmo__FunctionPass_O";
}
kind_name_KIND_LISPALLOC_llvmo__ModulePass_O:
{
return "KIND_LISPALLOC_llvmo__ModulePass_O";
}
kind_name_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
return "KIND_LISPALLOC_llvmo__ImmutablePass_O";
}
kind_name_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
return "KIND_LISPALLOC_llvmo__DataLayoutPass_O";
}
kind_name_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
return "KIND_LISPALLOC_llvmo__TargetLibraryInfo_O";
}
kind_name_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
return "KIND_LISPALLOC_llvmo__MCSubtargetInfo_O";
}
kind_name_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
return "KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O";
}
kind_name_KIND_LISPALLOC_llvmo__Module_O:
{
return "KIND_LISPALLOC_llvmo__Module_O";
}
kind_name_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
return "KIND_LISPALLOC_llvmo__EngineBuilder_O";
}
kind_name_KIND_LISPALLOC_core__ForeignData_O:
{
return "KIND_LISPALLOC_core__ForeignData_O";
}
kind_name_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
return "KIND_LISPALLOC_llvmo__LLVMContext_O";
}
kind_name_KIND_LISPALLOC_llvmo__Target_O:
{
return "KIND_LISPALLOC_llvmo__Target_O";
}
kind_name_KIND_LISPALLOC_core__LoadTimeValues_O:
{
return "KIND_LISPALLOC_core__LoadTimeValues_O";
}
kind_name_KIND_LISPALLOC_core__Binder_O:
{
return "KIND_LISPALLOC_core__Binder_O";
}
kind_name_KIND_LISPALLOC_core__IntArray_O:
{
return "KIND_LISPALLOC_core__IntArray_O";
}
kind_name_KIND_LISPALLOC_core__SourceManager_O:
{
return "KIND_LISPALLOC_core__SourceManager_O";
}
kind_name_KIND_LISPALLOC_core__Record_O:
{
return "KIND_LISPALLOC_core__Record_O";
}
kind_name_KIND_LISPALLOC_core__LightUserData_O:
{
return "KIND_LISPALLOC_core__LightUserData_O";
}
kind_name_KIND_LISPALLOC_core__UserData_O:
{
return "KIND_LISPALLOC_core__UserData_O";
}
kind_name_KIND_BOOTSTRAP_core__Symbol_O:
{
return "KIND_BOOTSTRAP_core__Symbol_O";
}
kind_name_KIND_LISPALLOC_core__Null_O:
{
return "KIND_LISPALLOC_core__Null_O";
}
kind_name_KIND_LISPALLOC_core__SourcePosInfo_O:
{
return "KIND_LISPALLOC_core__SourcePosInfo_O";
}
kind_name_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
return "KIND_TEMPLATED_LISPALLOC_core__Iterator_O";
}
kind_name_KIND_LISPALLOC_core__DirectoryIterator_O:
{
return "KIND_LISPALLOC_core__DirectoryIterator_O";
}
kind_name_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
return "KIND_LISPALLOC_core__RecursiveDirectoryIterator_O";
}
kind_name_KIND_LISPALLOC_core__Regex_O:
{
return "KIND_LISPALLOC_core__Regex_O";
}
kind_name_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
return "KIND_LISPALLOC_core__PosixTimeDuration_O";
}
kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
return "KIND_LISPALLOC_core__SymbolToEnumConverter_O";
}
kind_name_KIND_LISPALLOC_core__CandoException_O:
{
return "KIND_LISPALLOC_core__CandoException_O";
}
kind_name_KIND_LISPALLOC_core__Stream_O:
{
return "KIND_LISPALLOC_core__Stream_O";
}
kind_name_KIND_LISPALLOC_core__AnsiStream_O:
{
return "KIND_LISPALLOC_core__AnsiStream_O";
}
kind_name_KIND_LISPALLOC_core__FileStream_O:
{
return "KIND_LISPALLOC_core__FileStream_O";
}
kind_name_KIND_LISPALLOC_core__IOStreamStream_O:
{
return "KIND_LISPALLOC_core__IOStreamStream_O";
}
kind_name_KIND_LISPALLOC_core__IOFileStream_O:
{
return "KIND_LISPALLOC_core__IOFileStream_O";
}
kind_name_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
return "KIND_LISPALLOC_core__ConcatenatedStream_O";
}
kind_name_KIND_LISPALLOC_core__StringStream_O:
{
return "KIND_LISPALLOC_core__StringStream_O";
}
kind_name_KIND_LISPALLOC_core__StringInputStream_O:
{
return "KIND_LISPALLOC_core__StringInputStream_O";
}
kind_name_KIND_LISPALLOC_core__StringOutputStream_O:
{
return "KIND_LISPALLOC_core__StringOutputStream_O";
}
kind_name_KIND_LISPALLOC_core__SynonymStream_O:
{
return "KIND_LISPALLOC_core__SynonymStream_O";
}
kind_name_KIND_LISPALLOC_core__EchoStream_O:
{
return "KIND_LISPALLOC_core__EchoStream_O";
}
kind_name_KIND_LISPALLOC_core__TwoWayStream_O:
{
return "KIND_LISPALLOC_core__TwoWayStream_O";
}
kind_name_KIND_LISPALLOC_core__BroadcastStream_O:
{
return "KIND_LISPALLOC_core__BroadcastStream_O";
}
kind_name_KIND_LISPALLOC_core__Reader_O:
{
return "KIND_LISPALLOC_core__Reader_O";
}
kind_name_KIND_LISPALLOC_core__Cons_O:
{
return "KIND_LISPALLOC_core__Cons_O";
}
kind_name_KIND_LISPALLOC_core__Archive_O:
{
return "KIND_LISPALLOC_core__Archive_O";
}
kind_name_KIND_LISPALLOC_core__SaveArchive_O:
{
return "KIND_LISPALLOC_core__SaveArchive_O";
}
kind_name_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
return "KIND_LISPALLOC_core__SexpSaveArchive_O";
}
kind_name_KIND_LISPALLOC_core__LoadArchive_O:
{
return "KIND_LISPALLOC_core__LoadArchive_O";
}
kind_name_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
return "KIND_LISPALLOC_core__SexpLoadArchive_O";
}
kind_name_KIND_LISPALLOC_core__HashTable_O:
{
return "KIND_LISPALLOC_core__HashTable_O";
}
kind_name_KIND_LISPALLOC_core__HashTableEq_O:
{
return "KIND_LISPALLOC_core__HashTableEq_O";
}
kind_name_KIND_LISPALLOC_core__HashTableEqualp_O:
{
return "KIND_LISPALLOC_core__HashTableEqualp_O";
}
kind_name_KIND_LISPALLOC_core__HashTableEql_O:
{
return "KIND_LISPALLOC_core__HashTableEql_O";
}
kind_name_KIND_LISPALLOC_core__HashTableEqual_O:
{
return "KIND_LISPALLOC_core__HashTableEqual_O";
}
kind_name_KIND_LISPALLOC_cffi__Pointer_O:
{
return "KIND_LISPALLOC_cffi__Pointer_O";
}
kind_name_KIND_LISPALLOC_core__CxxObject_O:
{
return "KIND_LISPALLOC_core__CxxObject_O";
}
kind_name_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
return "KIND_LISPALLOC_core__WeakKeyMapping_O";
}
kind_name_KIND_LISPALLOC_core__LambdaListHandler_O:
{
return "KIND_LISPALLOC_core__LambdaListHandler_O";
}
kind_name_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
return "KIND_LISPALLOC_llvmo__InsertPoint_O";
}
kind_name_KIND_LISPALLOC_core__SourceFileInfo_O:
{
return "KIND_LISPALLOC_core__SourceFileInfo_O";
}
kind_name_KIND_LISPALLOC_core__SNode_O:
{
return "KIND_LISPALLOC_core__SNode_O";
}
kind_name_KIND_LISPALLOC_core__LeafSNode_O:
{
return "KIND_LISPALLOC_core__LeafSNode_O";
}
kind_name_KIND_LISPALLOC_core__BranchSNode_O:
{
return "KIND_LISPALLOC_core__BranchSNode_O";
}
kind_name_KIND_LISPALLOC_core__Path_O:
{
return "KIND_LISPALLOC_core__Path_O";
}
kind_name_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
return "KIND_LISPALLOC_asttooling__AstVisitor_O";
}
kind_name_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
return "KIND_LISPALLOC_llvmo__AttributeSet_O";
}
kind_name_KIND_LISPALLOC_core__StructureObject_O:
{
return "KIND_LISPALLOC_core__StructureObject_O";
}
kind_name_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
return "KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O";
}
kind_name_KIND_LISPALLOC_core__Package_O:
{
return "KIND_LISPALLOC_core__Package_O";
}
kind_name_KIND_LISPALLOC_core__DirectoryEntry_O:
{
return "KIND_LISPALLOC_core__DirectoryEntry_O";
}
kind_name_KIND_LISPALLOC_core__Character_dummy_O:
{
return "KIND_LISPALLOC_core__Character_dummy_O";
}
kind_name_KIND_LISPALLOC_core__Function_O:
{
return "KIND_LISPALLOC_core__Function_O";
}
kind_name_KIND_LISPALLOC_core__CompiledFunction_O:
{
return "KIND_LISPALLOC_core__CompiledFunction_O";
}
kind_name_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
return "KIND_LISPALLOC_core__SingleDispatchGenericFunction_O";
}
kind_name_KIND_LISPALLOC_core__SpecialForm_O:
{
return "KIND_LISPALLOC_core__SpecialForm_O";
}
kind_name_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
return "KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O";
}
kind_name_KIND_LISPALLOC_core__Instance_O:
{
return "KIND_LISPALLOC_core__Instance_O";
}
kind_name_KIND_LISPALLOC_core__Pointer_O:
{
return "KIND_LISPALLOC_core__Pointer_O";
}
kind_name_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
return "KIND_LISPALLOC_clbind__ClassRegistry_O";
}
kind_name_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
return "KIND_LISPALLOC_llvmo__DebugInfo_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
return "KIND_LISPALLOC_llvmo__DIDerivedType_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIArray_O:
{
return "KIND_LISPALLOC_llvmo__DIArray_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
return "KIND_LISPALLOC_llvmo__DIBasicType_O";
}
kind_name_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
return "KIND_LISPALLOC_llvmo__DISubprogram_O";
}
kind_name_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
return "KIND_LISPALLOC_llvmo__DILexicalBlock_O";
}
kind_name_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
return "KIND_LISPALLOC_llvmo__DICompileUnit_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
return "KIND_LISPALLOC_llvmo__DIDescriptor_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIType_O:
{
return "KIND_LISPALLOC_llvmo__DIType_O";
}
kind_name_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
return "KIND_LISPALLOC_llvmo__DISubroutineType_O";
}
kind_name_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
return "KIND_LISPALLOC_llvmo__DICompositeType_O";
}
kind_name_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
return "KIND_LISPALLOC_llvmo__DITypeArray_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIFile_O:
{
return "KIND_LISPALLOC_llvmo__DIFile_O";
}
kind_name_KIND_LISPALLOC_llvmo__DIScope_O:
{
return "KIND_LISPALLOC_llvmo__DIScope_O";
}
kind_name_KIND_LISPALLOC_core__SmallMultimap_O:
{
return "KIND_LISPALLOC_core__SmallMultimap_O";
}
kind_name_KIND_LISPALLOC_core__Pathname_O:
{
return "KIND_LISPALLOC_core__Pathname_O";
}
kind_name_KIND_LISPALLOC_core__LogicalPathname_O:
{
return "KIND_LISPALLOC_core__LogicalPathname_O";
}
kind_name_KIND_LISPALLOC_core__PosixTime_O:
{
return "KIND_LISPALLOC_core__PosixTime_O";
}
kind_name_KIND_LISPALLOC_core__SmallMap_O:
{
return "KIND_LISPALLOC_core__SmallMap_O";
}
kind_name_KIND_CLASSALLOC_core__Cache:
{
return "KIND_CLASSALLOC_core__Cache";
}
kind_name_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
return "KIND_ROOTCLASSALLOC_core__Lisp_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_";
}
kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
return "KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_";
}
kind_name_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure:
{
return "KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__";
}
kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
return "KIND_GCSTRING_gctools__GCString_moveable_char_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_";
}
kind_name_KIND_CLASSALLOC_llvmo__CompiledClosure:
{
return "KIND_CLASSALLOC_llvmo__CompiledClosure";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__";
}
kind_name_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor:
{
return "KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___";
}
kind_name_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor:
{
return "KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_";
}
kind_name_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor:
{
return "KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_";
}
kind_name_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor:
{
return "KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor";
}
kind_name_KIND_CLASSALLOC_core__MacroClosure:
{
return "KIND_CLASSALLOC_core__MacroClosure";
}
kind_name_KIND_CLASSALLOC_core__ConsStepper:
{
return "KIND_CLASSALLOC_core__ConsStepper";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_";
}
kind_name_KIND_CLASSALLOC_core__InstanceClosure:
{
return "KIND_CLASSALLOC_core__InstanceClosure";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__";
}
kind_name_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
return "KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory";
}
kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
return "KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_";
}
kind_name_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
return "KIND_LISPALLOC_asttooling__DerivableMatchCallback";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_";
}
kind_name_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
return "KIND_LISPALLOC_asttooling__DerivableASTFrontendAction";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__";
}
kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
return "KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_";
}
kind_name_KIND_CLASSALLOC_core__CoreExposer:
{
return "KIND_CLASSALLOC_core__CoreExposer";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__";
}
kind_name_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
return "KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__";
}
kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
return "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_";
}
kind_name_KIND_CLASSALLOC_core__InterpretedClosure:
{
return "KIND_CLASSALLOC_core__InterpretedClosure";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_";
}
kind_name_KIND_CLASSALLOC_core__VectorStepper:
{
return "KIND_CLASSALLOC_core__VectorStepper";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__";
}
#endif // defined(GC_KIND_NAME_MAP)
#if defined(GC_KIND_NAME_MAP_HELPERS)

#endif // defined(GC_KIND_NAME_MAP_HELPERS)
#if defined(GC_KIND_NAME_MAP_TABLE)
static void* KIND_NAME_MAP_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&kind_name_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps
  /* 5 */ , &&kind_name_KIND_ROOTCLASSALLOC_clbind__detail__class_map
  /* 6 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 7 */ , &&kind_name_KIND_CLASSALLOC_clbind__DummyCreator
  /* 8 */ , &&kind_name_KIND_CLASSALLOC_core__InstanceCreator
  /* 9 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator
  /* 10 */ , &&kind_name_KIND_BOOTSTRAP_core__T_O
  /* 11 */ , &&kind_name_KIND_LISPALLOC_core__MultiStringBuffer_O
  /* 12 */ , &&kind_name_KIND_LISPALLOC_core__ReadTable_O
  /* 13 */ , &&kind_name_KIND_LISPALLOC_core__Number_O
  /* 14 */ , &&kind_name_KIND_LISPALLOC_core__Complex_O
  /* 15 */ , &&kind_name_KIND_LISPALLOC_core__Real_O
  /* 16 */ , &&kind_name_KIND_LISPALLOC_core__Rational_O
  /* 17 */ , &&kind_name_KIND_LISPALLOC_core__Integer_O
  /* 18 */ , &&kind_name_KIND_LISPALLOC_core__Bignum_O
  /* 19 */ , &&kind_name_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 20 */ , &&kind_name_KIND_LISPALLOC_core__Ratio_O
  /* 21 */ , &&kind_name_KIND_LISPALLOC_core__Float_O
  /* 22 */ , &&kind_name_KIND_LISPALLOC_core__DoubleFloat_O
  /* 23 */ , &&kind_name_KIND_LISPALLOC_core__LongFloat_O
  /* 24 */ , &&kind_name_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 25 */ , &&kind_name_KIND_LISPALLOC_core__ShortFloat_O
  /* 26 */ , &&kind_name_KIND_LISPALLOC_core__FileStatus_O
  /* 27 */ , &&kind_name_KIND_LISPALLOC_core__WeakHashTable_O
  /* 28 */ , &&kind_name_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 29 */ , &&kind_name_KIND_LISPALLOC_core__Environment_O
  /* 30 */ , &&kind_name_KIND_LISPALLOC_core__ActivationFrame_O
  /* 31 */ , &&kind_name_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 32 */ , &&kind_name_KIND_LISPALLOC_core__ValueFrame_O
  /* 33 */ , &&kind_name_KIND_LISPALLOC_core__FunctionFrame_O
  /* 34 */ , &&kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 35 */ , &&kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 36 */ , &&kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 37 */ , &&kind_name_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 38 */ , &&kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 39 */ , &&kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 40 */ , &&kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 41 */ , &&kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 42 */ , &&kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 43 */ , &&kind_name_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 44 */ , &&kind_name_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 45 */ , &&kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 46 */ , &&kind_name_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 47 */ , &&kind_name_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 48 */ , &&kind_name_KIND_LISPALLOC_core__Array_O
  /* 49 */ , &&kind_name_KIND_LISPALLOC_core__ArrayObjects_O
  /* 50 */ , &&kind_name_KIND_LISPALLOC_core__ArrayDisplaced_O
  /* 51 */ , &&kind_name_KIND_LISPALLOC_core__Vector_O
  /* 52 */ , &&kind_name_KIND_LISPALLOC_core__BitVector_O
  /* 53 */ , &&kind_name_KIND_LISPALLOC_core__SimpleBitVector_O
  /* 54 */ , &&kind_name_KIND_LISPALLOC_core__BitVectorWithFillPtr_O
  /* 55 */ , &&kind_name_KIND_LISPALLOC_core__VectorDisplaced_O
  /* 56 */ , &&kind_name_KIND_LISPALLOC_core__String_O
  /* 57 */ , &&kind_name_KIND_BOOTSTRAP_core__Str_O
  /* 58 */ , &&kind_name_KIND_LISPALLOC_core__StrWithFillPtr_O
  /* 59 */ , &&kind_name_KIND_LISPALLOC_core__VectorObjects_O
  /* 60 */ , &&kind_name_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 61 */ , &&kind_name_KIND_LISPALLOC_core__SingleDispatchMethod_O
  /* 62 */ , &&kind_name_KIND_LISPALLOC_core__RandomState_O
  /* 63 */ , &&kind_name_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O
  /* 64 */ , &&kind_name_KIND_LISPALLOC_llvmo__DebugLoc_O
  /* 65 */ , &&kind_name_KIND_LISPALLOC_llvmo__Attribute_O
  /* 66 */ , &&kind_name_KIND_LISPALLOC_core__RegexMatch_O
  /* 67 */ , &&kind_name_KIND_LISPALLOC_core__WeakPointer_O
  /* 68 */ , &&kind_name_KIND_LISPALLOC_core__VaList_dummy_O
  /* 69 */ , &&kind_name_KIND_BOOTSTRAP_core__StandardObject_O
  /* 70 */ , &&kind_name_KIND_BOOTSTRAP_core__Metaobject_O
  /* 71 */ , &&kind_name_KIND_BOOTSTRAP_core__Specializer_O
  /* 72 */ , &&kind_name_KIND_BOOTSTRAP_core__Class_O
  /* 73 */ , &&kind_name_KIND_BOOTSTRAP_core__StdClass_O
  /* 74 */ , &&kind_name_KIND_BOOTSTRAP_core__StandardClass_O
  /* 75 */ , &&kind_name_KIND_LISPALLOC_core__FuncallableStandardClass_O
  /* 76 */ , &&kind_name_KIND_BOOTSTRAP_core__StructureClass_O
  /* 77 */ , &&kind_name_KIND_LISPALLOC_core__ForwardReferencedClass_O
  /* 78 */ , &&kind_name_KIND_LISPALLOC_core__CxxClass_O
  /* 79 */ , &&kind_name_KIND_BOOTSTRAP_core__BuiltInClass_O
  /* 80 */ , &&kind_name_KIND_LISPALLOC_clbind__ClassRep_O
  /* 81 */ , &&kind_name_KIND_LISPALLOC_core__ExternalObject_O
  /* 82 */ , &&kind_name_KIND_LISPALLOC_llvmo__Value_O
  /* 83 */ , &&kind_name_KIND_LISPALLOC_llvmo__Argument_O
  /* 84 */ , &&kind_name_KIND_LISPALLOC_llvmo__User_O
  /* 85 */ , &&kind_name_KIND_LISPALLOC_llvmo__Instruction_O
  /* 86 */ , &&kind_name_KIND_LISPALLOC_llvmo__AtomicRMWInst_O
  /* 87 */ , &&kind_name_KIND_LISPALLOC_llvmo__LandingPadInst_O
  /* 88 */ , &&kind_name_KIND_LISPALLOC_llvmo__PHINode_O
  /* 89 */ , &&kind_name_KIND_LISPALLOC_llvmo__CallInst_O
  /* 90 */ , &&kind_name_KIND_LISPALLOC_llvmo__StoreInst_O
  /* 91 */ , &&kind_name_KIND_LISPALLOC_llvmo__UnaryInstruction_O
  /* 92 */ , &&kind_name_KIND_LISPALLOC_llvmo__LoadInst_O
  /* 93 */ , &&kind_name_KIND_LISPALLOC_llvmo__AllocaInst_O
  /* 94 */ , &&kind_name_KIND_LISPALLOC_llvmo__VAArgInst_O
  /* 95 */ , &&kind_name_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O
  /* 96 */ , &&kind_name_KIND_LISPALLOC_llvmo__TerminatorInst_O
  /* 97 */ , &&kind_name_KIND_LISPALLOC_llvmo__UnreachableInst_O
  /* 98 */ , &&kind_name_KIND_LISPALLOC_llvmo__SwitchInst_O
  /* 99 */ , &&kind_name_KIND_LISPALLOC_llvmo__ReturnInst_O
  /* 100 */ , &&kind_name_KIND_LISPALLOC_llvmo__ResumeInst_O
  /* 101 */ , &&kind_name_KIND_LISPALLOC_llvmo__BranchInst_O
  /* 102 */ , &&kind_name_KIND_LISPALLOC_llvmo__InvokeInst_O
  /* 103 */ , &&kind_name_KIND_LISPALLOC_llvmo__IndirectBrInst_O
  /* 104 */ , &&kind_name_KIND_LISPALLOC_llvmo__FenceInst_O
  /* 105 */ , &&kind_name_KIND_LISPALLOC_llvmo__Constant_O
  /* 106 */ , &&kind_name_KIND_LISPALLOC_llvmo__BlockAddress_O
  /* 107 */ , &&kind_name_KIND_LISPALLOC_llvmo__GlobalValue_O
  /* 108 */ , &&kind_name_KIND_LISPALLOC_llvmo__GlobalVariable_O
  /* 109 */ , &&kind_name_KIND_LISPALLOC_llvmo__Function_O
  /* 110 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantArray_O
  /* 111 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantInt_O
  /* 112 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantDataSequential_O
  /* 113 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantDataArray_O
  /* 114 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantStruct_O
  /* 115 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantFP_O
  /* 116 */ , &&kind_name_KIND_LISPALLOC_llvmo__UndefValue_O
  /* 117 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantPointerNull_O
  /* 118 */ , &&kind_name_KIND_LISPALLOC_llvmo__ConstantExpr_O
  /* 119 */ , &&kind_name_KIND_LISPALLOC_llvmo__BasicBlock_O
  /* 120 */ , &&kind_name_KIND_LISPALLOC_llvmo__IRBuilderBase_O
  /* 121 */ , &&kind_name_KIND_LISPALLOC_llvmo__IRBuilder_O
  /* 122 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIBuilder_O
  /* 123 */ , &&kind_name_KIND_LISPALLOC_llvmo__Metadata_O
  /* 124 */ , &&kind_name_KIND_LISPALLOC_llvmo__ValueAsMetadata_O
  /* 125 */ , &&kind_name_KIND_LISPALLOC_llvmo__MDNode_O
  /* 126 */ , &&kind_name_KIND_LISPALLOC_llvmo__MDString_O
  /* 127 */ , &&kind_name_KIND_LISPALLOC_llvmo__ExecutionEngine_O
  /* 128 */ , &&kind_name_KIND_LISPALLOC_llvmo__APFloat_O
  /* 129 */ , &&kind_name_KIND_LISPALLOC_llvmo__PassManagerBuilder_O
  /* 130 */ , &&kind_name_KIND_LISPALLOC_llvmo__DataLayout_O
  /* 131 */ , &&kind_name_KIND_LISPALLOC_llvmo__Triple_O
  /* 132 */ , &&kind_name_KIND_LISPALLOC_llvmo__APInt_O
  /* 133 */ , &&kind_name_KIND_LISPALLOC_llvmo__PassManagerBase_O
  /* 134 */ , &&kind_name_KIND_LISPALLOC_llvmo__FunctionPassManager_O
  /* 135 */ , &&kind_name_KIND_LISPALLOC_llvmo__PassManager_O
  /* 136 */ , &&kind_name_KIND_LISPALLOC_llvmo__TargetMachine_O
  /* 137 */ , &&kind_name_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O
  /* 138 */ , &&kind_name_KIND_LISPALLOC_llvmo__TargetOptions_O
  /* 139 */ , &&kind_name_KIND_LISPALLOC_llvmo__Type_O
  /* 140 */ , &&kind_name_KIND_LISPALLOC_llvmo__IntegerType_O
  /* 141 */ , &&kind_name_KIND_LISPALLOC_llvmo__CompositeType_O
  /* 142 */ , &&kind_name_KIND_LISPALLOC_llvmo__SequentialType_O
  /* 143 */ , &&kind_name_KIND_LISPALLOC_llvmo__VectorType_O
  /* 144 */ , &&kind_name_KIND_LISPALLOC_llvmo__PointerType_O
  /* 145 */ , &&kind_name_KIND_LISPALLOC_llvmo__ArrayType_O
  /* 146 */ , &&kind_name_KIND_LISPALLOC_llvmo__StructType_O
  /* 147 */ , &&kind_name_KIND_LISPALLOC_llvmo__FunctionType_O
  /* 148 */ , &&kind_name_KIND_LISPALLOC_llvmo__NamedMDNode_O
  /* 149 */ , &&kind_name_KIND_LISPALLOC_llvmo__Linker_O
  /* 150 */ , &&kind_name_KIND_LISPALLOC_llvmo__Pass_O
  /* 151 */ , &&kind_name_KIND_LISPALLOC_llvmo__FunctionPass_O
  /* 152 */ , &&kind_name_KIND_LISPALLOC_llvmo__ModulePass_O
  /* 153 */ , &&kind_name_KIND_LISPALLOC_llvmo__ImmutablePass_O
  /* 154 */ , &&kind_name_KIND_LISPALLOC_llvmo__DataLayoutPass_O
  /* 155 */ , &&kind_name_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O
  /* 156 */ , &&kind_name_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O
  /* 157 */ , &&kind_name_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O
  /* 158 */ , &&kind_name_KIND_LISPALLOC_llvmo__Module_O
  /* 159 */ , &&kind_name_KIND_LISPALLOC_llvmo__EngineBuilder_O
  /* 160 */ , &&kind_name_KIND_LISPALLOC_core__ForeignData_O
  /* 161 */ , &&kind_name_KIND_LISPALLOC_llvmo__LLVMContext_O
  /* 162 */ , &&kind_name_KIND_LISPALLOC_llvmo__Target_O
  /* 163 */ , &&kind_name_KIND_LISPALLOC_core__LoadTimeValues_O
  /* 164 */ , &&kind_name_KIND_LISPALLOC_core__Binder_O
  /* 165 */ , &&kind_name_KIND_LISPALLOC_core__IntArray_O
  /* 166 */ , &&kind_name_KIND_LISPALLOC_core__SourceManager_O
  /* 167 */ , &&kind_name_KIND_LISPALLOC_core__Record_O
  /* 168 */ , &&kind_name_KIND_LISPALLOC_core__LightUserData_O
  /* 169 */ , &&kind_name_KIND_LISPALLOC_core__UserData_O
  /* 170 */ , &&kind_name_KIND_BOOTSTRAP_core__Symbol_O
  /* 171 */ , &&kind_name_KIND_LISPALLOC_core__Null_O
  /* 172 */ , &&kind_name_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 173 */ , &&kind_name_KIND_TEMPLATED_LISPALLOC_core__Iterator_O
  /* 174 */ , &&kind_name_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 175 */ , &&kind_name_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 176 */ , &&kind_name_KIND_LISPALLOC_core__Regex_O
  /* 177 */ , &&kind_name_KIND_LISPALLOC_core__PosixTimeDuration_O
  /* 178 */ , &&kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 179 */ , &&kind_name_KIND_LISPALLOC_core__CandoException_O
  /* 180 */ , &&kind_name_KIND_LISPALLOC_core__Stream_O
  /* 181 */ , &&kind_name_KIND_LISPALLOC_core__AnsiStream_O
  /* 182 */ , &&kind_name_KIND_LISPALLOC_core__FileStream_O
  /* 183 */ , &&kind_name_KIND_LISPALLOC_core__IOStreamStream_O
  /* 184 */ , &&kind_name_KIND_LISPALLOC_core__IOFileStream_O
  /* 185 */ , &&kind_name_KIND_LISPALLOC_core__ConcatenatedStream_O
  /* 186 */ , &&kind_name_KIND_LISPALLOC_core__StringStream_O
  /* 187 */ , &&kind_name_KIND_LISPALLOC_core__StringInputStream_O
  /* 188 */ , &&kind_name_KIND_LISPALLOC_core__StringOutputStream_O
  /* 189 */ , &&kind_name_KIND_LISPALLOC_core__SynonymStream_O
  /* 190 */ , &&kind_name_KIND_LISPALLOC_core__EchoStream_O
  /* 191 */ , &&kind_name_KIND_LISPALLOC_core__TwoWayStream_O
  /* 192 */ , &&kind_name_KIND_LISPALLOC_core__BroadcastStream_O
  /* 193 */ , &&kind_name_KIND_LISPALLOC_core__Reader_O
  /* 194 */ , &&kind_name_KIND_LISPALLOC_core__Cons_O
  /* 195 */ , &&kind_name_KIND_LISPALLOC_core__Archive_O
  /* 196 */ , &&kind_name_KIND_LISPALLOC_core__SaveArchive_O
  /* 197 */ , &&kind_name_KIND_LISPALLOC_core__SexpSaveArchive_O
  /* 198 */ , &&kind_name_KIND_LISPALLOC_core__LoadArchive_O
  /* 199 */ , &&kind_name_KIND_LISPALLOC_core__SexpLoadArchive_O
  /* 200 */ , &&kind_name_KIND_LISPALLOC_core__HashTable_O
  /* 201 */ , &&kind_name_KIND_LISPALLOC_core__HashTableEq_O
  /* 202 */ , &&kind_name_KIND_LISPALLOC_core__HashTableEqualp_O
  /* 203 */ , &&kind_name_KIND_LISPALLOC_core__HashTableEql_O
  /* 204 */ , &&kind_name_KIND_LISPALLOC_core__HashTableEqual_O
  /* 205 */ , &&kind_name_KIND_LISPALLOC_cffi__Pointer_O
  /* 206 */ , &&kind_name_KIND_LISPALLOC_core__CxxObject_O
  /* 207 */ , &&kind_name_KIND_LISPALLOC_core__WeakKeyMapping_O
  /* 208 */ , &&kind_name_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 209 */ , &&kind_name_KIND_LISPALLOC_llvmo__InsertPoint_O
  /* 210 */ , &&kind_name_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 211 */ , &&kind_name_KIND_LISPALLOC_core__SNode_O
  /* 212 */ , &&kind_name_KIND_LISPALLOC_core__LeafSNode_O
  /* 213 */ , &&kind_name_KIND_LISPALLOC_core__BranchSNode_O
  /* 214 */ , &&kind_name_KIND_LISPALLOC_core__Path_O
  /* 215 */ , &&kind_name_KIND_LISPALLOC_asttooling__AstVisitor_O
  /* 216 */ , &&kind_name_KIND_LISPALLOC_llvmo__AttributeSet_O
  /* 217 */ , &&kind_name_KIND_LISPALLOC_core__StructureObject_O
  /* 218 */ , &&kind_name_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 219 */ , &&kind_name_KIND_LISPALLOC_core__Package_O
  /* 220 */ , &&kind_name_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 221 */ , &&kind_name_KIND_LISPALLOC_core__Character_dummy_O
  /* 222 */ , &&kind_name_KIND_LISPALLOC_core__Function_O
  /* 223 */ , &&kind_name_KIND_LISPALLOC_core__CompiledFunction_O
  /* 224 */ , &&kind_name_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O
  /* 225 */ , &&kind_name_KIND_LISPALLOC_core__SpecialForm_O
  /* 226 */ , &&kind_name_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O
  /* 227 */ , &&kind_name_KIND_LISPALLOC_core__Instance_O
  /* 228 */ , &&kind_name_KIND_LISPALLOC_core__Pointer_O
  /* 229 */ , &&kind_name_KIND_LISPALLOC_clbind__ClassRegistry_O
  /* 230 */ , &&kind_name_KIND_LISPALLOC_llvmo__DebugInfo_O
  /* 231 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIDerivedType_O
  /* 232 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIArray_O
  /* 233 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIBasicType_O
  /* 234 */ , &&kind_name_KIND_LISPALLOC_llvmo__DISubprogram_O
  /* 235 */ , &&kind_name_KIND_LISPALLOC_llvmo__DILexicalBlock_O
  /* 236 */ , &&kind_name_KIND_LISPALLOC_llvmo__DICompileUnit_O
  /* 237 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIDescriptor_O
  /* 238 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIType_O
  /* 239 */ , &&kind_name_KIND_LISPALLOC_llvmo__DISubroutineType_O
  /* 240 */ , &&kind_name_KIND_LISPALLOC_llvmo__DICompositeType_O
  /* 241 */ , &&kind_name_KIND_LISPALLOC_llvmo__DITypeArray_O
  /* 242 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIFile_O
  /* 243 */ , &&kind_name_KIND_LISPALLOC_llvmo__DIScope_O
  /* 244 */ , &&kind_name_KIND_LISPALLOC_core__SmallMultimap_O
  /* 245 */ , &&kind_name_KIND_LISPALLOC_core__Pathname_O
  /* 246 */ , &&kind_name_KIND_LISPALLOC_core__LogicalPathname_O
  /* 247 */ , &&kind_name_KIND_LISPALLOC_core__PosixTime_O
  /* 248 */ , &&kind_name_KIND_LISPALLOC_core__SmallMap_O
  /* 249 */ , &&kind_name_KIND_CLASSALLOC_core__Cache
  /* 250 */ , &&kind_name_KIND_ROOTCLASSALLOC_core__Lisp_O
  /* 251 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 252 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 253 */ , &&kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 254 */ , &&kind_name_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure
  /* 255 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 256 */ , &&kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 257 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 258 */ , &&kind_name_KIND_CLASSALLOC_llvmo__CompiledClosure
  /* 259 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 260 */ , &&kind_name_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor
  /* 261 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___
  /* 262 */ , &&kind_name_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor
  /* 263 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_
  /* 264 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_
  /* 265 */ , &&kind_name_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor
  /* 266 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_
  /* 267 */ , &&kind_name_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor
  /* 268 */ , &&kind_name_KIND_CLASSALLOC_core__MacroClosure
  /* 269 */ , &&kind_name_KIND_CLASSALLOC_core__ConsStepper
  /* 270 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 271 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_
  /* 272 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 273 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 274 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_
  /* 275 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___
  /* 276 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 277 */ , &&kind_name_KIND_CLASSALLOC_core__InstanceClosure
  /* 278 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 279 */ , &&kind_name_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory
  /* 280 */ , &&kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_
  /* 281 */ , &&kind_name_KIND_LISPALLOC_asttooling__DerivableMatchCallback
  /* 282 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_
  /* 283 */ , &&kind_name_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction
  /* 284 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_
  /* 285 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__
  /* 286 */ , &&kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_
  /* 287 */ , &&kind_name_KIND_CLASSALLOC_core__CoreExposer
  /* 288 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 289 */ , &&kind_name_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction
  /* 290 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__
  /* 291 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 292 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 293 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 294 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 295 */ , &&kind_name_KIND_CLASSALLOC_core__InterpretedClosure
  /* 296 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 297 */ , &&kind_name_KIND_CLASSALLOC_core__VectorStepper
  /* 298 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__
};
#endif // defined(GC_KIND_NAME_MAP_TABLE)
#if defined(GC_OBJ_DUMP_MAP)
obj_dump_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps:
{
    typedef asttooling::RegMap::RegistryMaps type_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps;
    sout << "KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps size[" << (AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
    typedef clbind::detail::class_map type_KIND_ROOTCLASSALLOC_clbind__detail__class_map;
    sout << "KIND_ROOTCLASSALLOC_clbind__detail__class_map size[" << (AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_clbind__detail__class_map))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_core__Creator size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_clbind__DummyCreator:
{
    typedef clbind::DummyCreator type_KIND_CLASSALLOC_clbind__DummyCreator;
    sout << "KIND_CLASSALLOC_clbind__DummyCreator size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_clbind__DummyCreator))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__InstanceCreator:
{
    typedef core::InstanceCreator type_KIND_CLASSALLOC_core__InstanceCreator;
    sout << "KIND_CLASSALLOC_core__InstanceCreator size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__InstanceCreator))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__T_O:
{
    typedef core::T_O type_KIND_BOOTSTRAP_core__T_O;
    sout << "KIND_BOOTSTRAP_core__T_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__T_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
    typedef core::MultiStringBuffer_O type_KIND_LISPALLOC_core__MultiStringBuffer_O;
    sout << "KIND_LISPALLOC_core__MultiStringBuffer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__MultiStringBuffer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ReadTable_O:
{
    typedef core::ReadTable_O type_KIND_LISPALLOC_core__ReadTable_O;
    sout << "KIND_LISPALLOC_core__ReadTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ReadTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    sout << "KIND_LISPALLOC_core__Number_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Complex_O:
{
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    sout << "KIND_LISPALLOC_core__Complex_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Real_O:
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    sout << "KIND_LISPALLOC_core__Real_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Rational_O:
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    sout << "KIND_LISPALLOC_core__Rational_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    sout << "KIND_LISPALLOC_core__Integer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Bignum_O:
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    sout << "KIND_LISPALLOC_core__Bignum_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    sout << "KIND_LISPALLOC_core__Fixnum_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Ratio_O:
{
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    sout << "KIND_LISPALLOC_core__Ratio_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    sout << "KIND_LISPALLOC_core__Float_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    sout << "KIND_LISPALLOC_core__DoubleFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    sout << "KIND_LISPALLOC_core__LongFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    sout << "KIND_LISPALLOC_core__SingleFloat_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ShortFloat_O:
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    sout << "KIND_LISPALLOC_core__ShortFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FileStatus_O:
{
    typedef core::FileStatus_O type_KIND_LISPALLOC_core__FileStatus_O;
    sout << "KIND_LISPALLOC_core__FileStatus_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStatus_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakHashTable_O:
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    sout << "KIND_LISPALLOC_core__WeakHashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    sout << "KIND_LISPALLOC_core__WeakKeyHashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    sout << "KIND_LISPALLOC_core__Environment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ActivationFrame_O:
{
    typedef core::ActivationFrame_O type_KIND_LISPALLOC_core__ActivationFrame_O;
    sout << "KIND_LISPALLOC_core__ActivationFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ActivationFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    sout << "KIND_LISPALLOC_core__TagbodyFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ValueFrame_O:
{
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    sout << "KIND_LISPALLOC_core__ValueFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionFrame_O:
{
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    sout << "KIND_LISPALLOC_core__FunctionFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    sout << "KIND_LISPALLOC_core__LexicalEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    sout << "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__FunctionValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__ValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    sout << "KIND_LISPALLOC_core__TagbodyEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    sout << "KIND_LISPALLOC_core__CompileTimeEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    sout << "KIND_LISPALLOC_core__UnwindProtectEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    sout << "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    sout << "KIND_LISPALLOC_core__FunctionContainerEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    typedef core::StackValueEnvironment_O type_KIND_LISPALLOC_core__StackValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__StackValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StackValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    sout << "KIND_LISPALLOC_core__BlockEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    sout << "KIND_LISPALLOC_core__MacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    sout << "KIND_LISPALLOC_core__CatchEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    sout << "KIND_LISPALLOC_core__GlueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Array_O:
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    sout << "KIND_LISPALLOC_core__Array_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ArrayObjects_O:
{
    typedef core::ArrayObjects_O type_KIND_LISPALLOC_core__ArrayObjects_O;
    sout << "KIND_LISPALLOC_core__ArrayObjects_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ArrayObjects_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
    typedef core::ArrayDisplaced_O type_KIND_LISPALLOC_core__ArrayDisplaced_O;
    sout << "KIND_LISPALLOC_core__ArrayDisplaced_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ArrayDisplaced_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    sout << "KIND_LISPALLOC_core__Vector_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__BitVector_O:
{
    typedef core::BitVector_O type_KIND_LISPALLOC_core__BitVector_O;
    sout << "KIND_LISPALLOC_core__BitVector_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BitVector_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SimpleBitVector_O:
{
    typedef core::SimpleBitVector_O type_KIND_LISPALLOC_core__SimpleBitVector_O;
    sout << "KIND_LISPALLOC_core__SimpleBitVector_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SimpleBitVector_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
    typedef core::BitVectorWithFillPtr_O type_KIND_LISPALLOC_core__BitVectorWithFillPtr_O;
    sout << "KIND_LISPALLOC_core__BitVectorWithFillPtr_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BitVectorWithFillPtr_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VectorDisplaced_O:
{
    typedef core::VectorDisplaced_O type_KIND_LISPALLOC_core__VectorDisplaced_O;
    sout << "KIND_LISPALLOC_core__VectorDisplaced_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorDisplaced_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__String_O:
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    sout << "KIND_LISPALLOC_core__String_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Str_O:
{
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    sout << "KIND_BOOTSTRAP_core__Str_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
    typedef core::StrWithFillPtr_O type_KIND_LISPALLOC_core__StrWithFillPtr_O;
    sout << "KIND_LISPALLOC_core__StrWithFillPtr_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StrWithFillPtr_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VectorObjects_O:
{
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    sout << "KIND_LISPALLOC_core__VectorObjects_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    typedef core::VectorObjectsWithFillPtr_O type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O;
    sout << "KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
    typedef core::SingleDispatchMethod_O type_KIND_LISPALLOC_core__SingleDispatchMethod_O;
    sout << "KIND_LISPALLOC_core__SingleDispatchMethod_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchMethod_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__RandomState_O:
{
    typedef core::RandomState_O type_KIND_LISPALLOC_core__RandomState_O;
    sout << "KIND_LISPALLOC_core__RandomState_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RandomState_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    sout << "KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
    typedef llvmo::DebugLoc_O type_KIND_LISPALLOC_llvmo__DebugLoc_O;
    sout << "KIND_LISPALLOC_llvmo__DebugLoc_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DebugLoc_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Attribute_O:
{
    typedef llvmo::Attribute_O type_KIND_LISPALLOC_llvmo__Attribute_O;
    sout << "KIND_LISPALLOC_llvmo__Attribute_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Attribute_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__RegexMatch_O:
{
    typedef core::RegexMatch_O type_KIND_LISPALLOC_core__RegexMatch_O;
    sout << "KIND_LISPALLOC_core__RegexMatch_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RegexMatch_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakPointer_O:
{
    typedef core::WeakPointer_O type_KIND_LISPALLOC_core__WeakPointer_O;
    sout << "KIND_LISPALLOC_core__WeakPointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakPointer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VaList_dummy_O:
{
    typedef core::VaList_dummy_O type_KIND_LISPALLOC_core__VaList_dummy_O;
    sout << "KIND_LISPALLOC_core__VaList_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VaList_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    sout << "KIND_BOOTSTRAP_core__StandardObject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    sout << "KIND_BOOTSTRAP_core__Metaobject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    sout << "KIND_BOOTSTRAP_core__Specializer_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Class_O:
{
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    sout << "KIND_BOOTSTRAP_core__Class_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__StdClass_O:
{
    typedef core::StdClass_O type_KIND_BOOTSTRAP_core__StdClass_O;
    sout << "KIND_BOOTSTRAP_core__StdClass_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StdClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__StandardClass_O:
{
    typedef core::StandardClass_O type_KIND_BOOTSTRAP_core__StandardClass_O;
    sout << "KIND_BOOTSTRAP_core__StandardClass_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
    typedef core::FuncallableStandardClass_O type_KIND_LISPALLOC_core__FuncallableStandardClass_O;
    sout << "KIND_LISPALLOC_core__FuncallableStandardClass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FuncallableStandardClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__StructureClass_O:
{
    typedef core::StructureClass_O type_KIND_BOOTSTRAP_core__StructureClass_O;
    sout << "KIND_BOOTSTRAP_core__StructureClass_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StructureClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
    typedef core::ForwardReferencedClass_O type_KIND_LISPALLOC_core__ForwardReferencedClass_O;
    sout << "KIND_LISPALLOC_core__ForwardReferencedClass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ForwardReferencedClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CxxClass_O:
{
    typedef core::CxxClass_O type_KIND_LISPALLOC_core__CxxClass_O;
    sout << "KIND_LISPALLOC_core__CxxClass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    typedef core::BuiltInClass_O type_KIND_BOOTSTRAP_core__BuiltInClass_O;
    sout << "KIND_BOOTSTRAP_core__BuiltInClass_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__BuiltInClass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_clbind__ClassRep_O:
{
    typedef clbind::ClassRep_O type_KIND_LISPALLOC_clbind__ClassRep_O;
    sout << "KIND_LISPALLOC_clbind__ClassRep_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_clbind__ClassRep_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ExternalObject_O:
{
    typedef core::ExternalObject_O type_KIND_LISPALLOC_core__ExternalObject_O;
    sout << "KIND_LISPALLOC_core__ExternalObject_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ExternalObject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Value_O:
{
    typedef llvmo::Value_O type_KIND_LISPALLOC_llvmo__Value_O;
    sout << "KIND_LISPALLOC_llvmo__Value_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Value_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Argument_O:
{
    typedef llvmo::Argument_O type_KIND_LISPALLOC_llvmo__Argument_O;
    sout << "KIND_LISPALLOC_llvmo__Argument_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Argument_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__User_O:
{
    typedef llvmo::User_O type_KIND_LISPALLOC_llvmo__User_O;
    sout << "KIND_LISPALLOC_llvmo__User_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__User_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Instruction_O:
{
    typedef llvmo::Instruction_O type_KIND_LISPALLOC_llvmo__Instruction_O;
    sout << "KIND_LISPALLOC_llvmo__Instruction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Instruction_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    typedef llvmo::AtomicRMWInst_O type_KIND_LISPALLOC_llvmo__AtomicRMWInst_O;
    sout << "KIND_LISPALLOC_llvmo__AtomicRMWInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AtomicRMWInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    typedef llvmo::LandingPadInst_O type_KIND_LISPALLOC_llvmo__LandingPadInst_O;
    sout << "KIND_LISPALLOC_llvmo__LandingPadInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LandingPadInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__PHINode_O:
{
    typedef llvmo::PHINode_O type_KIND_LISPALLOC_llvmo__PHINode_O;
    sout << "KIND_LISPALLOC_llvmo__PHINode_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PHINode_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__CallInst_O:
{
    typedef llvmo::CallInst_O type_KIND_LISPALLOC_llvmo__CallInst_O;
    sout << "KIND_LISPALLOC_llvmo__CallInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__CallInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    typedef llvmo::StoreInst_O type_KIND_LISPALLOC_llvmo__StoreInst_O;
    sout << "KIND_LISPALLOC_llvmo__StoreInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__StoreInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    typedef llvmo::UnaryInstruction_O type_KIND_LISPALLOC_llvmo__UnaryInstruction_O;
    sout << "KIND_LISPALLOC_llvmo__UnaryInstruction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UnaryInstruction_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    typedef llvmo::LoadInst_O type_KIND_LISPALLOC_llvmo__LoadInst_O;
    sout << "KIND_LISPALLOC_llvmo__LoadInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LoadInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    typedef llvmo::AllocaInst_O type_KIND_LISPALLOC_llvmo__AllocaInst_O;
    sout << "KIND_LISPALLOC_llvmo__AllocaInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AllocaInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    typedef llvmo::VAArgInst_O type_KIND_LISPALLOC_llvmo__VAArgInst_O;
    sout << "KIND_LISPALLOC_llvmo__VAArgInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__VAArgInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    typedef llvmo::AtomicCmpXchgInst_O type_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O;
    sout << "KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    typedef llvmo::TerminatorInst_O type_KIND_LISPALLOC_llvmo__TerminatorInst_O;
    sout << "KIND_LISPALLOC_llvmo__TerminatorInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TerminatorInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    typedef llvmo::UnreachableInst_O type_KIND_LISPALLOC_llvmo__UnreachableInst_O;
    sout << "KIND_LISPALLOC_llvmo__UnreachableInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UnreachableInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    typedef llvmo::SwitchInst_O type_KIND_LISPALLOC_llvmo__SwitchInst_O;
    sout << "KIND_LISPALLOC_llvmo__SwitchInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__SwitchInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    typedef llvmo::ReturnInst_O type_KIND_LISPALLOC_llvmo__ReturnInst_O;
    sout << "KIND_LISPALLOC_llvmo__ReturnInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ReturnInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    typedef llvmo::ResumeInst_O type_KIND_LISPALLOC_llvmo__ResumeInst_O;
    sout << "KIND_LISPALLOC_llvmo__ResumeInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ResumeInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    typedef llvmo::BranchInst_O type_KIND_LISPALLOC_llvmo__BranchInst_O;
    sout << "KIND_LISPALLOC_llvmo__BranchInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BranchInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    typedef llvmo::InvokeInst_O type_KIND_LISPALLOC_llvmo__InvokeInst_O;
    sout << "KIND_LISPALLOC_llvmo__InvokeInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__InvokeInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    typedef llvmo::IndirectBrInst_O type_KIND_LISPALLOC_llvmo__IndirectBrInst_O;
    sout << "KIND_LISPALLOC_llvmo__IndirectBrInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IndirectBrInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    typedef llvmo::FenceInst_O type_KIND_LISPALLOC_llvmo__FenceInst_O;
    sout << "KIND_LISPALLOC_llvmo__FenceInst_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FenceInst_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Constant_O:
{
    typedef llvmo::Constant_O type_KIND_LISPALLOC_llvmo__Constant_O;
    sout << "KIND_LISPALLOC_llvmo__Constant_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Constant_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    typedef llvmo::BlockAddress_O type_KIND_LISPALLOC_llvmo__BlockAddress_O;
    sout << "KIND_LISPALLOC_llvmo__BlockAddress_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BlockAddress_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    typedef llvmo::GlobalValue_O type_KIND_LISPALLOC_llvmo__GlobalValue_O;
    sout << "KIND_LISPALLOC_llvmo__GlobalValue_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__GlobalValue_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    typedef llvmo::GlobalVariable_O type_KIND_LISPALLOC_llvmo__GlobalVariable_O;
    sout << "KIND_LISPALLOC_llvmo__GlobalVariable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__GlobalVariable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Function_O:
{
    typedef llvmo::Function_O type_KIND_LISPALLOC_llvmo__Function_O;
    sout << "KIND_LISPALLOC_llvmo__Function_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Function_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    typedef llvmo::ConstantArray_O type_KIND_LISPALLOC_llvmo__ConstantArray_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantArray_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantArray_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    typedef llvmo::ConstantInt_O type_KIND_LISPALLOC_llvmo__ConstantInt_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantInt_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantInt_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    typedef llvmo::ConstantDataSequential_O type_KIND_LISPALLOC_llvmo__ConstantDataSequential_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantDataSequential_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantDataSequential_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    typedef llvmo::ConstantDataArray_O type_KIND_LISPALLOC_llvmo__ConstantDataArray_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantDataArray_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantDataArray_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    typedef llvmo::ConstantStruct_O type_KIND_LISPALLOC_llvmo__ConstantStruct_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantStruct_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantStruct_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    typedef llvmo::ConstantFP_O type_KIND_LISPALLOC_llvmo__ConstantFP_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantFP_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantFP_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    typedef llvmo::UndefValue_O type_KIND_LISPALLOC_llvmo__UndefValue_O;
    sout << "KIND_LISPALLOC_llvmo__UndefValue_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UndefValue_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    typedef llvmo::ConstantPointerNull_O type_KIND_LISPALLOC_llvmo__ConstantPointerNull_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantPointerNull_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantPointerNull_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    typedef llvmo::ConstantExpr_O type_KIND_LISPALLOC_llvmo__ConstantExpr_O;
    sout << "KIND_LISPALLOC_llvmo__ConstantExpr_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantExpr_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    typedef llvmo::BasicBlock_O type_KIND_LISPALLOC_llvmo__BasicBlock_O;
    sout << "KIND_LISPALLOC_llvmo__BasicBlock_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BasicBlock_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    typedef llvmo::IRBuilderBase_O type_KIND_LISPALLOC_llvmo__IRBuilderBase_O;
    sout << "KIND_LISPALLOC_llvmo__IRBuilderBase_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IRBuilderBase_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    typedef llvmo::IRBuilder_O type_KIND_LISPALLOC_llvmo__IRBuilder_O;
    sout << "KIND_LISPALLOC_llvmo__IRBuilder_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IRBuilder_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
    typedef llvmo::DIBuilder_O type_KIND_LISPALLOC_llvmo__DIBuilder_O;
    sout << "KIND_LISPALLOC_llvmo__DIBuilder_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIBuilder_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Metadata_O:
{
    typedef llvmo::Metadata_O type_KIND_LISPALLOC_llvmo__Metadata_O;
    sout << "KIND_LISPALLOC_llvmo__Metadata_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Metadata_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    typedef llvmo::ValueAsMetadata_O type_KIND_LISPALLOC_llvmo__ValueAsMetadata_O;
    sout << "KIND_LISPALLOC_llvmo__ValueAsMetadata_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ValueAsMetadata_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__MDNode_O:
{
    typedef llvmo::MDNode_O type_KIND_LISPALLOC_llvmo__MDNode_O;
    sout << "KIND_LISPALLOC_llvmo__MDNode_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MDNode_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__MDString_O:
{
    typedef llvmo::MDString_O type_KIND_LISPALLOC_llvmo__MDString_O;
    sout << "KIND_LISPALLOC_llvmo__MDString_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MDString_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    typedef llvmo::ExecutionEngine_O type_KIND_LISPALLOC_llvmo__ExecutionEngine_O;
    sout << "KIND_LISPALLOC_llvmo__ExecutionEngine_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ExecutionEngine_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__APFloat_O:
{
    typedef llvmo::APFloat_O type_KIND_LISPALLOC_llvmo__APFloat_O;
    sout << "KIND_LISPALLOC_llvmo__APFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__APFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    typedef llvmo::PassManagerBuilder_O type_KIND_LISPALLOC_llvmo__PassManagerBuilder_O;
    sout << "KIND_LISPALLOC_llvmo__PassManagerBuilder_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManagerBuilder_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    typedef llvmo::DataLayout_O type_KIND_LISPALLOC_llvmo__DataLayout_O;
    sout << "KIND_LISPALLOC_llvmo__DataLayout_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DataLayout_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Triple_O:
{
    typedef llvmo::Triple_O type_KIND_LISPALLOC_llvmo__Triple_O;
    sout << "KIND_LISPALLOC_llvmo__Triple_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Triple_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__APInt_O:
{
    typedef llvmo::APInt_O type_KIND_LISPALLOC_llvmo__APInt_O;
    sout << "KIND_LISPALLOC_llvmo__APInt_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__APInt_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    typedef llvmo::PassManagerBase_O type_KIND_LISPALLOC_llvmo__PassManagerBase_O;
    sout << "KIND_LISPALLOC_llvmo__PassManagerBase_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManagerBase_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    typedef llvmo::FunctionPassManager_O type_KIND_LISPALLOC_llvmo__FunctionPassManager_O;
    sout << "KIND_LISPALLOC_llvmo__FunctionPassManager_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionPassManager_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__PassManager_O:
{
    typedef llvmo::PassManager_O type_KIND_LISPALLOC_llvmo__PassManager_O;
    sout << "KIND_LISPALLOC_llvmo__PassManager_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManager_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    typedef llvmo::TargetMachine_O type_KIND_LISPALLOC_llvmo__TargetMachine_O;
    sout << "KIND_LISPALLOC_llvmo__TargetMachine_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetMachine_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    typedef llvmo::LLVMTargetMachine_O type_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O;
    sout << "KIND_LISPALLOC_llvmo__LLVMTargetMachine_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    typedef llvmo::TargetOptions_O type_KIND_LISPALLOC_llvmo__TargetOptions_O;
    sout << "KIND_LISPALLOC_llvmo__TargetOptions_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetOptions_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Type_O:
{
    typedef llvmo::Type_O type_KIND_LISPALLOC_llvmo__Type_O;
    sout << "KIND_LISPALLOC_llvmo__Type_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Type_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    typedef llvmo::IntegerType_O type_KIND_LISPALLOC_llvmo__IntegerType_O;
    sout << "KIND_LISPALLOC_llvmo__IntegerType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IntegerType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    typedef llvmo::CompositeType_O type_KIND_LISPALLOC_llvmo__CompositeType_O;
    sout << "KIND_LISPALLOC_llvmo__CompositeType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__CompositeType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    typedef llvmo::SequentialType_O type_KIND_LISPALLOC_llvmo__SequentialType_O;
    sout << "KIND_LISPALLOC_llvmo__SequentialType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__SequentialType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__VectorType_O:
{
    typedef llvmo::VectorType_O type_KIND_LISPALLOC_llvmo__VectorType_O;
    sout << "KIND_LISPALLOC_llvmo__VectorType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__VectorType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__PointerType_O:
{
    typedef llvmo::PointerType_O type_KIND_LISPALLOC_llvmo__PointerType_O;
    sout << "KIND_LISPALLOC_llvmo__PointerType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PointerType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    typedef llvmo::ArrayType_O type_KIND_LISPALLOC_llvmo__ArrayType_O;
    sout << "KIND_LISPALLOC_llvmo__ArrayType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ArrayType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__StructType_O:
{
    typedef llvmo::StructType_O type_KIND_LISPALLOC_llvmo__StructType_O;
    sout << "KIND_LISPALLOC_llvmo__StructType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__StructType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    typedef llvmo::FunctionType_O type_KIND_LISPALLOC_llvmo__FunctionType_O;
    sout << "KIND_LISPALLOC_llvmo__FunctionType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    typedef llvmo::NamedMDNode_O type_KIND_LISPALLOC_llvmo__NamedMDNode_O;
    sout << "KIND_LISPALLOC_llvmo__NamedMDNode_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__NamedMDNode_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Linker_O:
{
    typedef llvmo::Linker_O type_KIND_LISPALLOC_llvmo__Linker_O;
    sout << "KIND_LISPALLOC_llvmo__Linker_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Linker_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Pass_O:
{
    typedef llvmo::Pass_O type_KIND_LISPALLOC_llvmo__Pass_O;
    sout << "KIND_LISPALLOC_llvmo__Pass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Pass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    typedef llvmo::FunctionPass_O type_KIND_LISPALLOC_llvmo__FunctionPass_O;
    sout << "KIND_LISPALLOC_llvmo__FunctionPass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionPass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    typedef llvmo::ModulePass_O type_KIND_LISPALLOC_llvmo__ModulePass_O;
    sout << "KIND_LISPALLOC_llvmo__ModulePass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ModulePass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    typedef llvmo::ImmutablePass_O type_KIND_LISPALLOC_llvmo__ImmutablePass_O;
    sout << "KIND_LISPALLOC_llvmo__ImmutablePass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ImmutablePass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    typedef llvmo::DataLayoutPass_O type_KIND_LISPALLOC_llvmo__DataLayoutPass_O;
    sout << "KIND_LISPALLOC_llvmo__DataLayoutPass_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DataLayoutPass_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    typedef llvmo::TargetLibraryInfo_O type_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O;
    sout << "KIND_LISPALLOC_llvmo__TargetLibraryInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    typedef llvmo::MCSubtargetInfo_O type_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O;
    sout << "KIND_LISPALLOC_llvmo__MCSubtargetInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    typedef llvmo::TargetSubtargetInfo_O type_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O;
    sout << "KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Module_O:
{
    typedef llvmo::Module_O type_KIND_LISPALLOC_llvmo__Module_O;
    sout << "KIND_LISPALLOC_llvmo__Module_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Module_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    typedef llvmo::EngineBuilder_O type_KIND_LISPALLOC_llvmo__EngineBuilder_O;
    sout << "KIND_LISPALLOC_llvmo__EngineBuilder_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__EngineBuilder_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ForeignData_O:
{
    typedef core::ForeignData_O type_KIND_LISPALLOC_core__ForeignData_O;
    sout << "KIND_LISPALLOC_core__ForeignData_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ForeignData_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    typedef llvmo::LLVMContext_O type_KIND_LISPALLOC_llvmo__LLVMContext_O;
    sout << "KIND_LISPALLOC_llvmo__LLVMContext_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LLVMContext_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__Target_O:
{
    typedef llvmo::Target_O type_KIND_LISPALLOC_llvmo__Target_O;
    sout << "KIND_LISPALLOC_llvmo__Target_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Target_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LoadTimeValues_O:
{
    typedef core::LoadTimeValues_O type_KIND_LISPALLOC_core__LoadTimeValues_O;
    sout << "KIND_LISPALLOC_core__LoadTimeValues_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadTimeValues_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Binder_O:
{
    typedef core::Binder_O type_KIND_LISPALLOC_core__Binder_O;
    sout << "KIND_LISPALLOC_core__Binder_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Binder_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__IntArray_O:
{
    typedef core::IntArray_O type_KIND_LISPALLOC_core__IntArray_O;
    sout << "KIND_LISPALLOC_core__IntArray_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__IntArray_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourceManager_O:
{
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    sout << "KIND_LISPALLOC_core__SourceManager_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Record_O:
{
    typedef core::Record_O type_KIND_LISPALLOC_core__Record_O;
    sout << "KIND_LISPALLOC_core__Record_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Record_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LightUserData_O:
{
    typedef core::LightUserData_O type_KIND_LISPALLOC_core__LightUserData_O;
    sout << "KIND_LISPALLOC_core__LightUserData_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LightUserData_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__UserData_O:
{
    typedef core::UserData_O type_KIND_LISPALLOC_core__UserData_O;
    sout << "KIND_LISPALLOC_core__UserData_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__UserData_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Symbol_O:
{
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    sout << "KIND_BOOTSTRAP_core__Symbol_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Null_O:
{
    typedef core::Null_O type_KIND_LISPALLOC_core__Null_O;
    sout << "KIND_LISPALLOC_core__Null_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Null_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    sout << "KIND_LISPALLOC_core__SourcePosInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    sout << "KIND_TEMPLATED_LISPALLOC_core__Iterator_O size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    typedef core::DirectoryIterator_O type_KIND_LISPALLOC_core__DirectoryIterator_O;
    sout << "KIND_LISPALLOC_core__DirectoryIterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryIterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    typedef core::RecursiveDirectoryIterator_O type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O;
    sout << "KIND_LISPALLOC_core__RecursiveDirectoryIterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Regex_O:
{
    typedef core::Regex_O type_KIND_LISPALLOC_core__Regex_O;
    sout << "KIND_LISPALLOC_core__Regex_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Regex_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
    typedef core::PosixTimeDuration_O type_KIND_LISPALLOC_core__PosixTimeDuration_O;
    sout << "KIND_LISPALLOC_core__PosixTimeDuration_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__PosixTimeDuration_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    sout << "KIND_LISPALLOC_core__SymbolToEnumConverter_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CandoException_O:
{
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    sout << "KIND_LISPALLOC_core__CandoException_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Stream_O:
{
    typedef core::Stream_O type_KIND_LISPALLOC_core__Stream_O;
    sout << "KIND_LISPALLOC_core__Stream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Stream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__AnsiStream_O:
{
    typedef core::AnsiStream_O type_KIND_LISPALLOC_core__AnsiStream_O;
    sout << "KIND_LISPALLOC_core__AnsiStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__AnsiStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FileStream_O:
{
    typedef core::FileStream_O type_KIND_LISPALLOC_core__FileStream_O;
    sout << "KIND_LISPALLOC_core__FileStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__IOStreamStream_O:
{
    typedef core::IOStreamStream_O type_KIND_LISPALLOC_core__IOStreamStream_O;
    sout << "KIND_LISPALLOC_core__IOStreamStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__IOStreamStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__IOFileStream_O:
{
    typedef core::IOFileStream_O type_KIND_LISPALLOC_core__IOFileStream_O;
    sout << "KIND_LISPALLOC_core__IOFileStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__IOFileStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
    typedef core::ConcatenatedStream_O type_KIND_LISPALLOC_core__ConcatenatedStream_O;
    sout << "KIND_LISPALLOC_core__ConcatenatedStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ConcatenatedStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StringStream_O:
{
    typedef core::StringStream_O type_KIND_LISPALLOC_core__StringStream_O;
    sout << "KIND_LISPALLOC_core__StringStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StringStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StringInputStream_O:
{
    typedef core::StringInputStream_O type_KIND_LISPALLOC_core__StringInputStream_O;
    sout << "KIND_LISPALLOC_core__StringInputStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StringInputStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StringOutputStream_O:
{
    typedef core::StringOutputStream_O type_KIND_LISPALLOC_core__StringOutputStream_O;
    sout << "KIND_LISPALLOC_core__StringOutputStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StringOutputStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SynonymStream_O:
{
    typedef core::SynonymStream_O type_KIND_LISPALLOC_core__SynonymStream_O;
    sout << "KIND_LISPALLOC_core__SynonymStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SynonymStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__EchoStream_O:
{
    typedef core::EchoStream_O type_KIND_LISPALLOC_core__EchoStream_O;
    sout << "KIND_LISPALLOC_core__EchoStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__EchoStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__TwoWayStream_O:
{
    typedef core::TwoWayStream_O type_KIND_LISPALLOC_core__TwoWayStream_O;
    sout << "KIND_LISPALLOC_core__TwoWayStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TwoWayStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__BroadcastStream_O:
{
    typedef core::BroadcastStream_O type_KIND_LISPALLOC_core__BroadcastStream_O;
    sout << "KIND_LISPALLOC_core__BroadcastStream_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BroadcastStream_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Reader_O:
{
    typedef core::Reader_O type_KIND_LISPALLOC_core__Reader_O;
    sout << "KIND_LISPALLOC_core__Reader_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Reader_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Cons_O:
{
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    sout << "KIND_LISPALLOC_core__Cons_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Archive_O:
{
    typedef core::Archive_O type_KIND_LISPALLOC_core__Archive_O;
    sout << "KIND_LISPALLOC_core__Archive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Archive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SaveArchive_O:
{
    typedef core::SaveArchive_O type_KIND_LISPALLOC_core__SaveArchive_O;
    sout << "KIND_LISPALLOC_core__SaveArchive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SaveArchive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
    typedef core::SexpSaveArchive_O type_KIND_LISPALLOC_core__SexpSaveArchive_O;
    sout << "KIND_LISPALLOC_core__SexpSaveArchive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SexpSaveArchive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LoadArchive_O:
{
    typedef core::LoadArchive_O type_KIND_LISPALLOC_core__LoadArchive_O;
    sout << "KIND_LISPALLOC_core__LoadArchive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadArchive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
    typedef core::SexpLoadArchive_O type_KIND_LISPALLOC_core__SexpLoadArchive_O;
    sout << "KIND_LISPALLOC_core__SexpLoadArchive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SexpLoadArchive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__HashTable_O:
{
    typedef core::HashTable_O type_KIND_LISPALLOC_core__HashTable_O;
    sout << "KIND_LISPALLOC_core__HashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__HashTableEq_O:
{
    typedef core::HashTableEq_O type_KIND_LISPALLOC_core__HashTableEq_O;
    sout << "KIND_LISPALLOC_core__HashTableEq_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEq_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    typedef core::HashTableEqualp_O type_KIND_LISPALLOC_core__HashTableEqualp_O;
    sout << "KIND_LISPALLOC_core__HashTableEqualp_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEqualp_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__HashTableEql_O:
{
    typedef core::HashTableEql_O type_KIND_LISPALLOC_core__HashTableEql_O;
    sout << "KIND_LISPALLOC_core__HashTableEql_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEql_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__HashTableEqual_O:
{
    typedef core::HashTableEqual_O type_KIND_LISPALLOC_core__HashTableEqual_O;
    sout << "KIND_LISPALLOC_core__HashTableEqual_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEqual_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_cffi__Pointer_O:
{
    typedef cffi::Pointer_O type_KIND_LISPALLOC_cffi__Pointer_O;
    sout << "KIND_LISPALLOC_cffi__Pointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_cffi__Pointer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    sout << "KIND_LISPALLOC_core__CxxObject_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
    typedef core::WeakKeyMapping_O type_KIND_LISPALLOC_core__WeakKeyMapping_O;
    sout << "KIND_LISPALLOC_core__WeakKeyMapping_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyMapping_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    typedef core::LambdaListHandler_O type_KIND_LISPALLOC_core__LambdaListHandler_O;
    sout << "KIND_LISPALLOC_core__LambdaListHandler_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LambdaListHandler_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
    typedef llvmo::InsertPoint_O type_KIND_LISPALLOC_llvmo__InsertPoint_O;
    sout << "KIND_LISPALLOC_llvmo__InsertPoint_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__InsertPoint_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    sout << "KIND_LISPALLOC_core__SourceFileInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SNode_O:
{
    typedef core::SNode_O type_KIND_LISPALLOC_core__SNode_O;
    sout << "KIND_LISPALLOC_core__SNode_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SNode_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LeafSNode_O:
{
    typedef core::LeafSNode_O type_KIND_LISPALLOC_core__LeafSNode_O;
    sout << "KIND_LISPALLOC_core__LeafSNode_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LeafSNode_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__BranchSNode_O:
{
    typedef core::BranchSNode_O type_KIND_LISPALLOC_core__BranchSNode_O;
    sout << "KIND_LISPALLOC_core__BranchSNode_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BranchSNode_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Path_O:
{
    typedef core::Path_O type_KIND_LISPALLOC_core__Path_O;
    sout << "KIND_LISPALLOC_core__Path_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Path_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
    typedef asttooling::AstVisitor_O type_KIND_LISPALLOC_asttooling__AstVisitor_O;
    sout << "KIND_LISPALLOC_asttooling__AstVisitor_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__AstVisitor_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    typedef llvmo::AttributeSet_O type_KIND_LISPALLOC_llvmo__AttributeSet_O;
    sout << "KIND_LISPALLOC_llvmo__AttributeSet_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AttributeSet_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StructureObject_O:
{
    typedef core::StructureObject_O type_KIND_LISPALLOC_core__StructureObject_O;
    sout << "KIND_LISPALLOC_core__StructureObject_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StructureObject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    typedef core::InvocationHistoryFrameIterator_O type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O;
    sout << "KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Package_O:
{
    typedef core::Package_O type_KIND_LISPALLOC_core__Package_O;
    sout << "KIND_LISPALLOC_core__Package_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Package_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    typedef core::DirectoryEntry_O type_KIND_LISPALLOC_core__DirectoryEntry_O;
    sout << "KIND_LISPALLOC_core__DirectoryEntry_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryEntry_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Character_dummy_O:
{
    typedef core::Character_dummy_O type_KIND_LISPALLOC_core__Character_dummy_O;
    sout << "KIND_LISPALLOC_core__Character_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Character_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Function_O:
{
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    sout << "KIND_LISPALLOC_core__Function_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CompiledFunction_O:
{
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    sout << "KIND_LISPALLOC_core__CompiledFunction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
    typedef core::SingleDispatchGenericFunction_O type_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O;
    sout << "KIND_LISPALLOC_core__SingleDispatchGenericFunction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SpecialForm_O:
{
    typedef core::SpecialForm_O type_KIND_LISPALLOC_core__SpecialForm_O;
    sout << "KIND_LISPALLOC_core__SpecialForm_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SpecialForm_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
    typedef core::SingleDispatchEffectiveMethodFunction_O type_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O;
    sout << "KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Instance_O:
{
    typedef core::Instance_O type_KIND_LISPALLOC_core__Instance_O;
    sout << "KIND_LISPALLOC_core__Instance_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Instance_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Pointer_O:
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    sout << "KIND_LISPALLOC_core__Pointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    typedef clbind::ClassRegistry_O type_KIND_LISPALLOC_clbind__ClassRegistry_O;
    sout << "KIND_LISPALLOC_clbind__ClassRegistry_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_clbind__ClassRegistry_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
    typedef llvmo::DebugInfo_O type_KIND_LISPALLOC_llvmo__DebugInfo_O;
    sout << "KIND_LISPALLOC_llvmo__DebugInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DebugInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
    typedef llvmo::DIDerivedType_O type_KIND_LISPALLOC_llvmo__DIDerivedType_O;
    sout << "KIND_LISPALLOC_llvmo__DIDerivedType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIDerivedType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIArray_O:
{
    typedef llvmo::DIArray_O type_KIND_LISPALLOC_llvmo__DIArray_O;
    sout << "KIND_LISPALLOC_llvmo__DIArray_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIArray_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
    typedef llvmo::DIBasicType_O type_KIND_LISPALLOC_llvmo__DIBasicType_O;
    sout << "KIND_LISPALLOC_llvmo__DIBasicType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIBasicType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
    typedef llvmo::DISubprogram_O type_KIND_LISPALLOC_llvmo__DISubprogram_O;
    sout << "KIND_LISPALLOC_llvmo__DISubprogram_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DISubprogram_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
    typedef llvmo::DILexicalBlock_O type_KIND_LISPALLOC_llvmo__DILexicalBlock_O;
    sout << "KIND_LISPALLOC_llvmo__DILexicalBlock_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DILexicalBlock_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
    typedef llvmo::DICompileUnit_O type_KIND_LISPALLOC_llvmo__DICompileUnit_O;
    sout << "KIND_LISPALLOC_llvmo__DICompileUnit_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DICompileUnit_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
    typedef llvmo::DIDescriptor_O type_KIND_LISPALLOC_llvmo__DIDescriptor_O;
    sout << "KIND_LISPALLOC_llvmo__DIDescriptor_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIDescriptor_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIType_O:
{
    typedef llvmo::DIType_O type_KIND_LISPALLOC_llvmo__DIType_O;
    sout << "KIND_LISPALLOC_llvmo__DIType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
    typedef llvmo::DISubroutineType_O type_KIND_LISPALLOC_llvmo__DISubroutineType_O;
    sout << "KIND_LISPALLOC_llvmo__DISubroutineType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DISubroutineType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
    typedef llvmo::DICompositeType_O type_KIND_LISPALLOC_llvmo__DICompositeType_O;
    sout << "KIND_LISPALLOC_llvmo__DICompositeType_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DICompositeType_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
    typedef llvmo::DITypeArray_O type_KIND_LISPALLOC_llvmo__DITypeArray_O;
    sout << "KIND_LISPALLOC_llvmo__DITypeArray_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DITypeArray_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIFile_O:
{
    typedef llvmo::DIFile_O type_KIND_LISPALLOC_llvmo__DIFile_O;
    sout << "KIND_LISPALLOC_llvmo__DIFile_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIFile_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_llvmo__DIScope_O:
{
    typedef llvmo::DIScope_O type_KIND_LISPALLOC_llvmo__DIScope_O;
    sout << "KIND_LISPALLOC_llvmo__DIScope_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIScope_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SmallMultimap_O:
{
    typedef core::SmallMultimap_O type_KIND_LISPALLOC_core__SmallMultimap_O;
    sout << "KIND_LISPALLOC_core__SmallMultimap_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SmallMultimap_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Pathname_O:
{
    typedef core::Pathname_O type_KIND_LISPALLOC_core__Pathname_O;
    sout << "KIND_LISPALLOC_core__Pathname_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Pathname_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LogicalPathname_O:
{
    typedef core::LogicalPathname_O type_KIND_LISPALLOC_core__LogicalPathname_O;
    sout << "KIND_LISPALLOC_core__LogicalPathname_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LogicalPathname_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__PosixTime_O:
{
    typedef core::PosixTime_O type_KIND_LISPALLOC_core__PosixTime_O;
    sout << "KIND_LISPALLOC_core__PosixTime_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__PosixTime_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SmallMap_O:
{
    typedef core::SmallMap_O type_KIND_LISPALLOC_core__SmallMap_O;
    sout << "KIND_LISPALLOC_core__SmallMap_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SmallMap_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__Cache:
{
    typedef core::Cache type_KIND_CLASSALLOC_core__Cache;
    sout << "KIND_CLASSALLOC_core__Cache size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__Cache))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
    typedef core::Lisp_O type_KIND_ROOTCLASSALLOC_core__Lisp_O;
    sout << "KIND_ROOTCLASSALLOC_core__Lisp_O size[" << (AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_core__Lisp_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
    gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    gctools::GCVector_moveable<core::KeywordArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::KeywordArgument>*>(client);
    sout << "gctools::GCVector_moveable<core::KeywordArgument>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::KeywordArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    sout << "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure:
{
    typedef core::SingleDispatchGenericFunctionClosure type_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure;
    sout << "KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    gctools::GCString_moveable<char>* obj_gc_safe = reinterpret_cast<gctools::GCString_moveable<char>*>(client);
    typedef typename gctools::GCString_moveable<char> type_KIND_GCSTRING_gctools__GCString_moveable_char_;
    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_KIND_GCSTRING_gctools__GCString_moveable_char_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "KIND_GCSTRING_gctools__GCString_moveable_char_" << "bytes[" << header_and_gcstring_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    gctools::GCVector_moveable<core::RequiredArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::RequiredArgument>*>(client);
    sout << "gctools::GCVector_moveable<core::RequiredArgument>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::RequiredArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_llvmo__CompiledClosure:
{
    typedef llvmo::CompiledClosure type_KIND_CLASSALLOC_llvmo__CompiledClosure;
    sout << "KIND_CLASSALLOC_llvmo__CompiledClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_llvmo__CompiledClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor:
{
    typedef asttooling::internal::VariadicOperatorMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor;
    sout << "KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
    gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>*>(client);
    sout << "gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>> type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor:
{
    typedef asttooling::internal::OverloadedMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor;
    sout << "KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
    gctools::GCVector_moveable<core::SymbolStorage>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolStorage>*>(client);
    sout << "gctools::GCVector_moveable<core::SymbolStorage>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::SymbolStorage> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    gctools::GCVector_moveable<asttooling::ContextFrame>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ContextFrame>*>(client);
    sout << "gctools::GCVector_moveable<asttooling::ContextFrame>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<asttooling::ContextFrame> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor:
{
    typedef asttooling::internal::FixedArgCountMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor;
    sout << "KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
    gctools::GCVector_moveable<core::T_O *>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::T_O *>*>(client);
    sout << "gctools::GCVector_moveable<core::T_O *>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::T_O *> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor:
{
    typedef asttooling::internal::FreeFuncMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor;
    sout << "KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__MacroClosure:
{
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    sout << "KIND_CLASSALLOC_core__MacroClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__ConsStepper:
{
    typedef core::ConsStepper type_KIND_CLASSALLOC_core__ConsStepper;
    sout << "KIND_CLASSALLOC_core__ConsStepper size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__ConsStepper))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    gctools::GCVector_moveable<core::AuxArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::AuxArgument>*>(client);
    sout << "gctools::GCVector_moveable<core::AuxArgument>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::AuxArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
    gctools::GCVector_moveable<asttooling::ParserValue>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ParserValue>*>(client);
    sout << "gctools::GCVector_moveable<asttooling::ParserValue>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<asttooling::ParserValue> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    gctools::GCVector_moveable<core::SymbolClassPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolClassPair>*>(client);
    sout << "gctools::GCVector_moveable<core::SymbolClassPair>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::SymbolClassPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
    gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>*>(client);
    sout << "gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
    gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>*>(client);
    sout << "gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>> type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    gctools::GCVector_moveable<core::CacheRecord>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::CacheRecord>*>(client);
    sout << "gctools::GCVector_moveable<core::CacheRecord>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::CacheRecord> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__InstanceClosure:
{
    typedef core::InstanceClosure type_KIND_CLASSALLOC_core__InstanceClosure;
    sout << "KIND_CLASSALLOC_core__InstanceClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__InstanceClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    typedef asttooling::DerivableFrontendActionFactory type_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory;
    sout << "KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>*>(client);
    sout << "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    typedef asttooling::DerivableMatchCallback type_KIND_LISPALLOC_asttooling__DerivableMatchCallback;
    sout << "KIND_LISPALLOC_asttooling__DerivableMatchCallback size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableMatchCallback))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    gctools::GCVector_moveable<asttooling::ErrorContent>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ErrorContent>*>(client);
    sout << "gctools::GCVector_moveable<asttooling::ErrorContent>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<asttooling::ErrorContent> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    typedef asttooling::DerivableASTFrontendAction type_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction;
    sout << "KIND_LISPALLOC_asttooling__DerivableASTFrontendAction size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    gctools::GCVector_moveable<asttooling::Message>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::Message>*>(client);
    sout << "gctools::GCVector_moveable<asttooling::Message>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<asttooling::Message> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__:
{
    gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>*>(client);
    sout << "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__CoreExposer:
{
    typedef core::CoreExposer type_KIND_CLASSALLOC_core__CoreExposer;
    sout << "KIND_CLASSALLOC_core__CoreExposer size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__CoreExposer))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    typedef asttooling::DerivableSyntaxOnlyAction type_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction;
    sout << "KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    gctools::GCVector_moveable<core::OptionalArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::OptionalArgument>*>(client);
    sout << "gctools::GCVector_moveable<core::OptionalArgument>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::OptionalArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    gctools::GCVector_moveable<core::ExceptionEntry>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::ExceptionEntry>*>(client);
    sout << "gctools::GCVector_moveable<core::ExceptionEntry>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::ExceptionEntry> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__InterpretedClosure:
{
    typedef core::InterpretedClosure type_KIND_CLASSALLOC_core__InterpretedClosure;
    sout << "KIND_CLASSALLOC_core__InterpretedClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__InterpretedClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    gctools::GCVector_moveable<core::DynamicBinding>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::DynamicBinding>*>(client);
    sout << "gctools::GCVector_moveable<core::DynamicBinding>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::DynamicBinding> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__VectorStepper:
{
    typedef core::VectorStepper type_KIND_CLASSALLOC_core__VectorStepper;
    sout << "KIND_CLASSALLOC_core__VectorStepper size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__VectorStepper))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
#endif // defined(GC_OBJ_DUMP_MAP)
#if defined(GC_OBJ_DUMP_MAP_HELPERS)

#endif // defined(GC_OBJ_DUMP_MAP_HELPERS)
#if defined(GC_OBJ_DUMP_MAP_TABLE)
static void* OBJ_DUMP_MAP_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_dump_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps
  /* 5 */ , &&obj_dump_KIND_ROOTCLASSALLOC_clbind__detail__class_map
  /* 6 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 7 */ , &&obj_dump_KIND_CLASSALLOC_clbind__DummyCreator
  /* 8 */ , &&obj_dump_KIND_CLASSALLOC_core__InstanceCreator
  /* 9 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator
  /* 10 */ , &&obj_dump_KIND_BOOTSTRAP_core__T_O
  /* 11 */ , &&obj_dump_KIND_LISPALLOC_core__MultiStringBuffer_O
  /* 12 */ , &&obj_dump_KIND_LISPALLOC_core__ReadTable_O
  /* 13 */ , &&obj_dump_KIND_LISPALLOC_core__Number_O
  /* 14 */ , &&obj_dump_KIND_LISPALLOC_core__Complex_O
  /* 15 */ , &&obj_dump_KIND_LISPALLOC_core__Real_O
  /* 16 */ , &&obj_dump_KIND_LISPALLOC_core__Rational_O
  /* 17 */ , &&obj_dump_KIND_LISPALLOC_core__Integer_O
  /* 18 */ , &&obj_dump_KIND_LISPALLOC_core__Bignum_O
  /* 19 */ , &&obj_dump_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 20 */ , &&obj_dump_KIND_LISPALLOC_core__Ratio_O
  /* 21 */ , &&obj_dump_KIND_LISPALLOC_core__Float_O
  /* 22 */ , &&obj_dump_KIND_LISPALLOC_core__DoubleFloat_O
  /* 23 */ , &&obj_dump_KIND_LISPALLOC_core__LongFloat_O
  /* 24 */ , &&obj_dump_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 25 */ , &&obj_dump_KIND_LISPALLOC_core__ShortFloat_O
  /* 26 */ , &&obj_dump_KIND_LISPALLOC_core__FileStatus_O
  /* 27 */ , &&obj_dump_KIND_LISPALLOC_core__WeakHashTable_O
  /* 28 */ , &&obj_dump_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 29 */ , &&obj_dump_KIND_LISPALLOC_core__Environment_O
  /* 30 */ , &&obj_dump_KIND_LISPALLOC_core__ActivationFrame_O
  /* 31 */ , &&obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 32 */ , &&obj_dump_KIND_LISPALLOC_core__ValueFrame_O
  /* 33 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionFrame_O
  /* 34 */ , &&obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 35 */ , &&obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 36 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 37 */ , &&obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 38 */ , &&obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 39 */ , &&obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 40 */ , &&obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 41 */ , &&obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 42 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 43 */ , &&obj_dump_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 44 */ , &&obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 45 */ , &&obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 46 */ , &&obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 47 */ , &&obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 48 */ , &&obj_dump_KIND_LISPALLOC_core__Array_O
  /* 49 */ , &&obj_dump_KIND_LISPALLOC_core__ArrayObjects_O
  /* 50 */ , &&obj_dump_KIND_LISPALLOC_core__ArrayDisplaced_O
  /* 51 */ , &&obj_dump_KIND_LISPALLOC_core__Vector_O
  /* 52 */ , &&obj_dump_KIND_LISPALLOC_core__BitVector_O
  /* 53 */ , &&obj_dump_KIND_LISPALLOC_core__SimpleBitVector_O
  /* 54 */ , &&obj_dump_KIND_LISPALLOC_core__BitVectorWithFillPtr_O
  /* 55 */ , &&obj_dump_KIND_LISPALLOC_core__VectorDisplaced_O
  /* 56 */ , &&obj_dump_KIND_LISPALLOC_core__String_O
  /* 57 */ , &&obj_dump_KIND_BOOTSTRAP_core__Str_O
  /* 58 */ , &&obj_dump_KIND_LISPALLOC_core__StrWithFillPtr_O
  /* 59 */ , &&obj_dump_KIND_LISPALLOC_core__VectorObjects_O
  /* 60 */ , &&obj_dump_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 61 */ , &&obj_dump_KIND_LISPALLOC_core__SingleDispatchMethod_O
  /* 62 */ , &&obj_dump_KIND_LISPALLOC_core__RandomState_O
  /* 63 */ , &&obj_dump_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O
  /* 64 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DebugLoc_O
  /* 65 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Attribute_O
  /* 66 */ , &&obj_dump_KIND_LISPALLOC_core__RegexMatch_O
  /* 67 */ , &&obj_dump_KIND_LISPALLOC_core__WeakPointer_O
  /* 68 */ , &&obj_dump_KIND_LISPALLOC_core__VaList_dummy_O
  /* 69 */ , &&obj_dump_KIND_BOOTSTRAP_core__StandardObject_O
  /* 70 */ , &&obj_dump_KIND_BOOTSTRAP_core__Metaobject_O
  /* 71 */ , &&obj_dump_KIND_BOOTSTRAP_core__Specializer_O
  /* 72 */ , &&obj_dump_KIND_BOOTSTRAP_core__Class_O
  /* 73 */ , &&obj_dump_KIND_BOOTSTRAP_core__StdClass_O
  /* 74 */ , &&obj_dump_KIND_BOOTSTRAP_core__StandardClass_O
  /* 75 */ , &&obj_dump_KIND_LISPALLOC_core__FuncallableStandardClass_O
  /* 76 */ , &&obj_dump_KIND_BOOTSTRAP_core__StructureClass_O
  /* 77 */ , &&obj_dump_KIND_LISPALLOC_core__ForwardReferencedClass_O
  /* 78 */ , &&obj_dump_KIND_LISPALLOC_core__CxxClass_O
  /* 79 */ , &&obj_dump_KIND_BOOTSTRAP_core__BuiltInClass_O
  /* 80 */ , &&obj_dump_KIND_LISPALLOC_clbind__ClassRep_O
  /* 81 */ , &&obj_dump_KIND_LISPALLOC_core__ExternalObject_O
  /* 82 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Value_O
  /* 83 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Argument_O
  /* 84 */ , &&obj_dump_KIND_LISPALLOC_llvmo__User_O
  /* 85 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Instruction_O
  /* 86 */ , &&obj_dump_KIND_LISPALLOC_llvmo__AtomicRMWInst_O
  /* 87 */ , &&obj_dump_KIND_LISPALLOC_llvmo__LandingPadInst_O
  /* 88 */ , &&obj_dump_KIND_LISPALLOC_llvmo__PHINode_O
  /* 89 */ , &&obj_dump_KIND_LISPALLOC_llvmo__CallInst_O
  /* 90 */ , &&obj_dump_KIND_LISPALLOC_llvmo__StoreInst_O
  /* 91 */ , &&obj_dump_KIND_LISPALLOC_llvmo__UnaryInstruction_O
  /* 92 */ , &&obj_dump_KIND_LISPALLOC_llvmo__LoadInst_O
  /* 93 */ , &&obj_dump_KIND_LISPALLOC_llvmo__AllocaInst_O
  /* 94 */ , &&obj_dump_KIND_LISPALLOC_llvmo__VAArgInst_O
  /* 95 */ , &&obj_dump_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O
  /* 96 */ , &&obj_dump_KIND_LISPALLOC_llvmo__TerminatorInst_O
  /* 97 */ , &&obj_dump_KIND_LISPALLOC_llvmo__UnreachableInst_O
  /* 98 */ , &&obj_dump_KIND_LISPALLOC_llvmo__SwitchInst_O
  /* 99 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ReturnInst_O
  /* 100 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ResumeInst_O
  /* 101 */ , &&obj_dump_KIND_LISPALLOC_llvmo__BranchInst_O
  /* 102 */ , &&obj_dump_KIND_LISPALLOC_llvmo__InvokeInst_O
  /* 103 */ , &&obj_dump_KIND_LISPALLOC_llvmo__IndirectBrInst_O
  /* 104 */ , &&obj_dump_KIND_LISPALLOC_llvmo__FenceInst_O
  /* 105 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Constant_O
  /* 106 */ , &&obj_dump_KIND_LISPALLOC_llvmo__BlockAddress_O
  /* 107 */ , &&obj_dump_KIND_LISPALLOC_llvmo__GlobalValue_O
  /* 108 */ , &&obj_dump_KIND_LISPALLOC_llvmo__GlobalVariable_O
  /* 109 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Function_O
  /* 110 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantArray_O
  /* 111 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantInt_O
  /* 112 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantDataSequential_O
  /* 113 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantDataArray_O
  /* 114 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantStruct_O
  /* 115 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantFP_O
  /* 116 */ , &&obj_dump_KIND_LISPALLOC_llvmo__UndefValue_O
  /* 117 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantPointerNull_O
  /* 118 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ConstantExpr_O
  /* 119 */ , &&obj_dump_KIND_LISPALLOC_llvmo__BasicBlock_O
  /* 120 */ , &&obj_dump_KIND_LISPALLOC_llvmo__IRBuilderBase_O
  /* 121 */ , &&obj_dump_KIND_LISPALLOC_llvmo__IRBuilder_O
  /* 122 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIBuilder_O
  /* 123 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Metadata_O
  /* 124 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ValueAsMetadata_O
  /* 125 */ , &&obj_dump_KIND_LISPALLOC_llvmo__MDNode_O
  /* 126 */ , &&obj_dump_KIND_LISPALLOC_llvmo__MDString_O
  /* 127 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ExecutionEngine_O
  /* 128 */ , &&obj_dump_KIND_LISPALLOC_llvmo__APFloat_O
  /* 129 */ , &&obj_dump_KIND_LISPALLOC_llvmo__PassManagerBuilder_O
  /* 130 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DataLayout_O
  /* 131 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Triple_O
  /* 132 */ , &&obj_dump_KIND_LISPALLOC_llvmo__APInt_O
  /* 133 */ , &&obj_dump_KIND_LISPALLOC_llvmo__PassManagerBase_O
  /* 134 */ , &&obj_dump_KIND_LISPALLOC_llvmo__FunctionPassManager_O
  /* 135 */ , &&obj_dump_KIND_LISPALLOC_llvmo__PassManager_O
  /* 136 */ , &&obj_dump_KIND_LISPALLOC_llvmo__TargetMachine_O
  /* 137 */ , &&obj_dump_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O
  /* 138 */ , &&obj_dump_KIND_LISPALLOC_llvmo__TargetOptions_O
  /* 139 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Type_O
  /* 140 */ , &&obj_dump_KIND_LISPALLOC_llvmo__IntegerType_O
  /* 141 */ , &&obj_dump_KIND_LISPALLOC_llvmo__CompositeType_O
  /* 142 */ , &&obj_dump_KIND_LISPALLOC_llvmo__SequentialType_O
  /* 143 */ , &&obj_dump_KIND_LISPALLOC_llvmo__VectorType_O
  /* 144 */ , &&obj_dump_KIND_LISPALLOC_llvmo__PointerType_O
  /* 145 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ArrayType_O
  /* 146 */ , &&obj_dump_KIND_LISPALLOC_llvmo__StructType_O
  /* 147 */ , &&obj_dump_KIND_LISPALLOC_llvmo__FunctionType_O
  /* 148 */ , &&obj_dump_KIND_LISPALLOC_llvmo__NamedMDNode_O
  /* 149 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Linker_O
  /* 150 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Pass_O
  /* 151 */ , &&obj_dump_KIND_LISPALLOC_llvmo__FunctionPass_O
  /* 152 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ModulePass_O
  /* 153 */ , &&obj_dump_KIND_LISPALLOC_llvmo__ImmutablePass_O
  /* 154 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DataLayoutPass_O
  /* 155 */ , &&obj_dump_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O
  /* 156 */ , &&obj_dump_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O
  /* 157 */ , &&obj_dump_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O
  /* 158 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Module_O
  /* 159 */ , &&obj_dump_KIND_LISPALLOC_llvmo__EngineBuilder_O
  /* 160 */ , &&obj_dump_KIND_LISPALLOC_core__ForeignData_O
  /* 161 */ , &&obj_dump_KIND_LISPALLOC_llvmo__LLVMContext_O
  /* 162 */ , &&obj_dump_KIND_LISPALLOC_llvmo__Target_O
  /* 163 */ , &&obj_dump_KIND_LISPALLOC_core__LoadTimeValues_O
  /* 164 */ , &&obj_dump_KIND_LISPALLOC_core__Binder_O
  /* 165 */ , &&obj_dump_KIND_LISPALLOC_core__IntArray_O
  /* 166 */ , &&obj_dump_KIND_LISPALLOC_core__SourceManager_O
  /* 167 */ , &&obj_dump_KIND_LISPALLOC_core__Record_O
  /* 168 */ , &&obj_dump_KIND_LISPALLOC_core__LightUserData_O
  /* 169 */ , &&obj_dump_KIND_LISPALLOC_core__UserData_O
  /* 170 */ , &&obj_dump_KIND_BOOTSTRAP_core__Symbol_O
  /* 171 */ , &&obj_dump_KIND_LISPALLOC_core__Null_O
  /* 172 */ , &&obj_dump_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 173 */ , &&obj_dump_KIND_TEMPLATED_LISPALLOC_core__Iterator_O
  /* 174 */ , &&obj_dump_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 175 */ , &&obj_dump_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 176 */ , &&obj_dump_KIND_LISPALLOC_core__Regex_O
  /* 177 */ , &&obj_dump_KIND_LISPALLOC_core__PosixTimeDuration_O
  /* 178 */ , &&obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 179 */ , &&obj_dump_KIND_LISPALLOC_core__CandoException_O
  /* 180 */ , &&obj_dump_KIND_LISPALLOC_core__Stream_O
  /* 181 */ , &&obj_dump_KIND_LISPALLOC_core__AnsiStream_O
  /* 182 */ , &&obj_dump_KIND_LISPALLOC_core__FileStream_O
  /* 183 */ , &&obj_dump_KIND_LISPALLOC_core__IOStreamStream_O
  /* 184 */ , &&obj_dump_KIND_LISPALLOC_core__IOFileStream_O
  /* 185 */ , &&obj_dump_KIND_LISPALLOC_core__ConcatenatedStream_O
  /* 186 */ , &&obj_dump_KIND_LISPALLOC_core__StringStream_O
  /* 187 */ , &&obj_dump_KIND_LISPALLOC_core__StringInputStream_O
  /* 188 */ , &&obj_dump_KIND_LISPALLOC_core__StringOutputStream_O
  /* 189 */ , &&obj_dump_KIND_LISPALLOC_core__SynonymStream_O
  /* 190 */ , &&obj_dump_KIND_LISPALLOC_core__EchoStream_O
  /* 191 */ , &&obj_dump_KIND_LISPALLOC_core__TwoWayStream_O
  /* 192 */ , &&obj_dump_KIND_LISPALLOC_core__BroadcastStream_O
  /* 193 */ , &&obj_dump_KIND_LISPALLOC_core__Reader_O
  /* 194 */ , &&obj_dump_KIND_LISPALLOC_core__Cons_O
  /* 195 */ , &&obj_dump_KIND_LISPALLOC_core__Archive_O
  /* 196 */ , &&obj_dump_KIND_LISPALLOC_core__SaveArchive_O
  /* 197 */ , &&obj_dump_KIND_LISPALLOC_core__SexpSaveArchive_O
  /* 198 */ , &&obj_dump_KIND_LISPALLOC_core__LoadArchive_O
  /* 199 */ , &&obj_dump_KIND_LISPALLOC_core__SexpLoadArchive_O
  /* 200 */ , &&obj_dump_KIND_LISPALLOC_core__HashTable_O
  /* 201 */ , &&obj_dump_KIND_LISPALLOC_core__HashTableEq_O
  /* 202 */ , &&obj_dump_KIND_LISPALLOC_core__HashTableEqualp_O
  /* 203 */ , &&obj_dump_KIND_LISPALLOC_core__HashTableEql_O
  /* 204 */ , &&obj_dump_KIND_LISPALLOC_core__HashTableEqual_O
  /* 205 */ , &&obj_dump_KIND_LISPALLOC_cffi__Pointer_O
  /* 206 */ , &&obj_dump_KIND_LISPALLOC_core__CxxObject_O
  /* 207 */ , &&obj_dump_KIND_LISPALLOC_core__WeakKeyMapping_O
  /* 208 */ , &&obj_dump_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 209 */ , &&obj_dump_KIND_LISPALLOC_llvmo__InsertPoint_O
  /* 210 */ , &&obj_dump_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 211 */ , &&obj_dump_KIND_LISPALLOC_core__SNode_O
  /* 212 */ , &&obj_dump_KIND_LISPALLOC_core__LeafSNode_O
  /* 213 */ , &&obj_dump_KIND_LISPALLOC_core__BranchSNode_O
  /* 214 */ , &&obj_dump_KIND_LISPALLOC_core__Path_O
  /* 215 */ , &&obj_dump_KIND_LISPALLOC_asttooling__AstVisitor_O
  /* 216 */ , &&obj_dump_KIND_LISPALLOC_llvmo__AttributeSet_O
  /* 217 */ , &&obj_dump_KIND_LISPALLOC_core__StructureObject_O
  /* 218 */ , &&obj_dump_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 219 */ , &&obj_dump_KIND_LISPALLOC_core__Package_O
  /* 220 */ , &&obj_dump_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 221 */ , &&obj_dump_KIND_LISPALLOC_core__Character_dummy_O
  /* 222 */ , &&obj_dump_KIND_LISPALLOC_core__Function_O
  /* 223 */ , &&obj_dump_KIND_LISPALLOC_core__CompiledFunction_O
  /* 224 */ , &&obj_dump_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O
  /* 225 */ , &&obj_dump_KIND_LISPALLOC_core__SpecialForm_O
  /* 226 */ , &&obj_dump_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O
  /* 227 */ , &&obj_dump_KIND_LISPALLOC_core__Instance_O
  /* 228 */ , &&obj_dump_KIND_LISPALLOC_core__Pointer_O
  /* 229 */ , &&obj_dump_KIND_LISPALLOC_clbind__ClassRegistry_O
  /* 230 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DebugInfo_O
  /* 231 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIDerivedType_O
  /* 232 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIArray_O
  /* 233 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIBasicType_O
  /* 234 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DISubprogram_O
  /* 235 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DILexicalBlock_O
  /* 236 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DICompileUnit_O
  /* 237 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIDescriptor_O
  /* 238 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIType_O
  /* 239 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DISubroutineType_O
  /* 240 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DICompositeType_O
  /* 241 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DITypeArray_O
  /* 242 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIFile_O
  /* 243 */ , &&obj_dump_KIND_LISPALLOC_llvmo__DIScope_O
  /* 244 */ , &&obj_dump_KIND_LISPALLOC_core__SmallMultimap_O
  /* 245 */ , &&obj_dump_KIND_LISPALLOC_core__Pathname_O
  /* 246 */ , &&obj_dump_KIND_LISPALLOC_core__LogicalPathname_O
  /* 247 */ , &&obj_dump_KIND_LISPALLOC_core__PosixTime_O
  /* 248 */ , &&obj_dump_KIND_LISPALLOC_core__SmallMap_O
  /* 249 */ , &&obj_dump_KIND_CLASSALLOC_core__Cache
  /* 250 */ , &&obj_dump_KIND_ROOTCLASSALLOC_core__Lisp_O
  /* 251 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 252 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 253 */ , &&obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 254 */ , &&obj_dump_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure
  /* 255 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 256 */ , &&obj_dump_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 257 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 258 */ , &&obj_dump_KIND_CLASSALLOC_llvmo__CompiledClosure
  /* 259 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 260 */ , &&obj_dump_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor
  /* 261 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___
  /* 262 */ , &&obj_dump_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor
  /* 263 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_
  /* 264 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_
  /* 265 */ , &&obj_dump_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor
  /* 266 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_
  /* 267 */ , &&obj_dump_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor
  /* 268 */ , &&obj_dump_KIND_CLASSALLOC_core__MacroClosure
  /* 269 */ , &&obj_dump_KIND_CLASSALLOC_core__ConsStepper
  /* 270 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 271 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_
  /* 272 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 273 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 274 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_
  /* 275 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___
  /* 276 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 277 */ , &&obj_dump_KIND_CLASSALLOC_core__InstanceClosure
  /* 278 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 279 */ , &&obj_dump_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory
  /* 280 */ , &&obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_
  /* 281 */ , &&obj_dump_KIND_LISPALLOC_asttooling__DerivableMatchCallback
  /* 282 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_
  /* 283 */ , &&obj_dump_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction
  /* 284 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_
  /* 285 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__
  /* 286 */ , &&obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_
  /* 287 */ , &&obj_dump_KIND_CLASSALLOC_core__CoreExposer
  /* 288 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 289 */ , &&obj_dump_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction
  /* 290 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__
  /* 291 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 292 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 293 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 294 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 295 */ , &&obj_dump_KIND_CLASSALLOC_core__InterpretedClosure
  /* 296 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 297 */ , &&obj_dump_KIND_CLASSALLOC_core__VectorStepper
  /* 298 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__
};
#endif // defined(GC_OBJ_DUMP_MAP_TABLE)
#if defined(GC_OBJ_SKIP)
obj_skip_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps:
{
    typedef asttooling::RegMap::RegistryMaps type_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps;
    client = (char*)client + AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
    typedef clbind::detail::class_map type_KIND_ROOTCLASSALLOC_clbind__detail__class_map;
    client = (char*)client + AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_clbind__detail__class_map)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_clbind__DummyCreator:
{
    typedef clbind::DummyCreator type_KIND_CLASSALLOC_clbind__DummyCreator;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_clbind__DummyCreator)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__InstanceCreator:
{
    typedef core::InstanceCreator type_KIND_CLASSALLOC_core__InstanceCreator;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__InstanceCreator)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__T_O:
{
    typedef core::T_O type_KIND_BOOTSTRAP_core__T_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__T_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
    typedef core::MultiStringBuffer_O type_KIND_LISPALLOC_core__MultiStringBuffer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MultiStringBuffer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ReadTable_O:
{
    typedef core::ReadTable_O type_KIND_LISPALLOC_core__ReadTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ReadTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Complex_O:
{
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Real_O:
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Rational_O:
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Bignum_O:
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Ratio_O:
{
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ShortFloat_O:
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FileStatus_O:
{
    typedef core::FileStatus_O type_KIND_LISPALLOC_core__FileStatus_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStatus_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakHashTable_O:
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ActivationFrame_O:
{
    typedef core::ActivationFrame_O type_KIND_LISPALLOC_core__ActivationFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ActivationFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ValueFrame_O:
{
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionFrame_O:
{
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    typedef core::StackValueEnvironment_O type_KIND_LISPALLOC_core__StackValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StackValueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Array_O:
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ArrayObjects_O:
{
    typedef core::ArrayObjects_O type_KIND_LISPALLOC_core__ArrayObjects_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ArrayObjects_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
    typedef core::ArrayDisplaced_O type_KIND_LISPALLOC_core__ArrayDisplaced_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ArrayDisplaced_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__BitVector_O:
{
    typedef core::BitVector_O type_KIND_LISPALLOC_core__BitVector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BitVector_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SimpleBitVector_O:
{
    typedef core::SimpleBitVector_O type_KIND_LISPALLOC_core__SimpleBitVector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SimpleBitVector_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
    typedef core::BitVectorWithFillPtr_O type_KIND_LISPALLOC_core__BitVectorWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BitVectorWithFillPtr_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VectorDisplaced_O:
{
    typedef core::VectorDisplaced_O type_KIND_LISPALLOC_core__VectorDisplaced_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorDisplaced_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__String_O:
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Str_O:
{
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
    typedef core::StrWithFillPtr_O type_KIND_LISPALLOC_core__StrWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StrWithFillPtr_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VectorObjects_O:
{
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    typedef core::VectorObjectsWithFillPtr_O type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
    typedef core::SingleDispatchMethod_O type_KIND_LISPALLOC_core__SingleDispatchMethod_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchMethod_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__RandomState_O:
{
    typedef core::RandomState_O type_KIND_LISPALLOC_core__RandomState_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RandomState_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
    typedef llvmo::DebugLoc_O type_KIND_LISPALLOC_llvmo__DebugLoc_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DebugLoc_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Attribute_O:
{
    typedef llvmo::Attribute_O type_KIND_LISPALLOC_llvmo__Attribute_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Attribute_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__RegexMatch_O:
{
    typedef core::RegexMatch_O type_KIND_LISPALLOC_core__RegexMatch_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RegexMatch_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakPointer_O:
{
    typedef core::WeakPointer_O type_KIND_LISPALLOC_core__WeakPointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakPointer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VaList_dummy_O:
{
    typedef core::VaList_dummy_O type_KIND_LISPALLOC_core__VaList_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VaList_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Class_O:
{
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__StdClass_O:
{
    typedef core::StdClass_O type_KIND_BOOTSTRAP_core__StdClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StdClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__StandardClass_O:
{
    typedef core::StandardClass_O type_KIND_BOOTSTRAP_core__StandardClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
    typedef core::FuncallableStandardClass_O type_KIND_LISPALLOC_core__FuncallableStandardClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FuncallableStandardClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__StructureClass_O:
{
    typedef core::StructureClass_O type_KIND_BOOTSTRAP_core__StructureClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StructureClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
    typedef core::ForwardReferencedClass_O type_KIND_LISPALLOC_core__ForwardReferencedClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ForwardReferencedClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CxxClass_O:
{
    typedef core::CxxClass_O type_KIND_LISPALLOC_core__CxxClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    typedef core::BuiltInClass_O type_KIND_BOOTSTRAP_core__BuiltInClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__BuiltInClass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_clbind__ClassRep_O:
{
    typedef clbind::ClassRep_O type_KIND_LISPALLOC_clbind__ClassRep_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_clbind__ClassRep_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ExternalObject_O:
{
    typedef core::ExternalObject_O type_KIND_LISPALLOC_core__ExternalObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ExternalObject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Value_O:
{
    typedef llvmo::Value_O type_KIND_LISPALLOC_llvmo__Value_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Value_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Argument_O:
{
    typedef llvmo::Argument_O type_KIND_LISPALLOC_llvmo__Argument_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Argument_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__User_O:
{
    typedef llvmo::User_O type_KIND_LISPALLOC_llvmo__User_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__User_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Instruction_O:
{
    typedef llvmo::Instruction_O type_KIND_LISPALLOC_llvmo__Instruction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Instruction_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    typedef llvmo::AtomicRMWInst_O type_KIND_LISPALLOC_llvmo__AtomicRMWInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AtomicRMWInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    typedef llvmo::LandingPadInst_O type_KIND_LISPALLOC_llvmo__LandingPadInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LandingPadInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__PHINode_O:
{
    typedef llvmo::PHINode_O type_KIND_LISPALLOC_llvmo__PHINode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PHINode_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__CallInst_O:
{
    typedef llvmo::CallInst_O type_KIND_LISPALLOC_llvmo__CallInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__CallInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    typedef llvmo::StoreInst_O type_KIND_LISPALLOC_llvmo__StoreInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__StoreInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    typedef llvmo::UnaryInstruction_O type_KIND_LISPALLOC_llvmo__UnaryInstruction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UnaryInstruction_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    typedef llvmo::LoadInst_O type_KIND_LISPALLOC_llvmo__LoadInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LoadInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    typedef llvmo::AllocaInst_O type_KIND_LISPALLOC_llvmo__AllocaInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AllocaInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    typedef llvmo::VAArgInst_O type_KIND_LISPALLOC_llvmo__VAArgInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__VAArgInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    typedef llvmo::AtomicCmpXchgInst_O type_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    typedef llvmo::TerminatorInst_O type_KIND_LISPALLOC_llvmo__TerminatorInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TerminatorInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    typedef llvmo::UnreachableInst_O type_KIND_LISPALLOC_llvmo__UnreachableInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UnreachableInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    typedef llvmo::SwitchInst_O type_KIND_LISPALLOC_llvmo__SwitchInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__SwitchInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    typedef llvmo::ReturnInst_O type_KIND_LISPALLOC_llvmo__ReturnInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ReturnInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    typedef llvmo::ResumeInst_O type_KIND_LISPALLOC_llvmo__ResumeInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ResumeInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    typedef llvmo::BranchInst_O type_KIND_LISPALLOC_llvmo__BranchInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BranchInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    typedef llvmo::InvokeInst_O type_KIND_LISPALLOC_llvmo__InvokeInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__InvokeInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    typedef llvmo::IndirectBrInst_O type_KIND_LISPALLOC_llvmo__IndirectBrInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IndirectBrInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    typedef llvmo::FenceInst_O type_KIND_LISPALLOC_llvmo__FenceInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FenceInst_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Constant_O:
{
    typedef llvmo::Constant_O type_KIND_LISPALLOC_llvmo__Constant_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Constant_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    typedef llvmo::BlockAddress_O type_KIND_LISPALLOC_llvmo__BlockAddress_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BlockAddress_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    typedef llvmo::GlobalValue_O type_KIND_LISPALLOC_llvmo__GlobalValue_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__GlobalValue_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    typedef llvmo::GlobalVariable_O type_KIND_LISPALLOC_llvmo__GlobalVariable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__GlobalVariable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Function_O:
{
    typedef llvmo::Function_O type_KIND_LISPALLOC_llvmo__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Function_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    typedef llvmo::ConstantArray_O type_KIND_LISPALLOC_llvmo__ConstantArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantArray_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    typedef llvmo::ConstantInt_O type_KIND_LISPALLOC_llvmo__ConstantInt_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantInt_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    typedef llvmo::ConstantDataSequential_O type_KIND_LISPALLOC_llvmo__ConstantDataSequential_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantDataSequential_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    typedef llvmo::ConstantDataArray_O type_KIND_LISPALLOC_llvmo__ConstantDataArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantDataArray_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    typedef llvmo::ConstantStruct_O type_KIND_LISPALLOC_llvmo__ConstantStruct_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantStruct_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    typedef llvmo::ConstantFP_O type_KIND_LISPALLOC_llvmo__ConstantFP_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantFP_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    typedef llvmo::UndefValue_O type_KIND_LISPALLOC_llvmo__UndefValue_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UndefValue_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    typedef llvmo::ConstantPointerNull_O type_KIND_LISPALLOC_llvmo__ConstantPointerNull_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantPointerNull_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    typedef llvmo::ConstantExpr_O type_KIND_LISPALLOC_llvmo__ConstantExpr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantExpr_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    typedef llvmo::BasicBlock_O type_KIND_LISPALLOC_llvmo__BasicBlock_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BasicBlock_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    typedef llvmo::IRBuilderBase_O type_KIND_LISPALLOC_llvmo__IRBuilderBase_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IRBuilderBase_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    typedef llvmo::IRBuilder_O type_KIND_LISPALLOC_llvmo__IRBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IRBuilder_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
    typedef llvmo::DIBuilder_O type_KIND_LISPALLOC_llvmo__DIBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIBuilder_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Metadata_O:
{
    typedef llvmo::Metadata_O type_KIND_LISPALLOC_llvmo__Metadata_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Metadata_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    typedef llvmo::ValueAsMetadata_O type_KIND_LISPALLOC_llvmo__ValueAsMetadata_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ValueAsMetadata_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__MDNode_O:
{
    typedef llvmo::MDNode_O type_KIND_LISPALLOC_llvmo__MDNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MDNode_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__MDString_O:
{
    typedef llvmo::MDString_O type_KIND_LISPALLOC_llvmo__MDString_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MDString_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    typedef llvmo::ExecutionEngine_O type_KIND_LISPALLOC_llvmo__ExecutionEngine_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ExecutionEngine_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__APFloat_O:
{
    typedef llvmo::APFloat_O type_KIND_LISPALLOC_llvmo__APFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__APFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    typedef llvmo::PassManagerBuilder_O type_KIND_LISPALLOC_llvmo__PassManagerBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManagerBuilder_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    typedef llvmo::DataLayout_O type_KIND_LISPALLOC_llvmo__DataLayout_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DataLayout_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Triple_O:
{
    typedef llvmo::Triple_O type_KIND_LISPALLOC_llvmo__Triple_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Triple_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__APInt_O:
{
    typedef llvmo::APInt_O type_KIND_LISPALLOC_llvmo__APInt_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__APInt_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    typedef llvmo::PassManagerBase_O type_KIND_LISPALLOC_llvmo__PassManagerBase_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManagerBase_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    typedef llvmo::FunctionPassManager_O type_KIND_LISPALLOC_llvmo__FunctionPassManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionPassManager_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__PassManager_O:
{
    typedef llvmo::PassManager_O type_KIND_LISPALLOC_llvmo__PassManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManager_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    typedef llvmo::TargetMachine_O type_KIND_LISPALLOC_llvmo__TargetMachine_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetMachine_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    typedef llvmo::LLVMTargetMachine_O type_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    typedef llvmo::TargetOptions_O type_KIND_LISPALLOC_llvmo__TargetOptions_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetOptions_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Type_O:
{
    typedef llvmo::Type_O type_KIND_LISPALLOC_llvmo__Type_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Type_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    typedef llvmo::IntegerType_O type_KIND_LISPALLOC_llvmo__IntegerType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IntegerType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    typedef llvmo::CompositeType_O type_KIND_LISPALLOC_llvmo__CompositeType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__CompositeType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    typedef llvmo::SequentialType_O type_KIND_LISPALLOC_llvmo__SequentialType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__SequentialType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__VectorType_O:
{
    typedef llvmo::VectorType_O type_KIND_LISPALLOC_llvmo__VectorType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__VectorType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__PointerType_O:
{
    typedef llvmo::PointerType_O type_KIND_LISPALLOC_llvmo__PointerType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PointerType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    typedef llvmo::ArrayType_O type_KIND_LISPALLOC_llvmo__ArrayType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ArrayType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__StructType_O:
{
    typedef llvmo::StructType_O type_KIND_LISPALLOC_llvmo__StructType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__StructType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    typedef llvmo::FunctionType_O type_KIND_LISPALLOC_llvmo__FunctionType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    typedef llvmo::NamedMDNode_O type_KIND_LISPALLOC_llvmo__NamedMDNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__NamedMDNode_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Linker_O:
{
    typedef llvmo::Linker_O type_KIND_LISPALLOC_llvmo__Linker_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Linker_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Pass_O:
{
    typedef llvmo::Pass_O type_KIND_LISPALLOC_llvmo__Pass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Pass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    typedef llvmo::FunctionPass_O type_KIND_LISPALLOC_llvmo__FunctionPass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionPass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    typedef llvmo::ModulePass_O type_KIND_LISPALLOC_llvmo__ModulePass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ModulePass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    typedef llvmo::ImmutablePass_O type_KIND_LISPALLOC_llvmo__ImmutablePass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ImmutablePass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    typedef llvmo::DataLayoutPass_O type_KIND_LISPALLOC_llvmo__DataLayoutPass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DataLayoutPass_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    typedef llvmo::TargetLibraryInfo_O type_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    typedef llvmo::MCSubtargetInfo_O type_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    typedef llvmo::TargetSubtargetInfo_O type_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Module_O:
{
    typedef llvmo::Module_O type_KIND_LISPALLOC_llvmo__Module_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Module_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    typedef llvmo::EngineBuilder_O type_KIND_LISPALLOC_llvmo__EngineBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__EngineBuilder_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ForeignData_O:
{
    typedef core::ForeignData_O type_KIND_LISPALLOC_core__ForeignData_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ForeignData_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    typedef llvmo::LLVMContext_O type_KIND_LISPALLOC_llvmo__LLVMContext_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LLVMContext_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__Target_O:
{
    typedef llvmo::Target_O type_KIND_LISPALLOC_llvmo__Target_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Target_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LoadTimeValues_O:
{
    typedef core::LoadTimeValues_O type_KIND_LISPALLOC_core__LoadTimeValues_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadTimeValues_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Binder_O:
{
    typedef core::Binder_O type_KIND_LISPALLOC_core__Binder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Binder_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__IntArray_O:
{
    typedef core::IntArray_O type_KIND_LISPALLOC_core__IntArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__IntArray_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SourceManager_O:
{
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Record_O:
{
    typedef core::Record_O type_KIND_LISPALLOC_core__Record_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Record_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LightUserData_O:
{
    typedef core::LightUserData_O type_KIND_LISPALLOC_core__LightUserData_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LightUserData_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__UserData_O:
{
    typedef core::UserData_O type_KIND_LISPALLOC_core__UserData_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UserData_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Symbol_O:
{
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Null_O:
{
    typedef core::Null_O type_KIND_LISPALLOC_core__Null_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Null_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    typedef core::DirectoryIterator_O type_KIND_LISPALLOC_core__DirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryIterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    typedef core::RecursiveDirectoryIterator_O type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Regex_O:
{
    typedef core::Regex_O type_KIND_LISPALLOC_core__Regex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Regex_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
    typedef core::PosixTimeDuration_O type_KIND_LISPALLOC_core__PosixTimeDuration_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__PosixTimeDuration_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CandoException_O:
{
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Stream_O:
{
    typedef core::Stream_O type_KIND_LISPALLOC_core__Stream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Stream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__AnsiStream_O:
{
    typedef core::AnsiStream_O type_KIND_LISPALLOC_core__AnsiStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__AnsiStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FileStream_O:
{
    typedef core::FileStream_O type_KIND_LISPALLOC_core__FileStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__IOStreamStream_O:
{
    typedef core::IOStreamStream_O type_KIND_LISPALLOC_core__IOStreamStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__IOStreamStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__IOFileStream_O:
{
    typedef core::IOFileStream_O type_KIND_LISPALLOC_core__IOFileStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__IOFileStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
    typedef core::ConcatenatedStream_O type_KIND_LISPALLOC_core__ConcatenatedStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ConcatenatedStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StringStream_O:
{
    typedef core::StringStream_O type_KIND_LISPALLOC_core__StringStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StringStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StringInputStream_O:
{
    typedef core::StringInputStream_O type_KIND_LISPALLOC_core__StringInputStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StringInputStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StringOutputStream_O:
{
    typedef core::StringOutputStream_O type_KIND_LISPALLOC_core__StringOutputStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StringOutputStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SynonymStream_O:
{
    typedef core::SynonymStream_O type_KIND_LISPALLOC_core__SynonymStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SynonymStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__EchoStream_O:
{
    typedef core::EchoStream_O type_KIND_LISPALLOC_core__EchoStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__EchoStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__TwoWayStream_O:
{
    typedef core::TwoWayStream_O type_KIND_LISPALLOC_core__TwoWayStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TwoWayStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__BroadcastStream_O:
{
    typedef core::BroadcastStream_O type_KIND_LISPALLOC_core__BroadcastStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BroadcastStream_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Reader_O:
{
    typedef core::Reader_O type_KIND_LISPALLOC_core__Reader_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Reader_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Cons_O:
{
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Archive_O:
{
    typedef core::Archive_O type_KIND_LISPALLOC_core__Archive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Archive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SaveArchive_O:
{
    typedef core::SaveArchive_O type_KIND_LISPALLOC_core__SaveArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SaveArchive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
    typedef core::SexpSaveArchive_O type_KIND_LISPALLOC_core__SexpSaveArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SexpSaveArchive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LoadArchive_O:
{
    typedef core::LoadArchive_O type_KIND_LISPALLOC_core__LoadArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadArchive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
    typedef core::SexpLoadArchive_O type_KIND_LISPALLOC_core__SexpLoadArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SexpLoadArchive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__HashTable_O:
{
    typedef core::HashTable_O type_KIND_LISPALLOC_core__HashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__HashTableEq_O:
{
    typedef core::HashTableEq_O type_KIND_LISPALLOC_core__HashTableEq_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEq_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    typedef core::HashTableEqualp_O type_KIND_LISPALLOC_core__HashTableEqualp_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEqualp_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__HashTableEql_O:
{
    typedef core::HashTableEql_O type_KIND_LISPALLOC_core__HashTableEql_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEql_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__HashTableEqual_O:
{
    typedef core::HashTableEqual_O type_KIND_LISPALLOC_core__HashTableEqual_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEqual_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_cffi__Pointer_O:
{
    typedef cffi::Pointer_O type_KIND_LISPALLOC_cffi__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_cffi__Pointer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
    typedef core::WeakKeyMapping_O type_KIND_LISPALLOC_core__WeakKeyMapping_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyMapping_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    typedef core::LambdaListHandler_O type_KIND_LISPALLOC_core__LambdaListHandler_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LambdaListHandler_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
    typedef llvmo::InsertPoint_O type_KIND_LISPALLOC_llvmo__InsertPoint_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__InsertPoint_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SNode_O:
{
    typedef core::SNode_O type_KIND_LISPALLOC_core__SNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SNode_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LeafSNode_O:
{
    typedef core::LeafSNode_O type_KIND_LISPALLOC_core__LeafSNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LeafSNode_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__BranchSNode_O:
{
    typedef core::BranchSNode_O type_KIND_LISPALLOC_core__BranchSNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BranchSNode_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Path_O:
{
    typedef core::Path_O type_KIND_LISPALLOC_core__Path_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Path_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
    typedef asttooling::AstVisitor_O type_KIND_LISPALLOC_asttooling__AstVisitor_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__AstVisitor_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    typedef llvmo::AttributeSet_O type_KIND_LISPALLOC_llvmo__AttributeSet_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AttributeSet_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StructureObject_O:
{
    typedef core::StructureObject_O type_KIND_LISPALLOC_core__StructureObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StructureObject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    typedef core::InvocationHistoryFrameIterator_O type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Package_O:
{
    typedef core::Package_O type_KIND_LISPALLOC_core__Package_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Package_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    typedef core::DirectoryEntry_O type_KIND_LISPALLOC_core__DirectoryEntry_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryEntry_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Character_dummy_O:
{
    typedef core::Character_dummy_O type_KIND_LISPALLOC_core__Character_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Character_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Function_O:
{
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CompiledFunction_O:
{
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
    typedef core::SingleDispatchGenericFunction_O type_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SpecialForm_O:
{
    typedef core::SpecialForm_O type_KIND_LISPALLOC_core__SpecialForm_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SpecialForm_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
    typedef core::SingleDispatchEffectiveMethodFunction_O type_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Instance_O:
{
    typedef core::Instance_O type_KIND_LISPALLOC_core__Instance_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Instance_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Pointer_O:
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    typedef clbind::ClassRegistry_O type_KIND_LISPALLOC_clbind__ClassRegistry_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_clbind__ClassRegistry_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
    typedef llvmo::DebugInfo_O type_KIND_LISPALLOC_llvmo__DebugInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DebugInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
    typedef llvmo::DIDerivedType_O type_KIND_LISPALLOC_llvmo__DIDerivedType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIDerivedType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIArray_O:
{
    typedef llvmo::DIArray_O type_KIND_LISPALLOC_llvmo__DIArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIArray_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
    typedef llvmo::DIBasicType_O type_KIND_LISPALLOC_llvmo__DIBasicType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIBasicType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
    typedef llvmo::DISubprogram_O type_KIND_LISPALLOC_llvmo__DISubprogram_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DISubprogram_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
    typedef llvmo::DILexicalBlock_O type_KIND_LISPALLOC_llvmo__DILexicalBlock_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DILexicalBlock_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
    typedef llvmo::DICompileUnit_O type_KIND_LISPALLOC_llvmo__DICompileUnit_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DICompileUnit_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
    typedef llvmo::DIDescriptor_O type_KIND_LISPALLOC_llvmo__DIDescriptor_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIDescriptor_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIType_O:
{
    typedef llvmo::DIType_O type_KIND_LISPALLOC_llvmo__DIType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
    typedef llvmo::DISubroutineType_O type_KIND_LISPALLOC_llvmo__DISubroutineType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DISubroutineType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
    typedef llvmo::DICompositeType_O type_KIND_LISPALLOC_llvmo__DICompositeType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DICompositeType_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
    typedef llvmo::DITypeArray_O type_KIND_LISPALLOC_llvmo__DITypeArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DITypeArray_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIFile_O:
{
    typedef llvmo::DIFile_O type_KIND_LISPALLOC_llvmo__DIFile_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIFile_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_llvmo__DIScope_O:
{
    typedef llvmo::DIScope_O type_KIND_LISPALLOC_llvmo__DIScope_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIScope_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SmallMultimap_O:
{
    typedef core::SmallMultimap_O type_KIND_LISPALLOC_core__SmallMultimap_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SmallMultimap_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Pathname_O:
{
    typedef core::Pathname_O type_KIND_LISPALLOC_core__Pathname_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pathname_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LogicalPathname_O:
{
    typedef core::LogicalPathname_O type_KIND_LISPALLOC_core__LogicalPathname_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LogicalPathname_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__PosixTime_O:
{
    typedef core::PosixTime_O type_KIND_LISPALLOC_core__PosixTime_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__PosixTime_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SmallMap_O:
{
    typedef core::SmallMap_O type_KIND_LISPALLOC_core__SmallMap_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SmallMap_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__Cache:
{
    typedef core::Cache type_KIND_CLASSALLOC_core__Cache;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__Cache)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
    typedef core::Lisp_O type_KIND_ROOTCLASSALLOC_core__Lisp_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_core__Lisp_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
  {
    gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
  {
    gctools::GCVector_moveable<core::KeywordArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::KeywordArgument>*>(client);
    typedef typename gctools::GCVector_moveable<core::KeywordArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
  {
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure:
{
    typedef core::SingleDispatchGenericFunctionClosure type_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
  {
    gctools::GCString_moveable<char>* obj_gc_safe = reinterpret_cast<gctools::GCString_moveable<char>*>(client);
    typedef typename gctools::GCString_moveable<char> type_KIND_GCSTRING_gctools__GCString_moveable_char_;
    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_KIND_GCSTRING_gctools__GCString_moveable_char_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + Align(header_and_gcstring_size);
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
  {
    gctools::GCVector_moveable<core::RequiredArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::RequiredArgument>*>(client);
    typedef typename gctools::GCVector_moveable<core::RequiredArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_llvmo__CompiledClosure:
{
    typedef llvmo::CompiledClosure type_KIND_CLASSALLOC_llvmo__CompiledClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_llvmo__CompiledClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor:
{
    typedef asttooling::internal::VariadicOperatorMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
  {
    gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>*>(client);
    typedef typename gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>> type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor:
{
    typedef asttooling::internal::OverloadedMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
  {
    gctools::GCVector_moveable<core::SymbolStorage>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolStorage>*>(client);
    typedef typename gctools::GCVector_moveable<core::SymbolStorage> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
  {
    gctools::GCVector_moveable<asttooling::ContextFrame>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ContextFrame>*>(client);
    typedef typename gctools::GCVector_moveable<asttooling::ContextFrame> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor:
{
    typedef asttooling::internal::FixedArgCountMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
  {
    gctools::GCVector_moveable<core::T_O *>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::T_O *>*>(client);
    typedef typename gctools::GCVector_moveable<core::T_O *> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor:
{
    typedef asttooling::internal::FreeFuncMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__MacroClosure:
{
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__ConsStepper:
{
    typedef core::ConsStepper type_KIND_CLASSALLOC_core__ConsStepper;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__ConsStepper)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
  {
    gctools::GCVector_moveable<core::AuxArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::AuxArgument>*>(client);
    typedef typename gctools::GCVector_moveable<core::AuxArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
  {
    gctools::GCVector_moveable<asttooling::ParserValue>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ParserValue>*>(client);
    typedef typename gctools::GCVector_moveable<asttooling::ParserValue> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
  {
    gctools::GCVector_moveable<core::SymbolClassPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolClassPair>*>(client);
    typedef typename gctools::GCVector_moveable<core::SymbolClassPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
  {
    gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>*>(client);
    typedef typename gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
  {
    gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>*>(client);
    typedef typename gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>> type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
  {
    gctools::GCVector_moveable<core::CacheRecord>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::CacheRecord>*>(client);
    typedef typename gctools::GCVector_moveable<core::CacheRecord> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__InstanceClosure:
{
    typedef core::InstanceClosure type_KIND_CLASSALLOC_core__InstanceClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__InstanceClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    typedef asttooling::DerivableFrontendActionFactory type_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
  {
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>*>(client);
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    typedef asttooling::DerivableMatchCallback type_KIND_LISPALLOC_asttooling__DerivableMatchCallback;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableMatchCallback)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
  {
    gctools::GCVector_moveable<asttooling::ErrorContent>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ErrorContent>*>(client);
    typedef typename gctools::GCVector_moveable<asttooling::ErrorContent> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    typedef asttooling::DerivableASTFrontendAction type_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
  {
    gctools::GCVector_moveable<asttooling::Message>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::Message>*>(client);
    typedef typename gctools::GCVector_moveable<asttooling::Message> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__:
{
  {
    gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
  {
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>*>(client);
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__CoreExposer:
{
    typedef core::CoreExposer type_KIND_CLASSALLOC_core__CoreExposer;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__CoreExposer)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    typedef asttooling::DerivableSyntaxOnlyAction type_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
  {
    gctools::GCVector_moveable<core::OptionalArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::OptionalArgument>*>(client);
    typedef typename gctools::GCVector_moveable<core::OptionalArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
  {
    gctools::GCVector_moveable<core::ExceptionEntry>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::ExceptionEntry>*>(client);
    typedef typename gctools::GCVector_moveable<core::ExceptionEntry> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__InterpretedClosure:
{
    typedef core::InterpretedClosure type_KIND_CLASSALLOC_core__InterpretedClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__InterpretedClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
  {
    gctools::GCVector_moveable<core::DynamicBinding>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::DynamicBinding>*>(client);
    typedef typename gctools::GCVector_moveable<core::DynamicBinding> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__VectorStepper:
{
    typedef core::VectorStepper type_KIND_CLASSALLOC_core__VectorStepper;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__VectorStepper)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
  {
    gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  }
    goto DONE; //return client;
}
#endif // defined(GC_OBJ_SKIP)
#if defined(GC_OBJ_SKIP_HELPERS)

#endif // defined(GC_OBJ_SKIP_HELPERS)
#if defined(GC_OBJ_SKIP_TABLE)
static void* OBJ_SKIP_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_skip_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps
  /* 5 */ , &&obj_skip_KIND_ROOTCLASSALLOC_clbind__detail__class_map
  /* 6 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 7 */ , &&obj_skip_KIND_CLASSALLOC_clbind__DummyCreator
  /* 8 */ , &&obj_skip_KIND_CLASSALLOC_core__InstanceCreator
  /* 9 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator
  /* 10 */ , &&obj_skip_KIND_BOOTSTRAP_core__T_O
  /* 11 */ , &&obj_skip_KIND_LISPALLOC_core__MultiStringBuffer_O
  /* 12 */ , &&obj_skip_KIND_LISPALLOC_core__ReadTable_O
  /* 13 */ , &&obj_skip_KIND_LISPALLOC_core__Number_O
  /* 14 */ , &&obj_skip_KIND_LISPALLOC_core__Complex_O
  /* 15 */ , &&obj_skip_KIND_LISPALLOC_core__Real_O
  /* 16 */ , &&obj_skip_KIND_LISPALLOC_core__Rational_O
  /* 17 */ , &&obj_skip_KIND_LISPALLOC_core__Integer_O
  /* 18 */ , &&obj_skip_KIND_LISPALLOC_core__Bignum_O
  /* 19 */ , &&obj_skip_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 20 */ , &&obj_skip_KIND_LISPALLOC_core__Ratio_O
  /* 21 */ , &&obj_skip_KIND_LISPALLOC_core__Float_O
  /* 22 */ , &&obj_skip_KIND_LISPALLOC_core__DoubleFloat_O
  /* 23 */ , &&obj_skip_KIND_LISPALLOC_core__LongFloat_O
  /* 24 */ , &&obj_skip_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 25 */ , &&obj_skip_KIND_LISPALLOC_core__ShortFloat_O
  /* 26 */ , &&obj_skip_KIND_LISPALLOC_core__FileStatus_O
  /* 27 */ , &&obj_skip_KIND_LISPALLOC_core__WeakHashTable_O
  /* 28 */ , &&obj_skip_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 29 */ , &&obj_skip_KIND_LISPALLOC_core__Environment_O
  /* 30 */ , &&obj_skip_KIND_LISPALLOC_core__ActivationFrame_O
  /* 31 */ , &&obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 32 */ , &&obj_skip_KIND_LISPALLOC_core__ValueFrame_O
  /* 33 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionFrame_O
  /* 34 */ , &&obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 35 */ , &&obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 36 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 37 */ , &&obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 38 */ , &&obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 39 */ , &&obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 40 */ , &&obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 41 */ , &&obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 42 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 43 */ , &&obj_skip_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 44 */ , &&obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 45 */ , &&obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 46 */ , &&obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 47 */ , &&obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 48 */ , &&obj_skip_KIND_LISPALLOC_core__Array_O
  /* 49 */ , &&obj_skip_KIND_LISPALLOC_core__ArrayObjects_O
  /* 50 */ , &&obj_skip_KIND_LISPALLOC_core__ArrayDisplaced_O
  /* 51 */ , &&obj_skip_KIND_LISPALLOC_core__Vector_O
  /* 52 */ , &&obj_skip_KIND_LISPALLOC_core__BitVector_O
  /* 53 */ , &&obj_skip_KIND_LISPALLOC_core__SimpleBitVector_O
  /* 54 */ , &&obj_skip_KIND_LISPALLOC_core__BitVectorWithFillPtr_O
  /* 55 */ , &&obj_skip_KIND_LISPALLOC_core__VectorDisplaced_O
  /* 56 */ , &&obj_skip_KIND_LISPALLOC_core__String_O
  /* 57 */ , &&obj_skip_KIND_BOOTSTRAP_core__Str_O
  /* 58 */ , &&obj_skip_KIND_LISPALLOC_core__StrWithFillPtr_O
  /* 59 */ , &&obj_skip_KIND_LISPALLOC_core__VectorObjects_O
  /* 60 */ , &&obj_skip_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 61 */ , &&obj_skip_KIND_LISPALLOC_core__SingleDispatchMethod_O
  /* 62 */ , &&obj_skip_KIND_LISPALLOC_core__RandomState_O
  /* 63 */ , &&obj_skip_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O
  /* 64 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DebugLoc_O
  /* 65 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Attribute_O
  /* 66 */ , &&obj_skip_KIND_LISPALLOC_core__RegexMatch_O
  /* 67 */ , &&obj_skip_KIND_LISPALLOC_core__WeakPointer_O
  /* 68 */ , &&obj_skip_KIND_LISPALLOC_core__VaList_dummy_O
  /* 69 */ , &&obj_skip_KIND_BOOTSTRAP_core__StandardObject_O
  /* 70 */ , &&obj_skip_KIND_BOOTSTRAP_core__Metaobject_O
  /* 71 */ , &&obj_skip_KIND_BOOTSTRAP_core__Specializer_O
  /* 72 */ , &&obj_skip_KIND_BOOTSTRAP_core__Class_O
  /* 73 */ , &&obj_skip_KIND_BOOTSTRAP_core__StdClass_O
  /* 74 */ , &&obj_skip_KIND_BOOTSTRAP_core__StandardClass_O
  /* 75 */ , &&obj_skip_KIND_LISPALLOC_core__FuncallableStandardClass_O
  /* 76 */ , &&obj_skip_KIND_BOOTSTRAP_core__StructureClass_O
  /* 77 */ , &&obj_skip_KIND_LISPALLOC_core__ForwardReferencedClass_O
  /* 78 */ , &&obj_skip_KIND_LISPALLOC_core__CxxClass_O
  /* 79 */ , &&obj_skip_KIND_BOOTSTRAP_core__BuiltInClass_O
  /* 80 */ , &&obj_skip_KIND_LISPALLOC_clbind__ClassRep_O
  /* 81 */ , &&obj_skip_KIND_LISPALLOC_core__ExternalObject_O
  /* 82 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Value_O
  /* 83 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Argument_O
  /* 84 */ , &&obj_skip_KIND_LISPALLOC_llvmo__User_O
  /* 85 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Instruction_O
  /* 86 */ , &&obj_skip_KIND_LISPALLOC_llvmo__AtomicRMWInst_O
  /* 87 */ , &&obj_skip_KIND_LISPALLOC_llvmo__LandingPadInst_O
  /* 88 */ , &&obj_skip_KIND_LISPALLOC_llvmo__PHINode_O
  /* 89 */ , &&obj_skip_KIND_LISPALLOC_llvmo__CallInst_O
  /* 90 */ , &&obj_skip_KIND_LISPALLOC_llvmo__StoreInst_O
  /* 91 */ , &&obj_skip_KIND_LISPALLOC_llvmo__UnaryInstruction_O
  /* 92 */ , &&obj_skip_KIND_LISPALLOC_llvmo__LoadInst_O
  /* 93 */ , &&obj_skip_KIND_LISPALLOC_llvmo__AllocaInst_O
  /* 94 */ , &&obj_skip_KIND_LISPALLOC_llvmo__VAArgInst_O
  /* 95 */ , &&obj_skip_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O
  /* 96 */ , &&obj_skip_KIND_LISPALLOC_llvmo__TerminatorInst_O
  /* 97 */ , &&obj_skip_KIND_LISPALLOC_llvmo__UnreachableInst_O
  /* 98 */ , &&obj_skip_KIND_LISPALLOC_llvmo__SwitchInst_O
  /* 99 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ReturnInst_O
  /* 100 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ResumeInst_O
  /* 101 */ , &&obj_skip_KIND_LISPALLOC_llvmo__BranchInst_O
  /* 102 */ , &&obj_skip_KIND_LISPALLOC_llvmo__InvokeInst_O
  /* 103 */ , &&obj_skip_KIND_LISPALLOC_llvmo__IndirectBrInst_O
  /* 104 */ , &&obj_skip_KIND_LISPALLOC_llvmo__FenceInst_O
  /* 105 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Constant_O
  /* 106 */ , &&obj_skip_KIND_LISPALLOC_llvmo__BlockAddress_O
  /* 107 */ , &&obj_skip_KIND_LISPALLOC_llvmo__GlobalValue_O
  /* 108 */ , &&obj_skip_KIND_LISPALLOC_llvmo__GlobalVariable_O
  /* 109 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Function_O
  /* 110 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantArray_O
  /* 111 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantInt_O
  /* 112 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantDataSequential_O
  /* 113 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantDataArray_O
  /* 114 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantStruct_O
  /* 115 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantFP_O
  /* 116 */ , &&obj_skip_KIND_LISPALLOC_llvmo__UndefValue_O
  /* 117 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantPointerNull_O
  /* 118 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ConstantExpr_O
  /* 119 */ , &&obj_skip_KIND_LISPALLOC_llvmo__BasicBlock_O
  /* 120 */ , &&obj_skip_KIND_LISPALLOC_llvmo__IRBuilderBase_O
  /* 121 */ , &&obj_skip_KIND_LISPALLOC_llvmo__IRBuilder_O
  /* 122 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIBuilder_O
  /* 123 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Metadata_O
  /* 124 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ValueAsMetadata_O
  /* 125 */ , &&obj_skip_KIND_LISPALLOC_llvmo__MDNode_O
  /* 126 */ , &&obj_skip_KIND_LISPALLOC_llvmo__MDString_O
  /* 127 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ExecutionEngine_O
  /* 128 */ , &&obj_skip_KIND_LISPALLOC_llvmo__APFloat_O
  /* 129 */ , &&obj_skip_KIND_LISPALLOC_llvmo__PassManagerBuilder_O
  /* 130 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DataLayout_O
  /* 131 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Triple_O
  /* 132 */ , &&obj_skip_KIND_LISPALLOC_llvmo__APInt_O
  /* 133 */ , &&obj_skip_KIND_LISPALLOC_llvmo__PassManagerBase_O
  /* 134 */ , &&obj_skip_KIND_LISPALLOC_llvmo__FunctionPassManager_O
  /* 135 */ , &&obj_skip_KIND_LISPALLOC_llvmo__PassManager_O
  /* 136 */ , &&obj_skip_KIND_LISPALLOC_llvmo__TargetMachine_O
  /* 137 */ , &&obj_skip_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O
  /* 138 */ , &&obj_skip_KIND_LISPALLOC_llvmo__TargetOptions_O
  /* 139 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Type_O
  /* 140 */ , &&obj_skip_KIND_LISPALLOC_llvmo__IntegerType_O
  /* 141 */ , &&obj_skip_KIND_LISPALLOC_llvmo__CompositeType_O
  /* 142 */ , &&obj_skip_KIND_LISPALLOC_llvmo__SequentialType_O
  /* 143 */ , &&obj_skip_KIND_LISPALLOC_llvmo__VectorType_O
  /* 144 */ , &&obj_skip_KIND_LISPALLOC_llvmo__PointerType_O
  /* 145 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ArrayType_O
  /* 146 */ , &&obj_skip_KIND_LISPALLOC_llvmo__StructType_O
  /* 147 */ , &&obj_skip_KIND_LISPALLOC_llvmo__FunctionType_O
  /* 148 */ , &&obj_skip_KIND_LISPALLOC_llvmo__NamedMDNode_O
  /* 149 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Linker_O
  /* 150 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Pass_O
  /* 151 */ , &&obj_skip_KIND_LISPALLOC_llvmo__FunctionPass_O
  /* 152 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ModulePass_O
  /* 153 */ , &&obj_skip_KIND_LISPALLOC_llvmo__ImmutablePass_O
  /* 154 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DataLayoutPass_O
  /* 155 */ , &&obj_skip_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O
  /* 156 */ , &&obj_skip_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O
  /* 157 */ , &&obj_skip_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O
  /* 158 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Module_O
  /* 159 */ , &&obj_skip_KIND_LISPALLOC_llvmo__EngineBuilder_O
  /* 160 */ , &&obj_skip_KIND_LISPALLOC_core__ForeignData_O
  /* 161 */ , &&obj_skip_KIND_LISPALLOC_llvmo__LLVMContext_O
  /* 162 */ , &&obj_skip_KIND_LISPALLOC_llvmo__Target_O
  /* 163 */ , &&obj_skip_KIND_LISPALLOC_core__LoadTimeValues_O
  /* 164 */ , &&obj_skip_KIND_LISPALLOC_core__Binder_O
  /* 165 */ , &&obj_skip_KIND_LISPALLOC_core__IntArray_O
  /* 166 */ , &&obj_skip_KIND_LISPALLOC_core__SourceManager_O
  /* 167 */ , &&obj_skip_KIND_LISPALLOC_core__Record_O
  /* 168 */ , &&obj_skip_KIND_LISPALLOC_core__LightUserData_O
  /* 169 */ , &&obj_skip_KIND_LISPALLOC_core__UserData_O
  /* 170 */ , &&obj_skip_KIND_BOOTSTRAP_core__Symbol_O
  /* 171 */ , &&obj_skip_KIND_LISPALLOC_core__Null_O
  /* 172 */ , &&obj_skip_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 173 */ , &&obj_skip_KIND_TEMPLATED_LISPALLOC_core__Iterator_O
  /* 174 */ , &&obj_skip_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 175 */ , &&obj_skip_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 176 */ , &&obj_skip_KIND_LISPALLOC_core__Regex_O
  /* 177 */ , &&obj_skip_KIND_LISPALLOC_core__PosixTimeDuration_O
  /* 178 */ , &&obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 179 */ , &&obj_skip_KIND_LISPALLOC_core__CandoException_O
  /* 180 */ , &&obj_skip_KIND_LISPALLOC_core__Stream_O
  /* 181 */ , &&obj_skip_KIND_LISPALLOC_core__AnsiStream_O
  /* 182 */ , &&obj_skip_KIND_LISPALLOC_core__FileStream_O
  /* 183 */ , &&obj_skip_KIND_LISPALLOC_core__IOStreamStream_O
  /* 184 */ , &&obj_skip_KIND_LISPALLOC_core__IOFileStream_O
  /* 185 */ , &&obj_skip_KIND_LISPALLOC_core__ConcatenatedStream_O
  /* 186 */ , &&obj_skip_KIND_LISPALLOC_core__StringStream_O
  /* 187 */ , &&obj_skip_KIND_LISPALLOC_core__StringInputStream_O
  /* 188 */ , &&obj_skip_KIND_LISPALLOC_core__StringOutputStream_O
  /* 189 */ , &&obj_skip_KIND_LISPALLOC_core__SynonymStream_O
  /* 190 */ , &&obj_skip_KIND_LISPALLOC_core__EchoStream_O
  /* 191 */ , &&obj_skip_KIND_LISPALLOC_core__TwoWayStream_O
  /* 192 */ , &&obj_skip_KIND_LISPALLOC_core__BroadcastStream_O
  /* 193 */ , &&obj_skip_KIND_LISPALLOC_core__Reader_O
  /* 194 */ , &&obj_skip_KIND_LISPALLOC_core__Cons_O
  /* 195 */ , &&obj_skip_KIND_LISPALLOC_core__Archive_O
  /* 196 */ , &&obj_skip_KIND_LISPALLOC_core__SaveArchive_O
  /* 197 */ , &&obj_skip_KIND_LISPALLOC_core__SexpSaveArchive_O
  /* 198 */ , &&obj_skip_KIND_LISPALLOC_core__LoadArchive_O
  /* 199 */ , &&obj_skip_KIND_LISPALLOC_core__SexpLoadArchive_O
  /* 200 */ , &&obj_skip_KIND_LISPALLOC_core__HashTable_O
  /* 201 */ , &&obj_skip_KIND_LISPALLOC_core__HashTableEq_O
  /* 202 */ , &&obj_skip_KIND_LISPALLOC_core__HashTableEqualp_O
  /* 203 */ , &&obj_skip_KIND_LISPALLOC_core__HashTableEql_O
  /* 204 */ , &&obj_skip_KIND_LISPALLOC_core__HashTableEqual_O
  /* 205 */ , &&obj_skip_KIND_LISPALLOC_cffi__Pointer_O
  /* 206 */ , &&obj_skip_KIND_LISPALLOC_core__CxxObject_O
  /* 207 */ , &&obj_skip_KIND_LISPALLOC_core__WeakKeyMapping_O
  /* 208 */ , &&obj_skip_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 209 */ , &&obj_skip_KIND_LISPALLOC_llvmo__InsertPoint_O
  /* 210 */ , &&obj_skip_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 211 */ , &&obj_skip_KIND_LISPALLOC_core__SNode_O
  /* 212 */ , &&obj_skip_KIND_LISPALLOC_core__LeafSNode_O
  /* 213 */ , &&obj_skip_KIND_LISPALLOC_core__BranchSNode_O
  /* 214 */ , &&obj_skip_KIND_LISPALLOC_core__Path_O
  /* 215 */ , &&obj_skip_KIND_LISPALLOC_asttooling__AstVisitor_O
  /* 216 */ , &&obj_skip_KIND_LISPALLOC_llvmo__AttributeSet_O
  /* 217 */ , &&obj_skip_KIND_LISPALLOC_core__StructureObject_O
  /* 218 */ , &&obj_skip_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 219 */ , &&obj_skip_KIND_LISPALLOC_core__Package_O
  /* 220 */ , &&obj_skip_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 221 */ , &&obj_skip_KIND_LISPALLOC_core__Character_dummy_O
  /* 222 */ , &&obj_skip_KIND_LISPALLOC_core__Function_O
  /* 223 */ , &&obj_skip_KIND_LISPALLOC_core__CompiledFunction_O
  /* 224 */ , &&obj_skip_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O
  /* 225 */ , &&obj_skip_KIND_LISPALLOC_core__SpecialForm_O
  /* 226 */ , &&obj_skip_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O
  /* 227 */ , &&obj_skip_KIND_LISPALLOC_core__Instance_O
  /* 228 */ , &&obj_skip_KIND_LISPALLOC_core__Pointer_O
  /* 229 */ , &&obj_skip_KIND_LISPALLOC_clbind__ClassRegistry_O
  /* 230 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DebugInfo_O
  /* 231 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIDerivedType_O
  /* 232 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIArray_O
  /* 233 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIBasicType_O
  /* 234 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DISubprogram_O
  /* 235 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DILexicalBlock_O
  /* 236 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DICompileUnit_O
  /* 237 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIDescriptor_O
  /* 238 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIType_O
  /* 239 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DISubroutineType_O
  /* 240 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DICompositeType_O
  /* 241 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DITypeArray_O
  /* 242 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIFile_O
  /* 243 */ , &&obj_skip_KIND_LISPALLOC_llvmo__DIScope_O
  /* 244 */ , &&obj_skip_KIND_LISPALLOC_core__SmallMultimap_O
  /* 245 */ , &&obj_skip_KIND_LISPALLOC_core__Pathname_O
  /* 246 */ , &&obj_skip_KIND_LISPALLOC_core__LogicalPathname_O
  /* 247 */ , &&obj_skip_KIND_LISPALLOC_core__PosixTime_O
  /* 248 */ , &&obj_skip_KIND_LISPALLOC_core__SmallMap_O
  /* 249 */ , &&obj_skip_KIND_CLASSALLOC_core__Cache
  /* 250 */ , &&obj_skip_KIND_ROOTCLASSALLOC_core__Lisp_O
  /* 251 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 252 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 253 */ , &&obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 254 */ , &&obj_skip_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure
  /* 255 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 256 */ , &&obj_skip_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 257 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 258 */ , &&obj_skip_KIND_CLASSALLOC_llvmo__CompiledClosure
  /* 259 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 260 */ , &&obj_skip_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor
  /* 261 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___
  /* 262 */ , &&obj_skip_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor
  /* 263 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_
  /* 264 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_
  /* 265 */ , &&obj_skip_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor
  /* 266 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_
  /* 267 */ , &&obj_skip_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor
  /* 268 */ , &&obj_skip_KIND_CLASSALLOC_core__MacroClosure
  /* 269 */ , &&obj_skip_KIND_CLASSALLOC_core__ConsStepper
  /* 270 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 271 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_
  /* 272 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 273 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 274 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_
  /* 275 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___
  /* 276 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 277 */ , &&obj_skip_KIND_CLASSALLOC_core__InstanceClosure
  /* 278 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 279 */ , &&obj_skip_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory
  /* 280 */ , &&obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_
  /* 281 */ , &&obj_skip_KIND_LISPALLOC_asttooling__DerivableMatchCallback
  /* 282 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_
  /* 283 */ , &&obj_skip_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction
  /* 284 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_
  /* 285 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__
  /* 286 */ , &&obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_
  /* 287 */ , &&obj_skip_KIND_CLASSALLOC_core__CoreExposer
  /* 288 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 289 */ , &&obj_skip_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction
  /* 290 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__
  /* 291 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 292 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 293 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 294 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 295 */ , &&obj_skip_KIND_CLASSALLOC_core__InterpretedClosure
  /* 296 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 297 */ , &&obj_skip_KIND_CLASSALLOC_core__VectorStepper
  /* 298 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__
};
#endif // defined(GC_OBJ_SKIP_TABLE)
#if defined(GC_OBJ_SCAN)
obj_scan_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::RegMap::RegistryMaps>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
  mps_res_t result = gctools::obj_scan_helper<clbind::detail::class_map>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_clbind__DummyCreator:
{
  mps_res_t result = gctools::obj_scan_helper<clbind::DummyCreator>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__InstanceCreator:
{
  mps_res_t result = gctools::obj_scan_helper<core::InstanceCreator>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    SMART_PTR_FIX(obj_gc_safe->_mostDerivedClassSymbol);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__T_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::T_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::MultiStringBuffer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ReadTable_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ReadTable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Number_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Number_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Complex_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Complex_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Real_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Real_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Rational_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Rational_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Integer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Integer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Bignum_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Bignum_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Fixnum_dummy_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Ratio_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Ratio_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Float_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Float_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DoubleFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::DoubleFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LongFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LongFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SingleFloat_dummy_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ShortFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ShortFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FileStatus_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FileStatus_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__WeakHashTable_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::WeakHashTable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::WeakKeyHashTable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Environment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Environment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ActivationFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ActivationFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__TagbodyFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::TagbodyFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ValueFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ValueFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FunctionFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LexicalEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::RuntimeVisibleEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FunctionValueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ValueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::TagbodyEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CompileTimeEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::UnwindProtectEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SymbolMacroletEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FunctionContainerEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StackValueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__BlockEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::BlockEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::MacroletEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CatchEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CatchEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__GlueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::GlueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Array_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Array_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ArrayObjects_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ArrayObjects_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ArrayDisplaced_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Vector_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Vector_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__BitVector_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::BitVector_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SimpleBitVector_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SimpleBitVector_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::BitVectorWithFillPtr_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__VectorDisplaced_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::VectorDisplaced_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__String_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::String_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Str_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Str_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StrWithFillPtr_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__VectorObjects_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::VectorObjects_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::VectorObjectsWithFillPtr_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SingleDispatchMethod_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__RandomState_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::RandomState_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DebugLoc_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Attribute_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Attribute_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__RegexMatch_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::RegexMatch_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__WeakPointer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::WeakPointer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__VaList_dummy_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::VaList_dummy_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__StandardObject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StandardObject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Metaobject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Metaobject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Specializer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Specializer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Class_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Class_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__StdClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StdClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__StandardClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StandardClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FuncallableStandardClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__StructureClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StructureClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ForwardReferencedClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CxxClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CxxClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::BuiltInClass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_clbind__ClassRep_O:
{
  mps_res_t result = gctools::obj_scan_helper<clbind::ClassRep_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ExternalObject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ExternalObject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Value_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Value_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Argument_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Argument_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__User_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::User_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Instruction_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Instruction_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::AtomicRMWInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::LandingPadInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__PHINode_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::PHINode_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__CallInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::CallInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__StoreInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::StoreInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::UnaryInstruction_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__LoadInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::LoadInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::AllocaInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::VAArgInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::AtomicCmpXchgInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::TerminatorInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::UnreachableInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::SwitchInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ReturnInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ResumeInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__BranchInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::BranchInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::InvokeInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::IndirectBrInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__FenceInst_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::FenceInst_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Constant_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Constant_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::BlockAddress_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::GlobalValue_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::GlobalVariable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Function_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Function_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantArray_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantInt_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantDataSequential_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantDataArray_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantStruct_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantFP_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__UndefValue_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::UndefValue_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantPointerNull_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ConstantExpr_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::BasicBlock_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::IRBuilderBase_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::IRBuilder_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIBuilder_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Metadata_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Metadata_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ValueAsMetadata_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__MDNode_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::MDNode_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__MDString_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::MDString_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ExecutionEngine_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__APFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::APFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::PassManagerBuilder_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DataLayout_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DataLayout_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Triple_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Triple_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__APInt_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::APInt_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::PassManagerBase_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::FunctionPassManager_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__PassManager_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::PassManager_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::TargetMachine_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::LLVMTargetMachine_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::TargetOptions_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Type_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Type_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__IntegerType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::IntegerType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__CompositeType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::CompositeType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__SequentialType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::SequentialType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__VectorType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::VectorType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__PointerType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::PointerType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ArrayType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ArrayType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__StructType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::StructType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__FunctionType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::FunctionType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::NamedMDNode_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Linker_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Linker_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Pass_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Pass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::FunctionPass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ModulePass_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ModulePass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::ImmutablePass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DataLayoutPass_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::TargetLibraryInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::MCSubtargetInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::TargetSubtargetInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Module_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Module_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::EngineBuilder_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ForeignData_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ForeignData_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::LLVMContext_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__Target_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::Target_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LoadTimeValues_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LoadTimeValues_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Binder_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Binder_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__IntArray_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::IntArray_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SourceManager_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SourceManager_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Record_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Record_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LightUserData_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LightUserData_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__UserData_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::UserData_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Symbol_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Symbol_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Null_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Null_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SourcePosInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SourcePosInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DirectoryIterator_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::DirectoryIterator_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::RecursiveDirectoryIterator_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Regex_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Regex_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::PosixTimeDuration_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SymbolToEnumConverter_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CandoException_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CandoException_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Stream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Stream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__AnsiStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::AnsiStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FileStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FileStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__IOStreamStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::IOStreamStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__IOFileStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::IOFileStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ConcatenatedStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__StringStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StringStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__StringInputStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StringInputStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__StringOutputStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StringOutputStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SynonymStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SynonymStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__EchoStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::EchoStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__TwoWayStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::TwoWayStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__BroadcastStream_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::BroadcastStream_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Reader_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Reader_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Cons_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Cons_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Archive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Archive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SaveArchive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SaveArchive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SexpSaveArchive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LoadArchive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LoadArchive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SexpLoadArchive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__HashTable_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::HashTable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__HashTableEq_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::HashTableEq_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__HashTableEqualp_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::HashTableEqualp_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__HashTableEql_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::HashTableEql_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__HashTableEqual_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::HashTableEqual_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_cffi__Pointer_O:
{
  mps_res_t result = gctools::obj_scan_helper<cffi::Pointer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CxxObject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CxxObject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::WeakKeyMapping_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LambdaListHandler_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LambdaListHandler_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::InsertPoint_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SourceFileInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SourceFileInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SNode_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SNode_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LeafSNode_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LeafSNode_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__BranchSNode_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::BranchSNode_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Path_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Path_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::AstVisitor_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::AttributeSet_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__StructureObject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StructureObject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::InvocationHistoryFrameIterator_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Package_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Package_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DirectoryEntry_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::DirectoryEntry_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Character_dummy_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Character_dummy_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Function_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Function_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CompiledFunction_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CompiledFunction_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SingleDispatchGenericFunction_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SpecialForm_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SpecialForm_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SingleDispatchEffectiveMethodFunction_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Instance_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Instance_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Pointer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Pointer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
  mps_res_t result = gctools::obj_scan_helper<clbind::ClassRegistry_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DebugInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIDerivedType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIArray_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIArray_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIBasicType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DISubprogram_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DILexicalBlock_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DICompileUnit_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIDescriptor_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DISubroutineType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DICompositeType_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DITypeArray_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIFile_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIFile_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_llvmo__DIScope_O:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::DIScope_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SmallMultimap_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SmallMultimap_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Pathname_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Pathname_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LogicalPathname_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LogicalPathname_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__PosixTime_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::PosixTime_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SmallMap_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SmallMap_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__Cache:
{
  mps_res_t result = gctools::obj_scan_helper<core::Cache>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Lisp_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
    gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>*>(client);
    for (gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          TAGGED_POINTER_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    gctools::GCVector_moveable<core::KeywordArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::KeywordArgument>*>(client);
    for (gctools::GCVector_moveable<core::KeywordArgument>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_ArgTarget);
    SMART_PTR_FIX(it->_Default);
    SMART_PTR_FIX(it->_Keyword);
    SMART_PTR_FIX(it->_Sensor._ArgTarget);
    }
    typedef typename gctools::GCVector_moveable<core::KeywordArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    for (gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure:
{
  mps_res_t result = gctools::obj_scan_helper<core::SingleDispatchGenericFunctionClosure>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    // Should never be invoked
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    gctools::GCVector_moveable<core::RequiredArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::RequiredArgument>*>(client);
    for (gctools::GCVector_moveable<core::RequiredArgument>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_ArgTarget);
    }
    typedef typename gctools::GCVector_moveable<core::RequiredArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_llvmo__CompiledClosure:
{
  mps_res_t result = gctools::obj_scan_helper<llvmo::CompiledClosure>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::internal::VariadicOperatorMatcherDescriptor>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
    gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>*>(client);
    for (gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->first);
    SMART_PTR_FIX(it->second);
    }
    typedef typename gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>> type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::internal::OverloadedMatcherDescriptor>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
    gctools::GCVector_moveable<core::SymbolStorage>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolStorage>*>(client);
    for (gctools::GCVector_moveable<core::SymbolStorage>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_Symbol);
    }
    typedef typename gctools::GCVector_moveable<core::SymbolStorage> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    gctools::GCVector_moveable<asttooling::ContextFrame>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ContextFrame>*>(client);
    for (gctools::GCVector_moveable<asttooling::ContextFrame>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->Range);
    }
    typedef typename gctools::GCVector_moveable<asttooling::ContextFrame> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::internal::FixedArgCountMatcherDescriptor>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
    gctools::GCVector_moveable<core::T_O *>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::T_O *>*>(client);
    for (gctools::GCVector_moveable<core::T_O *>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SIMPLE_POINTER_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<core::T_O *> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::internal::FreeFuncMatcherDescriptor>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__MacroClosure:
{
  mps_res_t result = gctools::obj_scan_helper<core::MacroClosure>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__ConsStepper:
{
  mps_res_t result = gctools::obj_scan_helper<core::ConsStepper>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    gctools::GCVector_moveable<core::AuxArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::AuxArgument>*>(client);
    for (gctools::GCVector_moveable<core::AuxArgument>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_ArgTarget);
    SMART_PTR_FIX(it->_Expression);
    }
    typedef typename gctools::GCVector_moveable<core::AuxArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
    gctools::GCVector_moveable<asttooling::ParserValue>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ParserValue>*>(client);
    for (gctools::GCVector_moveable<asttooling::ParserValue>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->Range);
    }
    typedef typename gctools::GCVector_moveable<asttooling::ParserValue> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    gctools::GCVector_moveable<core::SymbolClassPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolClassPair>*>(client);
    for (gctools::GCVector_moveable<core::SymbolClassPair>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->symbol);
    SMART_PTR_FIX(it->theClass);
    }
    typedef typename gctools::GCVector_moveable<core::SymbolClassPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
    gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>*>(client);
    for (gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->Name);
    TAGGED_POINTER_FIX(it->matcher);
    }
    typedef typename gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
    gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>*>(client);
    for (gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->first);
    SMART_PTR_FIX(it->second);
    }
    typedef typename gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>> type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    gctools::GCVector_moveable<core::CacheRecord>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::CacheRecord>*>(client);
    for (gctools::GCVector_moveable<core::CacheRecord>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_key);
    SMART_PTR_FIX(it->_value);
    }
    typedef typename gctools::GCVector_moveable<core::CacheRecord> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__InstanceClosure:
{
  mps_res_t result = gctools::obj_scan_helper<core::InstanceClosure>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::DerivableFrontendActionFactory>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>*>(client);
    for (gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::DerivableMatchCallback>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    gctools::GCVector_moveable<asttooling::ErrorContent>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::ErrorContent>*>(client);
    for (gctools::GCVector_moveable<asttooling::ErrorContent>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    TAGGED_POINTER_FIX(it->ContextStack._Vector._Contents);
    TAGGED_POINTER_FIX(it->Messages._Vector._Contents);
    }
    typedef typename gctools::GCVector_moveable<asttooling::ErrorContent> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::DerivableASTFrontendAction>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    gctools::GCVector_moveable<asttooling::Message>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<asttooling::Message>*>(client);
    for (gctools::GCVector_moveable<asttooling::Message>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->Range);
    }
    typedef typename gctools::GCVector_moveable<asttooling::Message> type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__:
{
    gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>*>(client);
    for (gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          TAGGED_POINTER_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>*>(client);
    for (gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__CoreExposer:
{
  mps_res_t result = gctools::obj_scan_helper<core::CoreExposer>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
  mps_res_t result = gctools::obj_scan_helper<asttooling::DerivableSyntaxOnlyAction>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    gctools::GCVector_moveable<core::OptionalArgument>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::OptionalArgument>*>(client);
    for (gctools::GCVector_moveable<core::OptionalArgument>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_ArgTarget);
    SMART_PTR_FIX(it->_Default);
    SMART_PTR_FIX(it->_Sensor._ArgTarget);
    }
    typedef typename gctools::GCVector_moveable<core::OptionalArgument> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_cleavir_ast);
    SMART_PTR_FIX(obj_gc_safe->_lambdaListHandler);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    gctools::GCVector_moveable<core::ExceptionEntry>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::ExceptionEntry>*>(client);
    for (gctools::GCVector_moveable<core::ExceptionEntry>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_Key);
    }
    typedef typename gctools::GCVector_moveable<core::ExceptionEntry> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__InterpretedClosure:
{
  mps_res_t result = gctools::obj_scan_helper<core::InterpretedClosure>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    gctools::GCVector_moveable<core::DynamicBinding>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::DynamicBinding>*>(client);
    for (gctools::GCVector_moveable<core::DynamicBinding>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_Var);
    SMART_PTR_FIX(it->_Val);
    }
    typedef typename gctools::GCVector_moveable<core::DynamicBinding> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__VectorStepper:
{
  mps_res_t result = gctools::obj_scan_helper<core::VectorStepper>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
goto TOP;
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::RegMap::RegistryMaps>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::RegMap::RegistryMaps* obj_gc_safe = reinterpret_cast<asttooling::RegMap::RegistryMaps*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->Constructors._Vector._Contents);
    typedef asttooling::RegMap::RegistryMaps type_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps;
    client = (char*)client + AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<clbind::detail::class_map>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    clbind::detail::class_map* obj_gc_safe = reinterpret_cast<clbind::detail::class_map*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->m_classes._Vector._Contents);
    typedef clbind::detail::class_map type_KIND_ROOTCLASSALLOC_clbind__detail__class_map;
    client = (char*)client + AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_clbind__detail__class_map)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<clbind::DummyCreator>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef clbind::DummyCreator type_KIND_CLASSALLOC_clbind__DummyCreator;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_clbind__DummyCreator)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::InstanceCreator>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::InstanceCreator* obj_gc_safe = reinterpret_cast<core::InstanceCreator*>(client);
    SMART_PTR_FIX(obj_gc_safe->_className);
    typedef core::InstanceCreator type_KIND_CLASSALLOC_core__InstanceCreator;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__InstanceCreator)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::T_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::T_O type_KIND_BOOTSTRAP_core__T_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__T_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::MultiStringBuffer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::MultiStringBuffer_O type_KIND_LISPALLOC_core__MultiStringBuffer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MultiStringBuffer_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ReadTable_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ReadTable_O* obj_gc_safe = reinterpret_cast<core::ReadTable_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Case);
    SMART_PTR_FIX(obj_gc_safe->_SyntaxTypes);
    SMART_PTR_FIX(obj_gc_safe->_MacroCharacters);
    SMART_PTR_FIX(obj_gc_safe->_DispatchMacroCharacters);
    typedef core::ReadTable_O type_KIND_LISPALLOC_core__ReadTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ReadTable_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Number_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Complex_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_real);
    SMART_PTR_FIX(obj_gc_safe->_imaginary);
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Real_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Rational_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Integer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Bignum_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Fixnum_dummy_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Ratio_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_numerator);
    SMART_PTR_FIX(obj_gc_safe->_denominator);
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Float_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::DoubleFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LongFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SingleFloat_dummy_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ShortFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::FileStatus_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::FileStatus_O type_KIND_LISPALLOC_core__FileStatus_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStatus_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::WeakHashTable_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::WeakKeyHashTable_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_HashTable._Keys);
    TAGGED_POINTER_FIX(obj_gc_safe->_HashTable._Values);
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Environment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ActivationFrame_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::ActivationFrame_O type_KIND_LISPALLOC_core__ActivationFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ActivationFrame_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::TagbodyFrame_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ValueFrame_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    TAGGED_POINTER_FIX(obj_gc_safe->_Objects._Array._Contents);
    SMART_PTR_FIX(obj_gc_safe->_DebuggingInfo);
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::FunctionFrame_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    TAGGED_POINTER_FIX(obj_gc_safe->_Objects._Array._Contents);
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LexicalEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::RuntimeVisibleEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::FunctionValueEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_FunctionIndices);
    SMART_PTR_FIX(obj_gc_safe->_FunctionFrame);
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ValueEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_SymbolIndex);
    SMART_PTR_FIX(obj_gc_safe->_ActivationFrame);
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::TagbodyEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Tags);
    TAGGED_POINTER_FIX(obj_gc_safe->_TagCode._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_ActivationFrame);
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CompileTimeEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::UnwindProtectEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_CleanupForm);
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SymbolMacroletEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_Macros);
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::FunctionContainerEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StackValueEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_Values);
    typedef core::StackValueEnvironment_O type_KIND_LISPALLOC_core__StackValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StackValueEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::BlockEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_BlockSymbol);
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::MacroletEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_Macros);
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CatchEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::GlueEnvironment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Map);
    SMART_PTR_FIX(obj_gc_safe->_Args);
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Array_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ArrayObjects_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ArrayObjects_O* obj_gc_safe = reinterpret_cast<core::ArrayObjects_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    TAGGED_POINTER_FIX(obj_gc_safe->_Values._Vector._Contents);
    typedef core::ArrayObjects_O type_KIND_LISPALLOC_core__ArrayObjects_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ArrayObjects_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ArrayDisplaced_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ArrayDisplaced_O* obj_gc_safe = reinterpret_cast<core::ArrayDisplaced_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    SMART_PTR_FIX(obj_gc_safe->_Array);
    typedef core::ArrayDisplaced_O type_KIND_LISPALLOC_core__ArrayDisplaced_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ArrayDisplaced_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Vector_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::BitVector_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::BitVector_O type_KIND_LISPALLOC_core__BitVector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BitVector_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SimpleBitVector_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::SimpleBitVector_O type_KIND_LISPALLOC_core__SimpleBitVector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SimpleBitVector_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::BitVectorWithFillPtr_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::BitVectorWithFillPtr_O type_KIND_LISPALLOC_core__BitVectorWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BitVectorWithFillPtr_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::VectorDisplaced_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::VectorDisplaced_O* obj_gc_safe = reinterpret_cast<core::VectorDisplaced_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    SMART_PTR_FIX(obj_gc_safe->_Vector);
    typedef core::VectorDisplaced_O type_KIND_LISPALLOC_core__VectorDisplaced_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorDisplaced_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::String_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Str_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_Contents._Contents);
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StrWithFillPtr_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StrWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::StrWithFillPtr_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_Contents._Contents);
    typedef core::StrWithFillPtr_O type_KIND_LISPALLOC_core__StrWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StrWithFillPtr_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::VectorObjects_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    TAGGED_POINTER_FIX(obj_gc_safe->_Values._Vector._Contents);
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::VectorObjectsWithFillPtr_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    TAGGED_POINTER_FIX(obj_gc_safe->_Values._Vector._Contents);
    typedef core::VectorObjectsWithFillPtr_O type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SingleDispatchMethod_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SingleDispatchMethod_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchMethod_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_name);
    SMART_PTR_FIX(obj_gc_safe->_receiver_class);
    SMART_PTR_FIX(obj_gc_safe->code);
    SMART_PTR_FIX(obj_gc_safe->_argument_handler);
    SMART_PTR_FIX(obj_gc_safe->_declares);
    SMART_PTR_FIX(obj_gc_safe->_docstring);
    typedef core::SingleDispatchMethod_O type_KIND_LISPALLOC_core__SingleDispatchMethod_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchMethod_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::RandomState_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::RandomState_O type_KIND_LISPALLOC_core__RandomState_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RandomState_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DebugLoc_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DebugLoc_O type_KIND_LISPALLOC_llvmo__DebugLoc_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DebugLoc_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Attribute_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::Attribute_O type_KIND_LISPALLOC_llvmo__Attribute_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Attribute_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::RegexMatch_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::RegexMatch_O type_KIND_LISPALLOC_core__RegexMatch_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RegexMatch_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::WeakPointer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::WeakPointer_O* obj_gc_safe = reinterpret_cast<core::WeakPointer_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_WeakObject.pointer);
    typedef core::WeakPointer_O type_KIND_LISPALLOC_core__WeakPointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakPointer_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::VaList_dummy_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::VaList_dummy_O type_KIND_LISPALLOC_core__VaList_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VaList_dummy_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StandardObject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Metaobject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Specializer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Class_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StdClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StdClass_O* obj_gc_safe = reinterpret_cast<core::StdClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    typedef core::StdClass_O type_KIND_BOOTSTRAP_core__StdClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StdClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StandardClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StandardClass_O* obj_gc_safe = reinterpret_cast<core::StandardClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_InstanceCoreClass);
    typedef core::StandardClass_O type_KIND_BOOTSTRAP_core__StandardClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::FuncallableStandardClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::FuncallableStandardClass_O* obj_gc_safe = reinterpret_cast<core::FuncallableStandardClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_InstanceCoreClass);
    typedef core::FuncallableStandardClass_O type_KIND_LISPALLOC_core__FuncallableStandardClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FuncallableStandardClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StructureClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StructureClass_O* obj_gc_safe = reinterpret_cast<core::StructureClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_InstanceCoreClass);
    typedef core::StructureClass_O type_KIND_BOOTSTRAP_core__StructureClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StructureClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ForwardReferencedClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ForwardReferencedClass_O* obj_gc_safe = reinterpret_cast<core::ForwardReferencedClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_InstanceCoreClass);
    typedef core::ForwardReferencedClass_O type_KIND_LISPALLOC_core__ForwardReferencedClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ForwardReferencedClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CxxClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CxxClass_O* obj_gc_safe = reinterpret_cast<core::CxxClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    typedef core::CxxClass_O type_KIND_LISPALLOC_core__CxxClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::BuiltInClass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::BuiltInClass_O* obj_gc_safe = reinterpret_cast<core::BuiltInClass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    typedef core::BuiltInClass_O type_KIND_BOOTSTRAP_core__BuiltInClass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__BuiltInClass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<clbind::ClassRep_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    clbind::ClassRep_O* obj_gc_safe = reinterpret_cast<clbind::ClassRep_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    TAGGED_POINTER_FIX(obj_gc_safe->_theCreator);
    TAGGED_POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->m_bases._Vector._Contents);
    typedef clbind::ClassRep_O type_KIND_LISPALLOC_clbind__ClassRep_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_clbind__ClassRep_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ExternalObject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ExternalObject_O* obj_gc_safe = reinterpret_cast<core::ExternalObject_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef core::ExternalObject_O type_KIND_LISPALLOC_core__ExternalObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ExternalObject_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Value_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Value_O* obj_gc_safe = reinterpret_cast<llvmo::Value_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Value_O type_KIND_LISPALLOC_llvmo__Value_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Value_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Argument_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Argument_O* obj_gc_safe = reinterpret_cast<llvmo::Argument_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Argument_O type_KIND_LISPALLOC_llvmo__Argument_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Argument_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::User_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::User_O* obj_gc_safe = reinterpret_cast<llvmo::User_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::User_O type_KIND_LISPALLOC_llvmo__User_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__User_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Instruction_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Instruction_O* obj_gc_safe = reinterpret_cast<llvmo::Instruction_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Instruction_O type_KIND_LISPALLOC_llvmo__Instruction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Instruction_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::AtomicRMWInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::AtomicRMWInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicRMWInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::AtomicRMWInst_O type_KIND_LISPALLOC_llvmo__AtomicRMWInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AtomicRMWInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::LandingPadInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::LandingPadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LandingPadInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::LandingPadInst_O type_KIND_LISPALLOC_llvmo__LandingPadInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LandingPadInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::PHINode_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::PHINode_O* obj_gc_safe = reinterpret_cast<llvmo::PHINode_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::PHINode_O type_KIND_LISPALLOC_llvmo__PHINode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PHINode_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::CallInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::CallInst_O* obj_gc_safe = reinterpret_cast<llvmo::CallInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::CallInst_O type_KIND_LISPALLOC_llvmo__CallInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__CallInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::StoreInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::StoreInst_O* obj_gc_safe = reinterpret_cast<llvmo::StoreInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::StoreInst_O type_KIND_LISPALLOC_llvmo__StoreInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__StoreInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::UnaryInstruction_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::UnaryInstruction_O* obj_gc_safe = reinterpret_cast<llvmo::UnaryInstruction_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::UnaryInstruction_O type_KIND_LISPALLOC_llvmo__UnaryInstruction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UnaryInstruction_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::LoadInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::LoadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LoadInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::LoadInst_O type_KIND_LISPALLOC_llvmo__LoadInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LoadInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::AllocaInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::AllocaInst_O* obj_gc_safe = reinterpret_cast<llvmo::AllocaInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::AllocaInst_O type_KIND_LISPALLOC_llvmo__AllocaInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AllocaInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::VAArgInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::VAArgInst_O* obj_gc_safe = reinterpret_cast<llvmo::VAArgInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::VAArgInst_O type_KIND_LISPALLOC_llvmo__VAArgInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__VAArgInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::AtomicCmpXchgInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::AtomicCmpXchgInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicCmpXchgInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::AtomicCmpXchgInst_O type_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::TerminatorInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::TerminatorInst_O* obj_gc_safe = reinterpret_cast<llvmo::TerminatorInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::TerminatorInst_O type_KIND_LISPALLOC_llvmo__TerminatorInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TerminatorInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::UnreachableInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::UnreachableInst_O* obj_gc_safe = reinterpret_cast<llvmo::UnreachableInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::UnreachableInst_O type_KIND_LISPALLOC_llvmo__UnreachableInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UnreachableInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::SwitchInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::SwitchInst_O* obj_gc_safe = reinterpret_cast<llvmo::SwitchInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::SwitchInst_O type_KIND_LISPALLOC_llvmo__SwitchInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__SwitchInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ReturnInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ReturnInst_O* obj_gc_safe = reinterpret_cast<llvmo::ReturnInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ReturnInst_O type_KIND_LISPALLOC_llvmo__ReturnInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ReturnInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ResumeInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ResumeInst_O* obj_gc_safe = reinterpret_cast<llvmo::ResumeInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ResumeInst_O type_KIND_LISPALLOC_llvmo__ResumeInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ResumeInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::BranchInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::BranchInst_O* obj_gc_safe = reinterpret_cast<llvmo::BranchInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::BranchInst_O type_KIND_LISPALLOC_llvmo__BranchInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BranchInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::InvokeInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::InvokeInst_O* obj_gc_safe = reinterpret_cast<llvmo::InvokeInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::InvokeInst_O type_KIND_LISPALLOC_llvmo__InvokeInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__InvokeInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::IndirectBrInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::IndirectBrInst_O* obj_gc_safe = reinterpret_cast<llvmo::IndirectBrInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::IndirectBrInst_O type_KIND_LISPALLOC_llvmo__IndirectBrInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IndirectBrInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::FenceInst_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::FenceInst_O* obj_gc_safe = reinterpret_cast<llvmo::FenceInst_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::FenceInst_O type_KIND_LISPALLOC_llvmo__FenceInst_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FenceInst_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Constant_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Constant_O* obj_gc_safe = reinterpret_cast<llvmo::Constant_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Constant_O type_KIND_LISPALLOC_llvmo__Constant_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Constant_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::BlockAddress_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::BlockAddress_O* obj_gc_safe = reinterpret_cast<llvmo::BlockAddress_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::BlockAddress_O type_KIND_LISPALLOC_llvmo__BlockAddress_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BlockAddress_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::GlobalValue_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::GlobalValue_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalValue_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::GlobalValue_O type_KIND_LISPALLOC_llvmo__GlobalValue_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__GlobalValue_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::GlobalVariable_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::GlobalVariable_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalVariable_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::GlobalVariable_O type_KIND_LISPALLOC_llvmo__GlobalVariable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__GlobalVariable_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Function_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Function_O* obj_gc_safe = reinterpret_cast<llvmo::Function_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    SMART_PTR_FIX(obj_gc_safe->_RunTimeValues);
    typedef llvmo::Function_O type_KIND_LISPALLOC_llvmo__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Function_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantArray_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantArray_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantArray_O type_KIND_LISPALLOC_llvmo__ConstantArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantArray_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantInt_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantInt_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantInt_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantInt_O type_KIND_LISPALLOC_llvmo__ConstantInt_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantInt_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantDataSequential_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantDataSequential_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataSequential_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantDataSequential_O type_KIND_LISPALLOC_llvmo__ConstantDataSequential_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantDataSequential_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantDataArray_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantDataArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataArray_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantDataArray_O type_KIND_LISPALLOC_llvmo__ConstantDataArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantDataArray_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantStruct_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantStruct_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantStruct_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantStruct_O type_KIND_LISPALLOC_llvmo__ConstantStruct_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantStruct_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantFP_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantFP_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantFP_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantFP_O type_KIND_LISPALLOC_llvmo__ConstantFP_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantFP_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::UndefValue_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::UndefValue_O* obj_gc_safe = reinterpret_cast<llvmo::UndefValue_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::UndefValue_O type_KIND_LISPALLOC_llvmo__UndefValue_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__UndefValue_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantPointerNull_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantPointerNull_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantPointerNull_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantPointerNull_O type_KIND_LISPALLOC_llvmo__ConstantPointerNull_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantPointerNull_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ConstantExpr_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ConstantExpr_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantExpr_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ConstantExpr_O type_KIND_LISPALLOC_llvmo__ConstantExpr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ConstantExpr_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::BasicBlock_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::BasicBlock_O* obj_gc_safe = reinterpret_cast<llvmo::BasicBlock_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::BasicBlock_O type_KIND_LISPALLOC_llvmo__BasicBlock_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__BasicBlock_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::IRBuilderBase_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::IRBuilderBase_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilderBase_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::IRBuilderBase_O type_KIND_LISPALLOC_llvmo__IRBuilderBase_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IRBuilderBase_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::IRBuilder_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::IRBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilder_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::IRBuilder_O type_KIND_LISPALLOC_llvmo__IRBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IRBuilder_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIBuilder_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::DIBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::DIBuilder_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::DIBuilder_O type_KIND_LISPALLOC_llvmo__DIBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIBuilder_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Metadata_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Metadata_O* obj_gc_safe = reinterpret_cast<llvmo::Metadata_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Metadata_O type_KIND_LISPALLOC_llvmo__Metadata_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Metadata_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ValueAsMetadata_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ValueAsMetadata_O* obj_gc_safe = reinterpret_cast<llvmo::ValueAsMetadata_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ValueAsMetadata_O type_KIND_LISPALLOC_llvmo__ValueAsMetadata_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ValueAsMetadata_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::MDNode_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::MDNode_O* obj_gc_safe = reinterpret_cast<llvmo::MDNode_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::MDNode_O type_KIND_LISPALLOC_llvmo__MDNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MDNode_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::MDString_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::MDString_O* obj_gc_safe = reinterpret_cast<llvmo::MDString_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::MDString_O type_KIND_LISPALLOC_llvmo__MDString_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MDString_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ExecutionEngine_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ExecutionEngine_O* obj_gc_safe = reinterpret_cast<llvmo::ExecutionEngine_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    SMART_PTR_FIX(obj_gc_safe->_DependentModules);
    typedef llvmo::ExecutionEngine_O type_KIND_LISPALLOC_llvmo__ExecutionEngine_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ExecutionEngine_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::APFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::APFloat_O* obj_gc_safe = reinterpret_cast<llvmo::APFloat_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::APFloat_O type_KIND_LISPALLOC_llvmo__APFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__APFloat_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::PassManagerBuilder_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::PassManagerBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBuilder_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::PassManagerBuilder_O type_KIND_LISPALLOC_llvmo__PassManagerBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManagerBuilder_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DataLayout_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::DataLayout_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayout_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::DataLayout_O type_KIND_LISPALLOC_llvmo__DataLayout_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DataLayout_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Triple_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Triple_O* obj_gc_safe = reinterpret_cast<llvmo::Triple_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Triple_O type_KIND_LISPALLOC_llvmo__Triple_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Triple_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::APInt_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::APInt_O* obj_gc_safe = reinterpret_cast<llvmo::APInt_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::APInt_O type_KIND_LISPALLOC_llvmo__APInt_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__APInt_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::PassManagerBase_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::PassManagerBase_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBase_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::PassManagerBase_O type_KIND_LISPALLOC_llvmo__PassManagerBase_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManagerBase_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::FunctionPassManager_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::FunctionPassManager_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPassManager_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::FunctionPassManager_O type_KIND_LISPALLOC_llvmo__FunctionPassManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionPassManager_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::PassManager_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::PassManager_O* obj_gc_safe = reinterpret_cast<llvmo::PassManager_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::PassManager_O type_KIND_LISPALLOC_llvmo__PassManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PassManager_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::TargetMachine_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::TargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::TargetMachine_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::TargetMachine_O type_KIND_LISPALLOC_llvmo__TargetMachine_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetMachine_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::LLVMTargetMachine_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::LLVMTargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMTargetMachine_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::LLVMTargetMachine_O type_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::TargetOptions_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::TargetOptions_O* obj_gc_safe = reinterpret_cast<llvmo::TargetOptions_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::TargetOptions_O type_KIND_LISPALLOC_llvmo__TargetOptions_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetOptions_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Type_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Type_O* obj_gc_safe = reinterpret_cast<llvmo::Type_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Type_O type_KIND_LISPALLOC_llvmo__Type_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Type_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::IntegerType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::IntegerType_O* obj_gc_safe = reinterpret_cast<llvmo::IntegerType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::IntegerType_O type_KIND_LISPALLOC_llvmo__IntegerType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__IntegerType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::CompositeType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::CompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::CompositeType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::CompositeType_O type_KIND_LISPALLOC_llvmo__CompositeType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__CompositeType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::SequentialType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::SequentialType_O* obj_gc_safe = reinterpret_cast<llvmo::SequentialType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::SequentialType_O type_KIND_LISPALLOC_llvmo__SequentialType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__SequentialType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::VectorType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::VectorType_O* obj_gc_safe = reinterpret_cast<llvmo::VectorType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::VectorType_O type_KIND_LISPALLOC_llvmo__VectorType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__VectorType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::PointerType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::PointerType_O* obj_gc_safe = reinterpret_cast<llvmo::PointerType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::PointerType_O type_KIND_LISPALLOC_llvmo__PointerType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__PointerType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ArrayType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ArrayType_O* obj_gc_safe = reinterpret_cast<llvmo::ArrayType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ArrayType_O type_KIND_LISPALLOC_llvmo__ArrayType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ArrayType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::StructType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::StructType_O* obj_gc_safe = reinterpret_cast<llvmo::StructType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::StructType_O type_KIND_LISPALLOC_llvmo__StructType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__StructType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::FunctionType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::FunctionType_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionType_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::FunctionType_O type_KIND_LISPALLOC_llvmo__FunctionType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::NamedMDNode_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::NamedMDNode_O* obj_gc_safe = reinterpret_cast<llvmo::NamedMDNode_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::NamedMDNode_O type_KIND_LISPALLOC_llvmo__NamedMDNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__NamedMDNode_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Linker_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Linker_O* obj_gc_safe = reinterpret_cast<llvmo::Linker_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Linker_O type_KIND_LISPALLOC_llvmo__Linker_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Linker_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Pass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Pass_O* obj_gc_safe = reinterpret_cast<llvmo::Pass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Pass_O type_KIND_LISPALLOC_llvmo__Pass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Pass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::FunctionPass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::FunctionPass_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::FunctionPass_O type_KIND_LISPALLOC_llvmo__FunctionPass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__FunctionPass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ModulePass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ModulePass_O* obj_gc_safe = reinterpret_cast<llvmo::ModulePass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ModulePass_O type_KIND_LISPALLOC_llvmo__ModulePass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ModulePass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::ImmutablePass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::ImmutablePass_O* obj_gc_safe = reinterpret_cast<llvmo::ImmutablePass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::ImmutablePass_O type_KIND_LISPALLOC_llvmo__ImmutablePass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__ImmutablePass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DataLayoutPass_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::DataLayoutPass_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayoutPass_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::DataLayoutPass_O type_KIND_LISPALLOC_llvmo__DataLayoutPass_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DataLayoutPass_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::TargetLibraryInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::TargetLibraryInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetLibraryInfo_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::TargetLibraryInfo_O type_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::MCSubtargetInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::MCSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::MCSubtargetInfo_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::MCSubtargetInfo_O type_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::TargetSubtargetInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::TargetSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetSubtargetInfo_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::TargetSubtargetInfo_O type_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Module_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Module_O* obj_gc_safe = reinterpret_cast<llvmo::Module_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    SMART_PTR_FIX(obj_gc_safe->_UniqueGlobalVariableStrings);
    typedef llvmo::Module_O type_KIND_LISPALLOC_llvmo__Module_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Module_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::EngineBuilder_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::EngineBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::EngineBuilder_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::EngineBuilder_O type_KIND_LISPALLOC_llvmo__EngineBuilder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__EngineBuilder_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ForeignData_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ForeignData_O* obj_gc_safe = reinterpret_cast<core::ForeignData_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    SMART_PTR_FIX(obj_gc_safe->_Kind);
    typedef core::ForeignData_O type_KIND_LISPALLOC_core__ForeignData_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ForeignData_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::LLVMContext_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::LLVMContext_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMContext_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::LLVMContext_O type_KIND_LISPALLOC_llvmo__LLVMContext_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__LLVMContext_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::Target_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::Target_O* obj_gc_safe = reinterpret_cast<llvmo::Target_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    typedef llvmo::Target_O type_KIND_LISPALLOC_llvmo__Target_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__Target_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LoadTimeValues_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LoadTimeValues_O* obj_gc_safe = reinterpret_cast<core::LoadTimeValues_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_Objects._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_Symbols._Vector._Contents);
    typedef core::LoadTimeValues_O type_KIND_LISPALLOC_core__LoadTimeValues_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadTimeValues_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Binder_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Binder_O* obj_gc_safe = reinterpret_cast<core::Binder_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Bindings);
    SMART_PTR_FIX(obj_gc_safe->_Values);
    typedef core::Binder_O type_KIND_LISPALLOC_core__Binder_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Binder_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::IntArray_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::IntArray_O type_KIND_LISPALLOC_core__IntArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__IntArray_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SourceManager_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_SourcePosInfo);
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Record_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_alist);
    SMART_PTR_FIX(obj_gc_safe->_replacement_table);
    SMART_PTR_FIX(obj_gc_safe->_Seen);
    typedef core::Record_O type_KIND_LISPALLOC_core__Record_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Record_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LightUserData_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::LightUserData_O type_KIND_LISPALLOC_core__LightUserData_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LightUserData_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::UserData_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::UserData_O type_KIND_LISPALLOC_core__UserData_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UserData_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Symbol_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Name);
    SMART_PTR_FIX(obj_gc_safe->_HomePackage);
    SMART_PTR_FIX(obj_gc_safe->_Value);
    SMART_PTR_FIX(obj_gc_safe->_Function);
    SMART_PTR_FIX(obj_gc_safe->_SetfFunction);
    SMART_PTR_FIX(obj_gc_safe->_PropertyList);
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Null_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Null_O* obj_gc_safe = reinterpret_cast<core::Null_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Name);
    SMART_PTR_FIX(obj_gc_safe->_HomePackage);
    SMART_PTR_FIX(obj_gc_safe->_Value);
    SMART_PTR_FIX(obj_gc_safe->_Function);
    SMART_PTR_FIX(obj_gc_safe->_SetfFunction);
    SMART_PTR_FIX(obj_gc_safe->_PropertyList);
    typedef core::Null_O type_KIND_LISPALLOC_core__Null_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Null_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SourcePosInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::DirectoryIterator_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Path);
    typedef core::DirectoryIterator_O type_KIND_LISPALLOC_core__DirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryIterator_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::RecursiveDirectoryIterator_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Path);
    typedef core::RecursiveDirectoryIterator_O type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Regex_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Regex_O type_KIND_LISPALLOC_core__Regex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Regex_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::PosixTimeDuration_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::PosixTimeDuration_O type_KIND_LISPALLOC_core__PosixTimeDuration_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__PosixTimeDuration_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SymbolToEnumConverter_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_WhatTheEnumsRepresent._Contents);
    SMART_PTR_FIX(obj_gc_safe->_EnumToSymbol);
    SMART_PTR_FIX(obj_gc_safe->_ArchiveSymbolToEnum);
    SMART_PTR_FIX(obj_gc_safe->_EnumToArchiveSymbol);
    SMART_PTR_FIX(obj_gc_safe->_SymbolToEnum);
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CandoException_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_message._Contents);
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Stream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Stream_O* obj_gc_safe = reinterpret_cast<core::Stream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    typedef core::Stream_O type_KIND_LISPALLOC_core__Stream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Stream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::AnsiStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::AnsiStream_O* obj_gc_safe = reinterpret_cast<core::AnsiStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    typedef core::AnsiStream_O type_KIND_LISPALLOC_core__AnsiStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__AnsiStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::FileStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::FileStream_O* obj_gc_safe = reinterpret_cast<core::FileStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_Filename);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    typedef core::FileStream_O type_KIND_LISPALLOC_core__FileStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::IOStreamStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::IOStreamStream_O* obj_gc_safe = reinterpret_cast<core::IOStreamStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_Filename);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    typedef core::IOStreamStream_O type_KIND_LISPALLOC_core__IOStreamStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__IOStreamStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::IOFileStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::IOFileStream_O* obj_gc_safe = reinterpret_cast<core::IOFileStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_Filename);
    SMART_PTR_FIX(obj_gc_safe->_ElementType);
    typedef core::IOFileStream_O type_KIND_LISPALLOC_core__IOFileStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__IOFileStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ConcatenatedStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ConcatenatedStream_O* obj_gc_safe = reinterpret_cast<core::ConcatenatedStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_List);
    typedef core::ConcatenatedStream_O type_KIND_LISPALLOC_core__ConcatenatedStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ConcatenatedStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StringStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StringStream_O* obj_gc_safe = reinterpret_cast<core::StringStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    typedef core::StringStream_O type_KIND_LISPALLOC_core__StringStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StringStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StringInputStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StringInputStream_O* obj_gc_safe = reinterpret_cast<core::StringInputStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_Contents);
    typedef core::StringInputStream_O type_KIND_LISPALLOC_core__StringInputStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StringInputStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StringOutputStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StringOutputStream_O* obj_gc_safe = reinterpret_cast<core::StringOutputStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_Contents);
    typedef core::StringOutputStream_O type_KIND_LISPALLOC_core__StringOutputStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StringOutputStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SynonymStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SynonymStream_O* obj_gc_safe = reinterpret_cast<core::SynonymStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_SynonymSymbol);
    typedef core::SynonymStream_O type_KIND_LISPALLOC_core__SynonymStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SynonymStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::EchoStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::EchoStream_O* obj_gc_safe = reinterpret_cast<core::EchoStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_In);
    SMART_PTR_FIX(obj_gc_safe->_Out);
    typedef core::EchoStream_O type_KIND_LISPALLOC_core__EchoStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__EchoStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::TwoWayStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::TwoWayStream_O* obj_gc_safe = reinterpret_cast<core::TwoWayStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_In);
    SMART_PTR_FIX(obj_gc_safe->_Out);
    typedef core::TwoWayStream_O type_KIND_LISPALLOC_core__TwoWayStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TwoWayStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::BroadcastStream_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::BroadcastStream_O* obj_gc_safe = reinterpret_cast<core::BroadcastStream_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Format);
    SMART_PTR_FIX(obj_gc_safe->_ByteStack);
    SMART_PTR_FIX(obj_gc_safe->_ExternalFormat);
    SMART_PTR_FIX(obj_gc_safe->_Streams);
    typedef core::BroadcastStream_O type_KIND_LISPALLOC_core__BroadcastStream_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BroadcastStream_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Reader_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Reader_O* obj_gc_safe = reinterpret_cast<core::Reader_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Input);
    typedef core::Reader_O type_KIND_LISPALLOC_core__Reader_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Reader_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
ALWAYS_INLINE mps_res_t obj_scan_helper<core::Cons_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Car);
    SMART_PTR_FIX(obj_gc_safe->_Cdr);
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Archive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Archive_O* obj_gc_safe = reinterpret_cast<core::Archive_O*>(client);
    SIMPLE_POINTER_FIX(obj_gc_safe->_TopNode.theObject);
    typedef core::Archive_O type_KIND_LISPALLOC_core__Archive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Archive_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SaveArchive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    SIMPLE_POINTER_FIX(obj_gc_safe->_TopNode.theObject);
    SMART_PTR_FIX(obj_gc_safe->_SNodeForObject);
    typedef core::SaveArchive_O type_KIND_LISPALLOC_core__SaveArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SaveArchive_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SexpSaveArchive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SexpSaveArchive_O* obj_gc_safe = reinterpret_cast<core::SexpSaveArchive_O*>(client);
    SIMPLE_POINTER_FIX(obj_gc_safe->_TopNode.theObject);
    SMART_PTR_FIX(obj_gc_safe->_SNodeForObject);
    typedef core::SexpSaveArchive_O type_KIND_LISPALLOC_core__SexpSaveArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SexpSaveArchive_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LoadArchive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    SIMPLE_POINTER_FIX(obj_gc_safe->_TopNode.theObject);
    SMART_PTR_FIX(obj_gc_safe->_ObjectForSNode);
    SMART_PTR_FIX(obj_gc_safe->_NodesToFinalize);
    typedef core::LoadArchive_O type_KIND_LISPALLOC_core__LoadArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadArchive_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SexpLoadArchive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SexpLoadArchive_O* obj_gc_safe = reinterpret_cast<core::SexpLoadArchive_O*>(client);
    SIMPLE_POINTER_FIX(obj_gc_safe->_TopNode.theObject);
    SMART_PTR_FIX(obj_gc_safe->_ObjectForSNode);
    SMART_PTR_FIX(obj_gc_safe->_NodesToFinalize);
    typedef core::SexpLoadArchive_O type_KIND_LISPALLOC_core__SexpLoadArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SexpLoadArchive_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::HashTable_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::HashTable_O* obj_gc_safe = reinterpret_cast<core::HashTable_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_RehashSize);
    SMART_PTR_FIX(obj_gc_safe->_HashTable);
    typedef core::HashTable_O type_KIND_LISPALLOC_core__HashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTable_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::HashTableEq_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::HashTableEq_O* obj_gc_safe = reinterpret_cast<core::HashTableEq_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_RehashSize);
    SMART_PTR_FIX(obj_gc_safe->_HashTable);
    typedef core::HashTableEq_O type_KIND_LISPALLOC_core__HashTableEq_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEq_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::HashTableEqualp_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::HashTableEqualp_O* obj_gc_safe = reinterpret_cast<core::HashTableEqualp_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_RehashSize);
    SMART_PTR_FIX(obj_gc_safe->_HashTable);
    typedef core::HashTableEqualp_O type_KIND_LISPALLOC_core__HashTableEqualp_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEqualp_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::HashTableEql_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::HashTableEql_O* obj_gc_safe = reinterpret_cast<core::HashTableEql_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_RehashSize);
    SMART_PTR_FIX(obj_gc_safe->_HashTable);
    typedef core::HashTableEql_O type_KIND_LISPALLOC_core__HashTableEql_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEql_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::HashTableEqual_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::HashTableEqual_O* obj_gc_safe = reinterpret_cast<core::HashTableEqual_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_RehashSize);
    SMART_PTR_FIX(obj_gc_safe->_HashTable);
    typedef core::HashTableEqual_O type_KIND_LISPALLOC_core__HashTableEqual_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEqual_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<cffi::Pointer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef cffi::Pointer_O type_KIND_LISPALLOC_cffi__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_cffi__Pointer_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CxxObject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::WeakKeyMapping_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::WeakKeyMapping_O* obj_gc_safe = reinterpret_cast<core::WeakKeyMapping_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_WeakObject.Key);
    TAGGED_POINTER_FIX(obj_gc_safe->_WeakObject.Value);
    typedef core::WeakKeyMapping_O type_KIND_LISPALLOC_core__WeakKeyMapping_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyMapping_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LambdaListHandler_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LambdaListHandler_O* obj_gc_safe = reinterpret_cast<core::LambdaListHandler_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ClassifiedSymbolList);
    SIMPLE_POINTER_FIX(obj_gc_safe->_SpecialSymbolSet.theObject);
    SMART_PTR_FIX(obj_gc_safe->_DeclareSpecifierList);
    TAGGED_POINTER_FIX(obj_gc_safe->_RequiredArguments._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_OptionalArguments._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_RestArgument._ArgTarget);
    SMART_PTR_FIX(obj_gc_safe->_KeyFlag);
    TAGGED_POINTER_FIX(obj_gc_safe->_KeywordArguments._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_AllowOtherKeys);
    TAGGED_POINTER_FIX(obj_gc_safe->_AuxArguments._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_Comment._Contents);
    SMART_PTR_FIX(obj_gc_safe->_LexicalVariableNamesForDebugging);
    typedef core::LambdaListHandler_O type_KIND_LISPALLOC_core__LambdaListHandler_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LambdaListHandler_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::InsertPoint_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::InsertPoint_O type_KIND_LISPALLOC_llvmo__InsertPoint_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__InsertPoint_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SourceFileInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_pathname);
    SMART_PTR_FIX(obj_gc_safe->_SourceDebugNamestring);
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SNode_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::SNode_O type_KIND_LISPALLOC_core__SNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SNode_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LeafSNode_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LeafSNode_O* obj_gc_safe = reinterpret_cast<core::LeafSNode_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Object);
    typedef core::LeafSNode_O type_KIND_LISPALLOC_core__LeafSNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LeafSNode_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::BranchSNode_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::BranchSNode_O* obj_gc_safe = reinterpret_cast<core::BranchSNode_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Kind);
    SMART_PTR_FIX(obj_gc_safe->_SNodePList);
    SIMPLE_POINTER_FIX(obj_gc_safe->_VectorSNodes.theObject);
    typedef core::BranchSNode_O type_KIND_LISPALLOC_core__BranchSNode_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BranchSNode_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Path_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Path_O type_KIND_LISPALLOC_core__Path_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Path_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::AstVisitor_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::AstVisitor_O* obj_gc_safe = reinterpret_cast<asttooling::AstVisitor_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Target);
    typedef asttooling::AstVisitor_O type_KIND_LISPALLOC_asttooling__AstVisitor_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__AstVisitor_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::AttributeSet_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::AttributeSet_O type_KIND_LISPALLOC_llvmo__AttributeSet_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__AttributeSet_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::StructureObject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::StructureObject_O* obj_gc_safe = reinterpret_cast<core::StructureObject_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Type);
    TAGGED_POINTER_FIX(obj_gc_safe->_Slots._Vector._Contents);
    typedef core::StructureObject_O type_KIND_LISPALLOC_core__StructureObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StructureObject_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::InvocationHistoryFrameIterator_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::InvocationHistoryFrameIterator_O type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Package_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Package_O* obj_gc_safe = reinterpret_cast<core::Package_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_Name._Contents);
    SMART_PTR_FIX(obj_gc_safe->_InternalSymbols);
    SMART_PTR_FIX(obj_gc_safe->_ExternalSymbols);
    SMART_PTR_FIX(obj_gc_safe->_Shadowing);
    TAGGED_POINTER_FIX(obj_gc_safe->_UsingPackages._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_PackagesUsedBy._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Nicknames);
    typedef core::Package_O type_KIND_LISPALLOC_core__Package_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Package_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::DirectoryEntry_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::DirectoryEntry_O type_KIND_LISPALLOC_core__DirectoryEntry_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryEntry_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Character_dummy_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Character_dummy_O type_KIND_LISPALLOC_core__Character_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Character_dummy_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Function_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CompiledFunction_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SingleDispatchGenericFunction_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SingleDispatchGenericFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunction_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    typedef core::SingleDispatchGenericFunction_O type_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SpecialForm_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SpecialForm_O* obj_gc_safe = reinterpret_cast<core::SpecialForm_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_SpecialSymbol);
    typedef core::SpecialForm_O type_KIND_LISPALLOC_core__SpecialForm_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SpecialForm_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SingleDispatchEffectiveMethodFunction_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SingleDispatchEffectiveMethodFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchEffectiveMethodFunction_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_Methods);
    typedef core::SingleDispatchEffectiveMethodFunction_O type_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Instance_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    TAGGED_POINTER_FIX(obj_gc_safe->_Slots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Sig);
    typedef core::Instance_O type_KIND_LISPALLOC_core__Instance_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Instance_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Pointer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<clbind::ClassRegistry_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    clbind::ClassRegistry_O* obj_gc_safe = reinterpret_cast<clbind::ClassRegistry_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->m_classes);
    typedef clbind::ClassRegistry_O type_KIND_LISPALLOC_clbind__ClassRegistry_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_clbind__ClassRegistry_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DebugInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DebugInfo_O type_KIND_LISPALLOC_llvmo__DebugInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DebugInfo_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIDerivedType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIDerivedType_O type_KIND_LISPALLOC_llvmo__DIDerivedType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIDerivedType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIArray_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIArray_O type_KIND_LISPALLOC_llvmo__DIArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIArray_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIBasicType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIBasicType_O type_KIND_LISPALLOC_llvmo__DIBasicType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIBasicType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DISubprogram_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DISubprogram_O type_KIND_LISPALLOC_llvmo__DISubprogram_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DISubprogram_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DILexicalBlock_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DILexicalBlock_O type_KIND_LISPALLOC_llvmo__DILexicalBlock_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DILexicalBlock_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DICompileUnit_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DICompileUnit_O type_KIND_LISPALLOC_llvmo__DICompileUnit_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DICompileUnit_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIDescriptor_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIDescriptor_O type_KIND_LISPALLOC_llvmo__DIDescriptor_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIDescriptor_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIType_O type_KIND_LISPALLOC_llvmo__DIType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DISubroutineType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DISubroutineType_O type_KIND_LISPALLOC_llvmo__DISubroutineType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DISubroutineType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DICompositeType_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DICompositeType_O type_KIND_LISPALLOC_llvmo__DICompositeType_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DICompositeType_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DITypeArray_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DITypeArray_O type_KIND_LISPALLOC_llvmo__DITypeArray_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DITypeArray_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIFile_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIFile_O type_KIND_LISPALLOC_llvmo__DIFile_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIFile_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::DIScope_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef llvmo::DIScope_O type_KIND_LISPALLOC_llvmo__DIScope_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_llvmo__DIScope_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SmallMultimap_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SmallMultimap_O* obj_gc_safe = reinterpret_cast<core::SmallMultimap_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->map._Contents);
    typedef core::SmallMultimap_O type_KIND_LISPALLOC_core__SmallMultimap_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SmallMultimap_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Pathname_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Host);
    SMART_PTR_FIX(obj_gc_safe->_Device);
    SMART_PTR_FIX(obj_gc_safe->_Directory);
    SMART_PTR_FIX(obj_gc_safe->_Name);
    SMART_PTR_FIX(obj_gc_safe->_Type);
    SMART_PTR_FIX(obj_gc_safe->_Version);
    typedef core::Pathname_O type_KIND_LISPALLOC_core__Pathname_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pathname_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::LogicalPathname_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Host);
    SMART_PTR_FIX(obj_gc_safe->_Device);
    SMART_PTR_FIX(obj_gc_safe->_Directory);
    SMART_PTR_FIX(obj_gc_safe->_Name);
    SMART_PTR_FIX(obj_gc_safe->_Type);
    SMART_PTR_FIX(obj_gc_safe->_Version);
    typedef core::LogicalPathname_O type_KIND_LISPALLOC_core__LogicalPathname_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LogicalPathname_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::PosixTime_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::PosixTime_O type_KIND_LISPALLOC_core__PosixTime_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__PosixTime_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SmallMap_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SmallMap_O* obj_gc_safe = reinterpret_cast<core::SmallMap_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->map._Contents);
    typedef core::SmallMap_O type_KIND_LISPALLOC_core__SmallMap_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SmallMap_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Cache>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Cache* obj_gc_safe = reinterpret_cast<core::Cache*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_keys._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_table._Vector._Contents);
    typedef core::Cache type_KIND_CLASSALLOC_core__Cache;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__Cache)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Lisp_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Lisp_O* obj_gc_safe = reinterpret_cast<core::Lisp_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Roots._BufferStringPool);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._ExceptionStack._Stack._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Roots._TerminalIO);
    SMART_PTR_FIX(obj_gc_safe->_Roots._BformatStringOutputStream);
    SMART_PTR_FIX(obj_gc_safe->_Roots._BignumRegister0);
    SMART_PTR_FIX(obj_gc_safe->_Roots._BignumRegister1);
    SMART_PTR_FIX(obj_gc_safe->_Roots._BignumRegister2);
    SMART_PTR_FIX(obj_gc_safe->_Roots._IntegerOverflowAdjust);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots.charInfo.gIndexedCharacters._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots.charInfo.gCharacterNames._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._ClassSymbolsHolder._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SystemProperties);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._Bindings._Bindings._Vector._Contents);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._SourceFiles._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Roots._CatchInfo);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots.bootClassTable._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Roots._LoadTimeValueArrays);
    SMART_PTR_FIX(obj_gc_safe->_Roots._CommandLineArguments);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._Packages._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SetfDefinitions);
    SMART_PTR_FIX(obj_gc_safe->_Roots._CorePackage);
    SMART_PTR_FIX(obj_gc_safe->_Roots._KeywordPackage);
    SMART_PTR_FIX(obj_gc_safe->_Roots._CommonLispPackage);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SpecialForms);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SingleDispatchGenericFunctionTable);
    SMART_PTR_FIX(obj_gc_safe->_Roots._TrueObject);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._SingleDispatchMethodCachePtr);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._MethodCachePtr);
    TAGGED_POINTER_FIX(obj_gc_safe->_Roots._SlotCachePtr);
    SMART_PTR_FIX(obj_gc_safe->_Roots._RehashSize);
    SMART_PTR_FIX(obj_gc_safe->_Roots._RehashThreshold);
    SMART_PTR_FIX(obj_gc_safe->_Roots._NullStream);
    SMART_PTR_FIX(obj_gc_safe->_Roots._PathnameTranslations);
    SMART_PTR_FIX(obj_gc_safe->_Roots._ImaginaryUnit);
    SMART_PTR_FIX(obj_gc_safe->_Roots._ImaginaryUnitNegative);
    SMART_PTR_FIX(obj_gc_safe->_Roots._PlusHalf);
    SMART_PTR_FIX(obj_gc_safe->_Roots._MinusHalf);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SingleFloatMinusZero);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SingleFloatPlusZero);
    SMART_PTR_FIX(obj_gc_safe->_Roots._DoubleFloatMinusZero);
    SMART_PTR_FIX(obj_gc_safe->_Roots._DoubleFloatPlusZero);
    SMART_PTR_FIX(obj_gc_safe->_Roots._SingleFloatOne);
    SMART_PTR_FIX(obj_gc_safe->_Roots._DoubleFloatOne);
    typedef core::Lisp_O type_KIND_ROOTCLASSALLOC_core__Lisp_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_ROOTCLASSALLOC_core__Lisp_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::SingleDispatchGenericFunctionClosure>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SingleDispatchGenericFunctionClosure* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunctionClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_cleavir_ast);
    SMART_PTR_FIX(obj_gc_safe->_Methods);
    SMART_PTR_FIX(obj_gc_safe->_lambdaListHandler);
    typedef core::SingleDispatchGenericFunctionClosure type_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<llvmo::CompiledClosure>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    llvmo::CompiledClosure* obj_gc_safe = reinterpret_cast<llvmo::CompiledClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_cleavir_ast);
    SMART_PTR_FIX(obj_gc_safe->llvmFunction);
    SMART_PTR_FIX(obj_gc_safe->associatedFunctions);
    SMART_PTR_FIX(obj_gc_safe->_lambdaList);
    typedef llvmo::CompiledClosure type_KIND_CLASSALLOC_llvmo__CompiledClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_llvmo__CompiledClosure)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::internal::VariadicOperatorMatcherDescriptor>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::internal::VariadicOperatorMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::VariadicOperatorMatcherDescriptor*>(client);
    SMART_PTR_FIX(obj_gc_safe->MatcherName);
    typedef asttooling::internal::VariadicOperatorMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::internal::OverloadedMatcherDescriptor>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::internal::OverloadedMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::OverloadedMatcherDescriptor*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->Overloads._Vector._Contents);
    typedef asttooling::internal::OverloadedMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::internal::FixedArgCountMatcherDescriptor>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::internal::FixedArgCountMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::FixedArgCountMatcherDescriptor*>(client);
    SMART_PTR_FIX(obj_gc_safe->MatcherName);
    typedef asttooling::internal::FixedArgCountMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::internal::FreeFuncMatcherDescriptor>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::internal::FreeFuncMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::FreeFuncMatcherDescriptor*>(client);
    SMART_PTR_FIX(obj_gc_safe->MatcherName);
    typedef asttooling::internal::FreeFuncMatcherDescriptor type_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::MacroClosure>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_cleavir_ast);
    SMART_PTR_FIX(obj_gc_safe->_lambdaListHandler);
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::ConsStepper>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::ConsStepper* obj_gc_safe = reinterpret_cast<core::ConsStepper*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Cur);
    typedef core::ConsStepper type_KIND_CLASSALLOC_core__ConsStepper;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__ConsStepper)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::InstanceClosure>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::InstanceClosure* obj_gc_safe = reinterpret_cast<core::InstanceClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_cleavir_ast);
    SMART_PTR_FIX(obj_gc_safe->instance);
    SMART_PTR_FIX(obj_gc_safe->lambda_list);
    typedef core::InstanceClosure type_KIND_CLASSALLOC_core__InstanceClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__InstanceClosure)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::DerivableFrontendActionFactory>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::DerivableFrontendActionFactory* obj_gc_safe = reinterpret_cast<asttooling::DerivableFrontendActionFactory*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    TAGGED_POINTER_FIX(obj_gc_safe->_Slots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Sig);
    typedef asttooling::DerivableFrontendActionFactory type_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::DerivableMatchCallback>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::DerivableMatchCallback* obj_gc_safe = reinterpret_cast<asttooling::DerivableMatchCallback*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    TAGGED_POINTER_FIX(obj_gc_safe->_Slots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Sig);
    typedef asttooling::DerivableMatchCallback type_KIND_LISPALLOC_asttooling__DerivableMatchCallback;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableMatchCallback)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::DerivableASTFrontendAction>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::DerivableASTFrontendAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableASTFrontendAction*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    TAGGED_POINTER_FIX(obj_gc_safe->_Slots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Sig);
    typedef asttooling::DerivableASTFrontendAction type_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::CoreExposer>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CoreExposer* obj_gc_safe = reinterpret_cast<core::CoreExposer*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Package);
    typedef core::CoreExposer type_KIND_CLASSALLOC_core__CoreExposer;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__CoreExposer)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<asttooling::DerivableSyntaxOnlyAction>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    asttooling::DerivableSyntaxOnlyAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableSyntaxOnlyAction*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    SMART_PTR_FIX(obj_gc_safe->_Class);
    TAGGED_POINTER_FIX(obj_gc_safe->_Slots._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_Sig);
    typedef asttooling::DerivableSyntaxOnlyAction type_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::InterpretedClosure>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::InterpretedClosure* obj_gc_safe = reinterpret_cast<core::InterpretedClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_cleavir_ast);
    SMART_PTR_FIX(obj_gc_safe->_lambdaListHandler);
    SMART_PTR_FIX(obj_gc_safe->_declares);
    SMART_PTR_FIX(obj_gc_safe->_docstring);
    SMART_PTR_FIX(obj_gc_safe->_code);
    typedef core::InterpretedClosure type_KIND_CLASSALLOC_core__InterpretedClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__InterpretedClosure)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::VectorStepper>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::VectorStepper* obj_gc_safe = reinterpret_cast<core::VectorStepper*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Domain);
    typedef core::VectorStepper type_KIND_CLASSALLOC_core__VectorStepper;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__VectorStepper)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}

#endif // defined(GC_OBJ_SCAN_HELPERS)
#if defined(GC_OBJ_SCAN_TABLE)
static void* OBJ_SCAN_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_scan_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps
  /* 5 */ , &&obj_scan_KIND_ROOTCLASSALLOC_clbind__detail__class_map
  /* 6 */ , &&obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 7 */ , &&obj_scan_KIND_CLASSALLOC_clbind__DummyCreator
  /* 8 */ , &&obj_scan_KIND_CLASSALLOC_core__InstanceCreator
  /* 9 */ , &&obj_scan_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator
  /* 10 */ , &&obj_scan_KIND_BOOTSTRAP_core__T_O
  /* 11 */ , &&obj_scan_KIND_LISPALLOC_core__MultiStringBuffer_O
  /* 12 */ , &&obj_scan_KIND_LISPALLOC_core__ReadTable_O
  /* 13 */ , &&obj_scan_KIND_LISPALLOC_core__Number_O
  /* 14 */ , &&obj_scan_KIND_LISPALLOC_core__Complex_O
  /* 15 */ , &&obj_scan_KIND_LISPALLOC_core__Real_O
  /* 16 */ , &&obj_scan_KIND_LISPALLOC_core__Rational_O
  /* 17 */ , &&obj_scan_KIND_LISPALLOC_core__Integer_O
  /* 18 */ , &&obj_scan_KIND_LISPALLOC_core__Bignum_O
  /* 19 */ , &&obj_scan_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 20 */ , &&obj_scan_KIND_LISPALLOC_core__Ratio_O
  /* 21 */ , &&obj_scan_KIND_LISPALLOC_core__Float_O
  /* 22 */ , &&obj_scan_KIND_LISPALLOC_core__DoubleFloat_O
  /* 23 */ , &&obj_scan_KIND_LISPALLOC_core__LongFloat_O
  /* 24 */ , &&obj_scan_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 25 */ , &&obj_scan_KIND_LISPALLOC_core__ShortFloat_O
  /* 26 */ , &&obj_scan_KIND_LISPALLOC_core__FileStatus_O
  /* 27 */ , &&obj_scan_KIND_LISPALLOC_core__WeakHashTable_O
  /* 28 */ , &&obj_scan_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 29 */ , &&obj_scan_KIND_LISPALLOC_core__Environment_O
  /* 30 */ , &&obj_scan_KIND_LISPALLOC_core__ActivationFrame_O
  /* 31 */ , &&obj_scan_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 32 */ , &&obj_scan_KIND_LISPALLOC_core__ValueFrame_O
  /* 33 */ , &&obj_scan_KIND_LISPALLOC_core__FunctionFrame_O
  /* 34 */ , &&obj_scan_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 35 */ , &&obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 36 */ , &&obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 37 */ , &&obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 38 */ , &&obj_scan_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 39 */ , &&obj_scan_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 40 */ , &&obj_scan_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 41 */ , &&obj_scan_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 42 */ , &&obj_scan_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 43 */ , &&obj_scan_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 44 */ , &&obj_scan_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 45 */ , &&obj_scan_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 46 */ , &&obj_scan_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 47 */ , &&obj_scan_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 48 */ , &&obj_scan_KIND_LISPALLOC_core__Array_O
  /* 49 */ , &&obj_scan_KIND_LISPALLOC_core__ArrayObjects_O
  /* 50 */ , &&obj_scan_KIND_LISPALLOC_core__ArrayDisplaced_O
  /* 51 */ , &&obj_scan_KIND_LISPALLOC_core__Vector_O
  /* 52 */ , &&obj_scan_KIND_LISPALLOC_core__BitVector_O
  /* 53 */ , &&obj_scan_KIND_LISPALLOC_core__SimpleBitVector_O
  /* 54 */ , &&obj_scan_KIND_LISPALLOC_core__BitVectorWithFillPtr_O
  /* 55 */ , &&obj_scan_KIND_LISPALLOC_core__VectorDisplaced_O
  /* 56 */ , &&obj_scan_KIND_LISPALLOC_core__String_O
  /* 57 */ , &&obj_scan_KIND_BOOTSTRAP_core__Str_O
  /* 58 */ , &&obj_scan_KIND_LISPALLOC_core__StrWithFillPtr_O
  /* 59 */ , &&obj_scan_KIND_LISPALLOC_core__VectorObjects_O
  /* 60 */ , &&obj_scan_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 61 */ , &&obj_scan_KIND_LISPALLOC_core__SingleDispatchMethod_O
  /* 62 */ , &&obj_scan_KIND_LISPALLOC_core__RandomState_O
  /* 63 */ , &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O
  /* 64 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DebugLoc_O
  /* 65 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Attribute_O
  /* 66 */ , &&obj_scan_KIND_LISPALLOC_core__RegexMatch_O
  /* 67 */ , &&obj_scan_KIND_LISPALLOC_core__WeakPointer_O
  /* 68 */ , &&obj_scan_KIND_LISPALLOC_core__VaList_dummy_O
  /* 69 */ , &&obj_scan_KIND_BOOTSTRAP_core__StandardObject_O
  /* 70 */ , &&obj_scan_KIND_BOOTSTRAP_core__Metaobject_O
  /* 71 */ , &&obj_scan_KIND_BOOTSTRAP_core__Specializer_O
  /* 72 */ , &&obj_scan_KIND_BOOTSTRAP_core__Class_O
  /* 73 */ , &&obj_scan_KIND_BOOTSTRAP_core__StdClass_O
  /* 74 */ , &&obj_scan_KIND_BOOTSTRAP_core__StandardClass_O
  /* 75 */ , &&obj_scan_KIND_LISPALLOC_core__FuncallableStandardClass_O
  /* 76 */ , &&obj_scan_KIND_BOOTSTRAP_core__StructureClass_O
  /* 77 */ , &&obj_scan_KIND_LISPALLOC_core__ForwardReferencedClass_O
  /* 78 */ , &&obj_scan_KIND_LISPALLOC_core__CxxClass_O
  /* 79 */ , &&obj_scan_KIND_BOOTSTRAP_core__BuiltInClass_O
  /* 80 */ , &&obj_scan_KIND_LISPALLOC_clbind__ClassRep_O
  /* 81 */ , &&obj_scan_KIND_LISPALLOC_core__ExternalObject_O
  /* 82 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Value_O
  /* 83 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Argument_O
  /* 84 */ , &&obj_scan_KIND_LISPALLOC_llvmo__User_O
  /* 85 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Instruction_O
  /* 86 */ , &&obj_scan_KIND_LISPALLOC_llvmo__AtomicRMWInst_O
  /* 87 */ , &&obj_scan_KIND_LISPALLOC_llvmo__LandingPadInst_O
  /* 88 */ , &&obj_scan_KIND_LISPALLOC_llvmo__PHINode_O
  /* 89 */ , &&obj_scan_KIND_LISPALLOC_llvmo__CallInst_O
  /* 90 */ , &&obj_scan_KIND_LISPALLOC_llvmo__StoreInst_O
  /* 91 */ , &&obj_scan_KIND_LISPALLOC_llvmo__UnaryInstruction_O
  /* 92 */ , &&obj_scan_KIND_LISPALLOC_llvmo__LoadInst_O
  /* 93 */ , &&obj_scan_KIND_LISPALLOC_llvmo__AllocaInst_O
  /* 94 */ , &&obj_scan_KIND_LISPALLOC_llvmo__VAArgInst_O
  /* 95 */ , &&obj_scan_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O
  /* 96 */ , &&obj_scan_KIND_LISPALLOC_llvmo__TerminatorInst_O
  /* 97 */ , &&obj_scan_KIND_LISPALLOC_llvmo__UnreachableInst_O
  /* 98 */ , &&obj_scan_KIND_LISPALLOC_llvmo__SwitchInst_O
  /* 99 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ReturnInst_O
  /* 100 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ResumeInst_O
  /* 101 */ , &&obj_scan_KIND_LISPALLOC_llvmo__BranchInst_O
  /* 102 */ , &&obj_scan_KIND_LISPALLOC_llvmo__InvokeInst_O
  /* 103 */ , &&obj_scan_KIND_LISPALLOC_llvmo__IndirectBrInst_O
  /* 104 */ , &&obj_scan_KIND_LISPALLOC_llvmo__FenceInst_O
  /* 105 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Constant_O
  /* 106 */ , &&obj_scan_KIND_LISPALLOC_llvmo__BlockAddress_O
  /* 107 */ , &&obj_scan_KIND_LISPALLOC_llvmo__GlobalValue_O
  /* 108 */ , &&obj_scan_KIND_LISPALLOC_llvmo__GlobalVariable_O
  /* 109 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Function_O
  /* 110 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantArray_O
  /* 111 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantInt_O
  /* 112 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantDataSequential_O
  /* 113 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantDataArray_O
  /* 114 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantStruct_O
  /* 115 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantFP_O
  /* 116 */ , &&obj_scan_KIND_LISPALLOC_llvmo__UndefValue_O
  /* 117 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantPointerNull_O
  /* 118 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ConstantExpr_O
  /* 119 */ , &&obj_scan_KIND_LISPALLOC_llvmo__BasicBlock_O
  /* 120 */ , &&obj_scan_KIND_LISPALLOC_llvmo__IRBuilderBase_O
  /* 121 */ , &&obj_scan_KIND_LISPALLOC_llvmo__IRBuilder_O
  /* 122 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIBuilder_O
  /* 123 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Metadata_O
  /* 124 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ValueAsMetadata_O
  /* 125 */ , &&obj_scan_KIND_LISPALLOC_llvmo__MDNode_O
  /* 126 */ , &&obj_scan_KIND_LISPALLOC_llvmo__MDString_O
  /* 127 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ExecutionEngine_O
  /* 128 */ , &&obj_scan_KIND_LISPALLOC_llvmo__APFloat_O
  /* 129 */ , &&obj_scan_KIND_LISPALLOC_llvmo__PassManagerBuilder_O
  /* 130 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DataLayout_O
  /* 131 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Triple_O
  /* 132 */ , &&obj_scan_KIND_LISPALLOC_llvmo__APInt_O
  /* 133 */ , &&obj_scan_KIND_LISPALLOC_llvmo__PassManagerBase_O
  /* 134 */ , &&obj_scan_KIND_LISPALLOC_llvmo__FunctionPassManager_O
  /* 135 */ , &&obj_scan_KIND_LISPALLOC_llvmo__PassManager_O
  /* 136 */ , &&obj_scan_KIND_LISPALLOC_llvmo__TargetMachine_O
  /* 137 */ , &&obj_scan_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O
  /* 138 */ , &&obj_scan_KIND_LISPALLOC_llvmo__TargetOptions_O
  /* 139 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Type_O
  /* 140 */ , &&obj_scan_KIND_LISPALLOC_llvmo__IntegerType_O
  /* 141 */ , &&obj_scan_KIND_LISPALLOC_llvmo__CompositeType_O
  /* 142 */ , &&obj_scan_KIND_LISPALLOC_llvmo__SequentialType_O
  /* 143 */ , &&obj_scan_KIND_LISPALLOC_llvmo__VectorType_O
  /* 144 */ , &&obj_scan_KIND_LISPALLOC_llvmo__PointerType_O
  /* 145 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ArrayType_O
  /* 146 */ , &&obj_scan_KIND_LISPALLOC_llvmo__StructType_O
  /* 147 */ , &&obj_scan_KIND_LISPALLOC_llvmo__FunctionType_O
  /* 148 */ , &&obj_scan_KIND_LISPALLOC_llvmo__NamedMDNode_O
  /* 149 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Linker_O
  /* 150 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Pass_O
  /* 151 */ , &&obj_scan_KIND_LISPALLOC_llvmo__FunctionPass_O
  /* 152 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ModulePass_O
  /* 153 */ , &&obj_scan_KIND_LISPALLOC_llvmo__ImmutablePass_O
  /* 154 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DataLayoutPass_O
  /* 155 */ , &&obj_scan_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O
  /* 156 */ , &&obj_scan_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O
  /* 157 */ , &&obj_scan_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O
  /* 158 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Module_O
  /* 159 */ , &&obj_scan_KIND_LISPALLOC_llvmo__EngineBuilder_O
  /* 160 */ , &&obj_scan_KIND_LISPALLOC_core__ForeignData_O
  /* 161 */ , &&obj_scan_KIND_LISPALLOC_llvmo__LLVMContext_O
  /* 162 */ , &&obj_scan_KIND_LISPALLOC_llvmo__Target_O
  /* 163 */ , &&obj_scan_KIND_LISPALLOC_core__LoadTimeValues_O
  /* 164 */ , &&obj_scan_KIND_LISPALLOC_core__Binder_O
  /* 165 */ , &&obj_scan_KIND_LISPALLOC_core__IntArray_O
  /* 166 */ , &&obj_scan_KIND_LISPALLOC_core__SourceManager_O
  /* 167 */ , &&obj_scan_KIND_LISPALLOC_core__Record_O
  /* 168 */ , &&obj_scan_KIND_LISPALLOC_core__LightUserData_O
  /* 169 */ , &&obj_scan_KIND_LISPALLOC_core__UserData_O
  /* 170 */ , &&obj_scan_KIND_BOOTSTRAP_core__Symbol_O
  /* 171 */ , &&obj_scan_KIND_LISPALLOC_core__Null_O
  /* 172 */ , &&obj_scan_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 173 */ , &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__Iterator_O
  /* 174 */ , &&obj_scan_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 175 */ , &&obj_scan_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 176 */ , &&obj_scan_KIND_LISPALLOC_core__Regex_O
  /* 177 */ , &&obj_scan_KIND_LISPALLOC_core__PosixTimeDuration_O
  /* 178 */ , &&obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 179 */ , &&obj_scan_KIND_LISPALLOC_core__CandoException_O
  /* 180 */ , &&obj_scan_KIND_LISPALLOC_core__Stream_O
  /* 181 */ , &&obj_scan_KIND_LISPALLOC_core__AnsiStream_O
  /* 182 */ , &&obj_scan_KIND_LISPALLOC_core__FileStream_O
  /* 183 */ , &&obj_scan_KIND_LISPALLOC_core__IOStreamStream_O
  /* 184 */ , &&obj_scan_KIND_LISPALLOC_core__IOFileStream_O
  /* 185 */ , &&obj_scan_KIND_LISPALLOC_core__ConcatenatedStream_O
  /* 186 */ , &&obj_scan_KIND_LISPALLOC_core__StringStream_O
  /* 187 */ , &&obj_scan_KIND_LISPALLOC_core__StringInputStream_O
  /* 188 */ , &&obj_scan_KIND_LISPALLOC_core__StringOutputStream_O
  /* 189 */ , &&obj_scan_KIND_LISPALLOC_core__SynonymStream_O
  /* 190 */ , &&obj_scan_KIND_LISPALLOC_core__EchoStream_O
  /* 191 */ , &&obj_scan_KIND_LISPALLOC_core__TwoWayStream_O
  /* 192 */ , &&obj_scan_KIND_LISPALLOC_core__BroadcastStream_O
  /* 193 */ , &&obj_scan_KIND_LISPALLOC_core__Reader_O
  /* 194 */ , &&obj_scan_KIND_LISPALLOC_core__Cons_O
  /* 195 */ , &&obj_scan_KIND_LISPALLOC_core__Archive_O
  /* 196 */ , &&obj_scan_KIND_LISPALLOC_core__SaveArchive_O
  /* 197 */ , &&obj_scan_KIND_LISPALLOC_core__SexpSaveArchive_O
  /* 198 */ , &&obj_scan_KIND_LISPALLOC_core__LoadArchive_O
  /* 199 */ , &&obj_scan_KIND_LISPALLOC_core__SexpLoadArchive_O
  /* 200 */ , &&obj_scan_KIND_LISPALLOC_core__HashTable_O
  /* 201 */ , &&obj_scan_KIND_LISPALLOC_core__HashTableEq_O
  /* 202 */ , &&obj_scan_KIND_LISPALLOC_core__HashTableEqualp_O
  /* 203 */ , &&obj_scan_KIND_LISPALLOC_core__HashTableEql_O
  /* 204 */ , &&obj_scan_KIND_LISPALLOC_core__HashTableEqual_O
  /* 205 */ , &&obj_scan_KIND_LISPALLOC_cffi__Pointer_O
  /* 206 */ , &&obj_scan_KIND_LISPALLOC_core__CxxObject_O
  /* 207 */ , &&obj_scan_KIND_LISPALLOC_core__WeakKeyMapping_O
  /* 208 */ , &&obj_scan_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 209 */ , &&obj_scan_KIND_LISPALLOC_llvmo__InsertPoint_O
  /* 210 */ , &&obj_scan_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 211 */ , &&obj_scan_KIND_LISPALLOC_core__SNode_O
  /* 212 */ , &&obj_scan_KIND_LISPALLOC_core__LeafSNode_O
  /* 213 */ , &&obj_scan_KIND_LISPALLOC_core__BranchSNode_O
  /* 214 */ , &&obj_scan_KIND_LISPALLOC_core__Path_O
  /* 215 */ , &&obj_scan_KIND_LISPALLOC_asttooling__AstVisitor_O
  /* 216 */ , &&obj_scan_KIND_LISPALLOC_llvmo__AttributeSet_O
  /* 217 */ , &&obj_scan_KIND_LISPALLOC_core__StructureObject_O
  /* 218 */ , &&obj_scan_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 219 */ , &&obj_scan_KIND_LISPALLOC_core__Package_O
  /* 220 */ , &&obj_scan_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 221 */ , &&obj_scan_KIND_LISPALLOC_core__Character_dummy_O
  /* 222 */ , &&obj_scan_KIND_LISPALLOC_core__Function_O
  /* 223 */ , &&obj_scan_KIND_LISPALLOC_core__CompiledFunction_O
  /* 224 */ , &&obj_scan_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O
  /* 225 */ , &&obj_scan_KIND_LISPALLOC_core__SpecialForm_O
  /* 226 */ , &&obj_scan_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O
  /* 227 */ , &&obj_scan_KIND_LISPALLOC_core__Instance_O
  /* 228 */ , &&obj_scan_KIND_LISPALLOC_core__Pointer_O
  /* 229 */ , &&obj_scan_KIND_LISPALLOC_clbind__ClassRegistry_O
  /* 230 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DebugInfo_O
  /* 231 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIDerivedType_O
  /* 232 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIArray_O
  /* 233 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIBasicType_O
  /* 234 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DISubprogram_O
  /* 235 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DILexicalBlock_O
  /* 236 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DICompileUnit_O
  /* 237 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIDescriptor_O
  /* 238 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIType_O
  /* 239 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DISubroutineType_O
  /* 240 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DICompositeType_O
  /* 241 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DITypeArray_O
  /* 242 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIFile_O
  /* 243 */ , &&obj_scan_KIND_LISPALLOC_llvmo__DIScope_O
  /* 244 */ , &&obj_scan_KIND_LISPALLOC_core__SmallMultimap_O
  /* 245 */ , &&obj_scan_KIND_LISPALLOC_core__Pathname_O
  /* 246 */ , &&obj_scan_KIND_LISPALLOC_core__LogicalPathname_O
  /* 247 */ , &&obj_scan_KIND_LISPALLOC_core__PosixTime_O
  /* 248 */ , &&obj_scan_KIND_LISPALLOC_core__SmallMap_O
  /* 249 */ , &&obj_scan_KIND_CLASSALLOC_core__Cache
  /* 250 */ , &&obj_scan_KIND_ROOTCLASSALLOC_core__Lisp_O
  /* 251 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 252 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 253 */ , &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 254 */ , &&obj_scan_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure
  /* 255 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 256 */ , &&obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 257 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 258 */ , &&obj_scan_KIND_CLASSALLOC_llvmo__CompiledClosure
  /* 259 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 260 */ , &&obj_scan_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor
  /* 261 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___
  /* 262 */ , &&obj_scan_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor
  /* 263 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_
  /* 264 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_
  /* 265 */ , &&obj_scan_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor
  /* 266 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_
  /* 267 */ , &&obj_scan_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor
  /* 268 */ , &&obj_scan_KIND_CLASSALLOC_core__MacroClosure
  /* 269 */ , &&obj_scan_KIND_CLASSALLOC_core__ConsStepper
  /* 270 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 271 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_
  /* 272 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 273 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 274 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_
  /* 275 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___
  /* 276 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 277 */ , &&obj_scan_KIND_CLASSALLOC_core__InstanceClosure
  /* 278 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 279 */ , &&obj_scan_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory
  /* 280 */ , &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_
  /* 281 */ , &&obj_scan_KIND_LISPALLOC_asttooling__DerivableMatchCallback
  /* 282 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_
  /* 283 */ , &&obj_scan_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction
  /* 284 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_
  /* 285 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__
  /* 286 */ , &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_
  /* 287 */ , &&obj_scan_KIND_CLASSALLOC_core__CoreExposer
  /* 288 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 289 */ , &&obj_scan_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction
  /* 290 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__
  /* 291 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 292 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 293 */ , &&obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 294 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 295 */ , &&obj_scan_KIND_CLASSALLOC_core__InterpretedClosure
  /* 296 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 297 */ , &&obj_scan_KIND_CLASSALLOC_core__VectorStepper
  /* 298 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__
};
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
obj_finalize_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps:
{
    asttooling::RegMap::RegistryMaps* obj_gc_safe = reinterpret_cast<asttooling::RegMap::RegistryMaps*>(client);
    obj_gc_safe->~RegistryMaps();
    return;
}
obj_finalize_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
    clbind::detail::class_map* obj_gc_safe = reinterpret_cast<clbind::detail::class_map*>(client);
    obj_gc_safe->~class_map();
    return;
}
obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    obj_gc_safe->~Creator();
}
obj_finalize_KIND_CLASSALLOC_clbind__DummyCreator:
{
    clbind::DummyCreator* obj_gc_safe = reinterpret_cast<clbind::DummyCreator*>(client);
    obj_gc_safe->~DummyCreator();
    return;
}
obj_finalize_KIND_CLASSALLOC_core__InstanceCreator:
{
    core::InstanceCreator* obj_gc_safe = reinterpret_cast<core::InstanceCreator*>(client);
    obj_gc_safe->~InstanceCreator();
    return;
}
obj_finalize_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    obj_gc_safe->~ConstructorCreator();
}
obj_finalize_KIND_BOOTSTRAP_core__T_O:
{
    core::T_O* obj_gc_safe = reinterpret_cast<core::T_O*>(client);
    obj_gc_safe->~T_O();
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
obj_finalize_KIND_LISPALLOC_core__VaList_dummy_O:
{
    core::VaList_dummy_O* obj_gc_safe = reinterpret_cast<core::VaList_dummy_O*>(client);
    obj_gc_safe->~VaList_dummy_O();
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
obj_finalize_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    obj_gc_safe->~Cons_O();
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
obj_finalize_KIND_CLASSALLOC_core__Cache:
{
    core::Cache* obj_gc_safe = reinterpret_cast<core::Cache*>(client);
    obj_gc_safe->~Cache();
    return;
}
obj_finalize_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
    core::Lisp_O* obj_gc_safe = reinterpret_cast<core::Lisp_O*>(client);
    obj_gc_safe->~Lisp_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::KeywordArgument>"));}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_finalize_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure:
{
    core::SingleDispatchGenericFunctionClosure* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunctionClosure*>(client);
    obj_gc_safe->~SingleDispatchGenericFunctionClosure();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::RequiredArgument>"));}
obj_finalize_KIND_CLASSALLOC_llvmo__CompiledClosure:
{
    llvmo::CompiledClosure* obj_gc_safe = reinterpret_cast<llvmo::CompiledClosure*>(client);
    obj_gc_safe->~CompiledClosure();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_finalize_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor:
{
    asttooling::internal::VariadicOperatorMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::VariadicOperatorMatcherDescriptor*>(client);
    obj_gc_safe->~VariadicOperatorMatcherDescriptor();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_finalize_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor:
{
    asttooling::internal::OverloadedMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::OverloadedMatcherDescriptor*>(client);
    obj_gc_safe->~OverloadedMatcherDescriptor();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolStorage>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ContextFrame>"));}
obj_finalize_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor:
{
    asttooling::internal::FixedArgCountMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::FixedArgCountMatcherDescriptor*>(client);
    obj_gc_safe->~FixedArgCountMatcherDescriptor();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::T_O *>"));}
obj_finalize_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor:
{
    asttooling::internal::FreeFuncMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::FreeFuncMatcherDescriptor*>(client);
    obj_gc_safe->~FreeFuncMatcherDescriptor();
    return;
}
obj_finalize_KIND_CLASSALLOC_core__MacroClosure:
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    obj_gc_safe->~MacroClosure();
    return;
}
obj_finalize_KIND_CLASSALLOC_core__ConsStepper:
{
    core::ConsStepper* obj_gc_safe = reinterpret_cast<core::ConsStepper*>(client);
    obj_gc_safe->~ConsStepper();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::AuxArgument>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ParserValue>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_finalize_KIND_CLASSALLOC_core__InstanceClosure:
{
    core::InstanceClosure* obj_gc_safe = reinterpret_cast<core::InstanceClosure*>(client);
    obj_gc_safe->~InstanceClosure();
    return;
}
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
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>"));}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>"));}
obj_finalize_KIND_CLASSALLOC_core__CoreExposer:
{
    core::CoreExposer* obj_gc_safe = reinterpret_cast<core::CoreExposer*>(client);
    obj_gc_safe->~CoreExposer();
    return;
}
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
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    obj_gc_safe->~BuiltinClosure();
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_finalize_KIND_CLASSALLOC_core__InterpretedClosure:
{
    core::InterpretedClosure* obj_gc_safe = reinterpret_cast<core::InterpretedClosure*>(client);
    obj_gc_safe->~InterpretedClosure();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_finalize_KIND_CLASSALLOC_core__VectorStepper:
{
    core::VectorStepper* obj_gc_safe = reinterpret_cast<core::VectorStepper*>(client);
    obj_gc_safe->~VectorStepper();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>"));}
#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_HELPERS)

#endif // defined(GC_OBJ_FINALIZE_HELPERS)
#if defined(GC_OBJ_FINALIZE_TABLE)
static void* OBJ_FINALIZE_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_finalize_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps
  /* 5 */ , &&obj_finalize_KIND_ROOTCLASSALLOC_clbind__detail__class_map
  /* 6 */ , &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 7 */ , &&obj_finalize_KIND_CLASSALLOC_clbind__DummyCreator
  /* 8 */ , &&obj_finalize_KIND_CLASSALLOC_core__InstanceCreator
  /* 9 */ , &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator
  /* 10 */ , &&obj_finalize_KIND_BOOTSTRAP_core__T_O
  /* 11 */ , &&obj_finalize_KIND_LISPALLOC_core__MultiStringBuffer_O
  /* 12 */ , &&obj_finalize_KIND_LISPALLOC_core__ReadTable_O
  /* 13 */ , &&obj_finalize_KIND_LISPALLOC_core__Number_O
  /* 14 */ , &&obj_finalize_KIND_LISPALLOC_core__Complex_O
  /* 15 */ , &&obj_finalize_KIND_LISPALLOC_core__Real_O
  /* 16 */ , &&obj_finalize_KIND_LISPALLOC_core__Rational_O
  /* 17 */ , &&obj_finalize_KIND_LISPALLOC_core__Integer_O
  /* 18 */ , &&obj_finalize_KIND_LISPALLOC_core__Bignum_O
  /* 19 */ , &&obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 20 */ , &&obj_finalize_KIND_LISPALLOC_core__Ratio_O
  /* 21 */ , &&obj_finalize_KIND_LISPALLOC_core__Float_O
  /* 22 */ , &&obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O
  /* 23 */ , &&obj_finalize_KIND_LISPALLOC_core__LongFloat_O
  /* 24 */ , &&obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 25 */ , &&obj_finalize_KIND_LISPALLOC_core__ShortFloat_O
  /* 26 */ , &&obj_finalize_KIND_LISPALLOC_core__FileStatus_O
  /* 27 */ , &&obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O
  /* 28 */ , &&obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 29 */ , &&obj_finalize_KIND_LISPALLOC_core__Environment_O
  /* 30 */ , &&obj_finalize_KIND_LISPALLOC_core__ActivationFrame_O
  /* 31 */ , &&obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 32 */ , &&obj_finalize_KIND_LISPALLOC_core__ValueFrame_O
  /* 33 */ , &&obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O
  /* 34 */ , &&obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 35 */ , &&obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 36 */ , &&obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 37 */ , &&obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 38 */ , &&obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 39 */ , &&obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 40 */ , &&obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 41 */ , &&obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 42 */ , &&obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 43 */ , &&obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 44 */ , &&obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 45 */ , &&obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 46 */ , &&obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 47 */ , &&obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 48 */ , &&obj_finalize_KIND_LISPALLOC_core__Array_O
  /* 49 */ , &&obj_finalize_KIND_LISPALLOC_core__ArrayObjects_O
  /* 50 */ , &&obj_finalize_KIND_LISPALLOC_core__ArrayDisplaced_O
  /* 51 */ , &&obj_finalize_KIND_LISPALLOC_core__Vector_O
  /* 52 */ , &&obj_finalize_KIND_LISPALLOC_core__BitVector_O
  /* 53 */ , &&obj_finalize_KIND_LISPALLOC_core__SimpleBitVector_O
  /* 54 */ , &&obj_finalize_KIND_LISPALLOC_core__BitVectorWithFillPtr_O
  /* 55 */ , &&obj_finalize_KIND_LISPALLOC_core__VectorDisplaced_O
  /* 56 */ , &&obj_finalize_KIND_LISPALLOC_core__String_O
  /* 57 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Str_O
  /* 58 */ , &&obj_finalize_KIND_LISPALLOC_core__StrWithFillPtr_O
  /* 59 */ , &&obj_finalize_KIND_LISPALLOC_core__VectorObjects_O
  /* 60 */ , &&obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 61 */ , &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchMethod_O
  /* 62 */ , &&obj_finalize_KIND_LISPALLOC_core__RandomState_O
  /* 63 */ , &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O
  /* 64 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DebugLoc_O
  /* 65 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Attribute_O
  /* 66 */ , &&obj_finalize_KIND_LISPALLOC_core__RegexMatch_O
  /* 67 */ , &&obj_finalize_KIND_LISPALLOC_core__WeakPointer_O
  /* 68 */ , &&obj_finalize_KIND_LISPALLOC_core__VaList_dummy_O
  /* 69 */ , &&obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O
  /* 70 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O
  /* 71 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Specializer_O
  /* 72 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Class_O
  /* 73 */ , &&obj_finalize_KIND_BOOTSTRAP_core__StdClass_O
  /* 74 */ , &&obj_finalize_KIND_BOOTSTRAP_core__StandardClass_O
  /* 75 */ , &&obj_finalize_KIND_LISPALLOC_core__FuncallableStandardClass_O
  /* 76 */ , &&obj_finalize_KIND_BOOTSTRAP_core__StructureClass_O
  /* 77 */ , &&obj_finalize_KIND_LISPALLOC_core__ForwardReferencedClass_O
  /* 78 */ , &&obj_finalize_KIND_LISPALLOC_core__CxxClass_O
  /* 79 */ , &&obj_finalize_KIND_BOOTSTRAP_core__BuiltInClass_O
  /* 80 */ , &&obj_finalize_KIND_LISPALLOC_clbind__ClassRep_O
  /* 81 */ , &&obj_finalize_KIND_LISPALLOC_core__ExternalObject_O
  /* 82 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Value_O
  /* 83 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Argument_O
  /* 84 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__User_O
  /* 85 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Instruction_O
  /* 86 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__AtomicRMWInst_O
  /* 87 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__LandingPadInst_O
  /* 88 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__PHINode_O
  /* 89 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__CallInst_O
  /* 90 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__StoreInst_O
  /* 91 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__UnaryInstruction_O
  /* 92 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__LoadInst_O
  /* 93 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__AllocaInst_O
  /* 94 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__VAArgInst_O
  /* 95 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O
  /* 96 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__TerminatorInst_O
  /* 97 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__UnreachableInst_O
  /* 98 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__SwitchInst_O
  /* 99 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ReturnInst_O
  /* 100 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ResumeInst_O
  /* 101 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__BranchInst_O
  /* 102 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__InvokeInst_O
  /* 103 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__IndirectBrInst_O
  /* 104 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__FenceInst_O
  /* 105 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Constant_O
  /* 106 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__BlockAddress_O
  /* 107 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__GlobalValue_O
  /* 108 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__GlobalVariable_O
  /* 109 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Function_O
  /* 110 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantArray_O
  /* 111 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantInt_O
  /* 112 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataSequential_O
  /* 113 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataArray_O
  /* 114 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantStruct_O
  /* 115 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantFP_O
  /* 116 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__UndefValue_O
  /* 117 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantPointerNull_O
  /* 118 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantExpr_O
  /* 119 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__BasicBlock_O
  /* 120 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__IRBuilderBase_O
  /* 121 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__IRBuilder_O
  /* 122 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIBuilder_O
  /* 123 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Metadata_O
  /* 124 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ValueAsMetadata_O
  /* 125 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__MDNode_O
  /* 126 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__MDString_O
  /* 127 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ExecutionEngine_O
  /* 128 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__APFloat_O
  /* 129 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBuilder_O
  /* 130 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DataLayout_O
  /* 131 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Triple_O
  /* 132 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__APInt_O
  /* 133 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBase_O
  /* 134 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionPassManager_O
  /* 135 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__PassManager_O
  /* 136 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__TargetMachine_O
  /* 137 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O
  /* 138 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__TargetOptions_O
  /* 139 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Type_O
  /* 140 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__IntegerType_O
  /* 141 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__CompositeType_O
  /* 142 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__SequentialType_O
  /* 143 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__VectorType_O
  /* 144 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__PointerType_O
  /* 145 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ArrayType_O
  /* 146 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__StructType_O
  /* 147 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionType_O
  /* 148 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__NamedMDNode_O
  /* 149 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Linker_O
  /* 150 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Pass_O
  /* 151 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionPass_O
  /* 152 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ModulePass_O
  /* 153 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__ImmutablePass_O
  /* 154 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DataLayoutPass_O
  /* 155 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O
  /* 156 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O
  /* 157 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O
  /* 158 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Module_O
  /* 159 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__EngineBuilder_O
  /* 160 */ , &&obj_finalize_KIND_LISPALLOC_core__ForeignData_O
  /* 161 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__LLVMContext_O
  /* 162 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__Target_O
  /* 163 */ , &&obj_finalize_KIND_LISPALLOC_core__LoadTimeValues_O
  /* 164 */ , &&obj_finalize_KIND_LISPALLOC_core__Binder_O
  /* 165 */ , &&obj_finalize_KIND_LISPALLOC_core__IntArray_O
  /* 166 */ , &&obj_finalize_KIND_LISPALLOC_core__SourceManager_O
  /* 167 */ , &&obj_finalize_KIND_LISPALLOC_core__Record_O
  /* 168 */ , &&obj_finalize_KIND_LISPALLOC_core__LightUserData_O
  /* 169 */ , &&obj_finalize_KIND_LISPALLOC_core__UserData_O
  /* 170 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Symbol_O
  /* 171 */ , &&obj_finalize_KIND_LISPALLOC_core__Null_O
  /* 172 */ , &&obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 173 */ , &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__Iterator_O
  /* 174 */ , &&obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 175 */ , &&obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 176 */ , &&obj_finalize_KIND_LISPALLOC_core__Regex_O
  /* 177 */ , &&obj_finalize_KIND_LISPALLOC_core__PosixTimeDuration_O
  /* 178 */ , &&obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 179 */ , &&obj_finalize_KIND_LISPALLOC_core__CandoException_O
  /* 180 */ , &&obj_finalize_KIND_LISPALLOC_core__Stream_O
  /* 181 */ , &&obj_finalize_KIND_LISPALLOC_core__AnsiStream_O
  /* 182 */ , &&obj_finalize_KIND_LISPALLOC_core__FileStream_O
  /* 183 */ , &&obj_finalize_KIND_LISPALLOC_core__IOStreamStream_O
  /* 184 */ , &&obj_finalize_KIND_LISPALLOC_core__IOFileStream_O
  /* 185 */ , &&obj_finalize_KIND_LISPALLOC_core__ConcatenatedStream_O
  /* 186 */ , &&obj_finalize_KIND_LISPALLOC_core__StringStream_O
  /* 187 */ , &&obj_finalize_KIND_LISPALLOC_core__StringInputStream_O
  /* 188 */ , &&obj_finalize_KIND_LISPALLOC_core__StringOutputStream_O
  /* 189 */ , &&obj_finalize_KIND_LISPALLOC_core__SynonymStream_O
  /* 190 */ , &&obj_finalize_KIND_LISPALLOC_core__EchoStream_O
  /* 191 */ , &&obj_finalize_KIND_LISPALLOC_core__TwoWayStream_O
  /* 192 */ , &&obj_finalize_KIND_LISPALLOC_core__BroadcastStream_O
  /* 193 */ , &&obj_finalize_KIND_LISPALLOC_core__Reader_O
  /* 194 */ , &&obj_finalize_KIND_LISPALLOC_core__Cons_O
  /* 195 */ , &&obj_finalize_KIND_LISPALLOC_core__Archive_O
  /* 196 */ , &&obj_finalize_KIND_LISPALLOC_core__SaveArchive_O
  /* 197 */ , &&obj_finalize_KIND_LISPALLOC_core__SexpSaveArchive_O
  /* 198 */ , &&obj_finalize_KIND_LISPALLOC_core__LoadArchive_O
  /* 199 */ , &&obj_finalize_KIND_LISPALLOC_core__SexpLoadArchive_O
  /* 200 */ , &&obj_finalize_KIND_LISPALLOC_core__HashTable_O
  /* 201 */ , &&obj_finalize_KIND_LISPALLOC_core__HashTableEq_O
  /* 202 */ , &&obj_finalize_KIND_LISPALLOC_core__HashTableEqualp_O
  /* 203 */ , &&obj_finalize_KIND_LISPALLOC_core__HashTableEql_O
  /* 204 */ , &&obj_finalize_KIND_LISPALLOC_core__HashTableEqual_O
  /* 205 */ , &&obj_finalize_KIND_LISPALLOC_cffi__Pointer_O
  /* 206 */ , &&obj_finalize_KIND_LISPALLOC_core__CxxObject_O
  /* 207 */ , &&obj_finalize_KIND_LISPALLOC_core__WeakKeyMapping_O
  /* 208 */ , &&obj_finalize_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 209 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__InsertPoint_O
  /* 210 */ , &&obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 211 */ , &&obj_finalize_KIND_LISPALLOC_core__SNode_O
  /* 212 */ , &&obj_finalize_KIND_LISPALLOC_core__LeafSNode_O
  /* 213 */ , &&obj_finalize_KIND_LISPALLOC_core__BranchSNode_O
  /* 214 */ , &&obj_finalize_KIND_LISPALLOC_core__Path_O
  /* 215 */ , &&obj_finalize_KIND_LISPALLOC_asttooling__AstVisitor_O
  /* 216 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__AttributeSet_O
  /* 217 */ , &&obj_finalize_KIND_LISPALLOC_core__StructureObject_O
  /* 218 */ , &&obj_finalize_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 219 */ , &&obj_finalize_KIND_LISPALLOC_core__Package_O
  /* 220 */ , &&obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 221 */ , &&obj_finalize_KIND_LISPALLOC_core__Character_dummy_O
  /* 222 */ , &&obj_finalize_KIND_LISPALLOC_core__Function_O
  /* 223 */ , &&obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O
  /* 224 */ , &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O
  /* 225 */ , &&obj_finalize_KIND_LISPALLOC_core__SpecialForm_O
  /* 226 */ , &&obj_finalize_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O
  /* 227 */ , &&obj_finalize_KIND_LISPALLOC_core__Instance_O
  /* 228 */ , &&obj_finalize_KIND_LISPALLOC_core__Pointer_O
  /* 229 */ , &&obj_finalize_KIND_LISPALLOC_clbind__ClassRegistry_O
  /* 230 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DebugInfo_O
  /* 231 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIDerivedType_O
  /* 232 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIArray_O
  /* 233 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIBasicType_O
  /* 234 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DISubprogram_O
  /* 235 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DILexicalBlock_O
  /* 236 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DICompileUnit_O
  /* 237 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIDescriptor_O
  /* 238 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIType_O
  /* 239 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DISubroutineType_O
  /* 240 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DICompositeType_O
  /* 241 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DITypeArray_O
  /* 242 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIFile_O
  /* 243 */ , &&obj_finalize_KIND_LISPALLOC_llvmo__DIScope_O
  /* 244 */ , &&obj_finalize_KIND_LISPALLOC_core__SmallMultimap_O
  /* 245 */ , &&obj_finalize_KIND_LISPALLOC_core__Pathname_O
  /* 246 */ , &&obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O
  /* 247 */ , &&obj_finalize_KIND_LISPALLOC_core__PosixTime_O
  /* 248 */ , &&obj_finalize_KIND_LISPALLOC_core__SmallMap_O
  /* 249 */ , &&obj_finalize_KIND_CLASSALLOC_core__Cache
  /* 250 */ , &&obj_finalize_KIND_ROOTCLASSALLOC_core__Lisp_O
  /* 251 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 252 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 253 */ , &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 254 */ , &&obj_finalize_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure
  /* 255 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 256 */ , &&obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 257 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 258 */ , &&obj_finalize_KIND_CLASSALLOC_llvmo__CompiledClosure
  /* 259 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 260 */ , &&obj_finalize_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor
  /* 261 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___
  /* 262 */ , &&obj_finalize_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor
  /* 263 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_
  /* 264 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_
  /* 265 */ , &&obj_finalize_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor
  /* 266 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_
  /* 267 */ , &&obj_finalize_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor
  /* 268 */ , &&obj_finalize_KIND_CLASSALLOC_core__MacroClosure
  /* 269 */ , &&obj_finalize_KIND_CLASSALLOC_core__ConsStepper
  /* 270 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 271 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_
  /* 272 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 273 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 274 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_
  /* 275 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___
  /* 276 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 277 */ , &&obj_finalize_KIND_CLASSALLOC_core__InstanceClosure
  /* 278 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 279 */ , &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory
  /* 280 */ , &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_
  /* 281 */ , &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableMatchCallback
  /* 282 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_
  /* 283 */ , &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction
  /* 284 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_
  /* 285 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__
  /* 286 */ , &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_
  /* 287 */ , &&obj_finalize_KIND_CLASSALLOC_core__CoreExposer
  /* 288 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 289 */ , &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction
  /* 290 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__
  /* 291 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 292 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 293 */ , &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 294 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 295 */ , &&obj_finalize_KIND_CLASSALLOC_core__InterpretedClosure
  /* 296 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 297 */ , &&obj_finalize_KIND_CLASSALLOC_core__VectorStepper
  /* 298 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_OBJ_DEALLOCATOR)
obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps:
{
    asttooling::RegMap::RegistryMaps* obj_gc_safe = reinterpret_cast<asttooling::RegMap::RegistryMaps*>(client);
    GCObjectAllocator<asttooling::RegMap::RegistryMaps>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_clbind__detail__class_map:
{
    clbind::detail::class_map* obj_gc_safe = reinterpret_cast<clbind::detail::class_map*>(client);
    GCObjectAllocator<clbind::detail::class_map>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    GCObjectAllocator<core::Creator>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_clbind__DummyCreator:
{
    clbind::DummyCreator* obj_gc_safe = reinterpret_cast<clbind::DummyCreator*>(client);
    GCObjectAllocator<clbind::DummyCreator>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__InstanceCreator:
{
    core::InstanceCreator* obj_gc_safe = reinterpret_cast<core::InstanceCreator*>(client);
    GCObjectAllocator<core::InstanceCreator>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    GCObjectAllocator<clbind::ConstructorCreator>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__T_O:
{
    core::T_O* obj_gc_safe = reinterpret_cast<core::T_O*>(client);
    GCObjectAllocator<core::T_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MultiStringBuffer_O:
{
    core::MultiStringBuffer_O* obj_gc_safe = reinterpret_cast<core::MultiStringBuffer_O*>(client);
    GCObjectAllocator<core::MultiStringBuffer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ReadTable_O:
{
    core::ReadTable_O* obj_gc_safe = reinterpret_cast<core::ReadTable_O*>(client);
    GCObjectAllocator<core::ReadTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    GCObjectAllocator<core::Number_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    GCObjectAllocator<core::Complex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    GCObjectAllocator<core::Real_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    GCObjectAllocator<core::Rational_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    GCObjectAllocator<core::Integer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    GCObjectAllocator<core::Bignum_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    GCObjectAllocator<core::Fixnum_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    GCObjectAllocator<core::Ratio_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    GCObjectAllocator<core::Float_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    GCObjectAllocator<core::DoubleFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    GCObjectAllocator<core::LongFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    GCObjectAllocator<core::SingleFloat_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    GCObjectAllocator<core::ShortFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    GCObjectAllocator<core::FileStatus_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    GCObjectAllocator<core::WeakHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    GCObjectAllocator<core::WeakKeyHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    GCObjectAllocator<core::Environment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ActivationFrame_O:
{
    core::ActivationFrame_O* obj_gc_safe = reinterpret_cast<core::ActivationFrame_O*>(client);
    GCObjectAllocator<core::ActivationFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    GCObjectAllocator<core::TagbodyFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    GCObjectAllocator<core::ValueFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    GCObjectAllocator<core::FunctionFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    GCObjectAllocator<core::LexicalEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    GCObjectAllocator<core::RuntimeVisibleEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    GCObjectAllocator<core::FunctionValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    GCObjectAllocator<core::ValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    GCObjectAllocator<core::TagbodyEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    GCObjectAllocator<core::CompileTimeEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    GCObjectAllocator<core::UnwindProtectEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    GCObjectAllocator<core::SymbolMacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    GCObjectAllocator<core::FunctionContainerEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    GCObjectAllocator<core::StackValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    GCObjectAllocator<core::BlockEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    GCObjectAllocator<core::MacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    GCObjectAllocator<core::CatchEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    GCObjectAllocator<core::GlueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    GCObjectAllocator<core::Array_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayObjects_O:
{
    core::ArrayObjects_O* obj_gc_safe = reinterpret_cast<core::ArrayObjects_O*>(client);
    GCObjectAllocator<core::ArrayObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayDisplaced_O:
{
    core::ArrayDisplaced_O* obj_gc_safe = reinterpret_cast<core::ArrayDisplaced_O*>(client);
    GCObjectAllocator<core::ArrayDisplaced_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    GCObjectAllocator<core::Vector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVector_O:
{
    core::BitVector_O* obj_gc_safe = reinterpret_cast<core::BitVector_O*>(client);
    GCObjectAllocator<core::BitVector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SimpleBitVector_O:
{
    core::SimpleBitVector_O* obj_gc_safe = reinterpret_cast<core::SimpleBitVector_O*>(client);
    GCObjectAllocator<core::SimpleBitVector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVectorWithFillPtr_O:
{
    core::BitVectorWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::BitVectorWithFillPtr_O*>(client);
    GCObjectAllocator<core::BitVectorWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorDisplaced_O:
{
    core::VectorDisplaced_O* obj_gc_safe = reinterpret_cast<core::VectorDisplaced_O*>(client);
    GCObjectAllocator<core::VectorDisplaced_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    GCObjectAllocator<core::String_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    GCObjectAllocator<core::Str_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StrWithFillPtr_O:
{
    core::StrWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::StrWithFillPtr_O*>(client);
    GCObjectAllocator<core::StrWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    GCObjectAllocator<core::VectorObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    GCObjectAllocator<core::VectorObjectsWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchMethod_O:
{
    core::SingleDispatchMethod_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchMethod_O*>(client);
    GCObjectAllocator<core::SingleDispatchMethod_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RandomState_O:
{
    core::RandomState_O* obj_gc_safe = reinterpret_cast<core::RandomState_O*>(client);
    GCObjectAllocator<core::RandomState_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    GCObjectAllocator<core::WrappedPointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugLoc_O:
{
    llvmo::DebugLoc_O* obj_gc_safe = reinterpret_cast<llvmo::DebugLoc_O*>(client);
    GCObjectAllocator<llvmo::DebugLoc_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Attribute_O:
{
    llvmo::Attribute_O* obj_gc_safe = reinterpret_cast<llvmo::Attribute_O*>(client);
    GCObjectAllocator<llvmo::Attribute_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RegexMatch_O:
{
    core::RegexMatch_O* obj_gc_safe = reinterpret_cast<core::RegexMatch_O*>(client);
    GCObjectAllocator<core::RegexMatch_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakPointer_O:
{
    core::WeakPointer_O* obj_gc_safe = reinterpret_cast<core::WeakPointer_O*>(client);
    GCObjectAllocator<core::WeakPointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VaList_dummy_O:
{
    core::VaList_dummy_O* obj_gc_safe = reinterpret_cast<core::VaList_dummy_O*>(client);
    GCObjectAllocator<core::VaList_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    GCObjectAllocator<core::StandardObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    GCObjectAllocator<core::Metaobject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    GCObjectAllocator<core::Specializer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    GCObjectAllocator<core::Class_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StdClass_O:
{
    core::StdClass_O* obj_gc_safe = reinterpret_cast<core::StdClass_O*>(client);
    GCObjectAllocator<core::StdClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardClass_O:
{
    core::StandardClass_O* obj_gc_safe = reinterpret_cast<core::StandardClass_O*>(client);
    GCObjectAllocator<core::StandardClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FuncallableStandardClass_O:
{
    core::FuncallableStandardClass_O* obj_gc_safe = reinterpret_cast<core::FuncallableStandardClass_O*>(client);
    GCObjectAllocator<core::FuncallableStandardClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StructureClass_O:
{
    core::StructureClass_O* obj_gc_safe = reinterpret_cast<core::StructureClass_O*>(client);
    GCObjectAllocator<core::StructureClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForwardReferencedClass_O:
{
    core::ForwardReferencedClass_O* obj_gc_safe = reinterpret_cast<core::ForwardReferencedClass_O*>(client);
    GCObjectAllocator<core::ForwardReferencedClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxClass_O:
{
    core::CxxClass_O* obj_gc_safe = reinterpret_cast<core::CxxClass_O*>(client);
    GCObjectAllocator<core::CxxClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    core::BuiltInClass_O* obj_gc_safe = reinterpret_cast<core::BuiltInClass_O*>(client);
    GCObjectAllocator<core::BuiltInClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRep_O:
{
    clbind::ClassRep_O* obj_gc_safe = reinterpret_cast<clbind::ClassRep_O*>(client);
    GCObjectAllocator<clbind::ClassRep_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ExternalObject_O:
{
    core::ExternalObject_O* obj_gc_safe = reinterpret_cast<core::ExternalObject_O*>(client);
    GCObjectAllocator<core::ExternalObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Value_O:
{
    llvmo::Value_O* obj_gc_safe = reinterpret_cast<llvmo::Value_O*>(client);
    GCObjectAllocator<llvmo::Value_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Argument_O:
{
    llvmo::Argument_O* obj_gc_safe = reinterpret_cast<llvmo::Argument_O*>(client);
    GCObjectAllocator<llvmo::Argument_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__User_O:
{
    llvmo::User_O* obj_gc_safe = reinterpret_cast<llvmo::User_O*>(client);
    GCObjectAllocator<llvmo::User_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Instruction_O:
{
    llvmo::Instruction_O* obj_gc_safe = reinterpret_cast<llvmo::Instruction_O*>(client);
    GCObjectAllocator<llvmo::Instruction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    llvmo::AtomicRMWInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicRMWInst_O*>(client);
    GCObjectAllocator<llvmo::AtomicRMWInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    llvmo::LandingPadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LandingPadInst_O*>(client);
    GCObjectAllocator<llvmo::LandingPadInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PHINode_O:
{
    llvmo::PHINode_O* obj_gc_safe = reinterpret_cast<llvmo::PHINode_O*>(client);
    GCObjectAllocator<llvmo::PHINode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CallInst_O:
{
    llvmo::CallInst_O* obj_gc_safe = reinterpret_cast<llvmo::CallInst_O*>(client);
    GCObjectAllocator<llvmo::CallInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    llvmo::StoreInst_O* obj_gc_safe = reinterpret_cast<llvmo::StoreInst_O*>(client);
    GCObjectAllocator<llvmo::StoreInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    llvmo::UnaryInstruction_O* obj_gc_safe = reinterpret_cast<llvmo::UnaryInstruction_O*>(client);
    GCObjectAllocator<llvmo::UnaryInstruction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    llvmo::LoadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LoadInst_O*>(client);
    GCObjectAllocator<llvmo::LoadInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    llvmo::AllocaInst_O* obj_gc_safe = reinterpret_cast<llvmo::AllocaInst_O*>(client);
    GCObjectAllocator<llvmo::AllocaInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    llvmo::VAArgInst_O* obj_gc_safe = reinterpret_cast<llvmo::VAArgInst_O*>(client);
    GCObjectAllocator<llvmo::VAArgInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    llvmo::AtomicCmpXchgInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicCmpXchgInst_O*>(client);
    GCObjectAllocator<llvmo::AtomicCmpXchgInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    llvmo::TerminatorInst_O* obj_gc_safe = reinterpret_cast<llvmo::TerminatorInst_O*>(client);
    GCObjectAllocator<llvmo::TerminatorInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    llvmo::UnreachableInst_O* obj_gc_safe = reinterpret_cast<llvmo::UnreachableInst_O*>(client);
    GCObjectAllocator<llvmo::UnreachableInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    llvmo::SwitchInst_O* obj_gc_safe = reinterpret_cast<llvmo::SwitchInst_O*>(client);
    GCObjectAllocator<llvmo::SwitchInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    llvmo::ReturnInst_O* obj_gc_safe = reinterpret_cast<llvmo::ReturnInst_O*>(client);
    GCObjectAllocator<llvmo::ReturnInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    llvmo::ResumeInst_O* obj_gc_safe = reinterpret_cast<llvmo::ResumeInst_O*>(client);
    GCObjectAllocator<llvmo::ResumeInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    llvmo::BranchInst_O* obj_gc_safe = reinterpret_cast<llvmo::BranchInst_O*>(client);
    GCObjectAllocator<llvmo::BranchInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    llvmo::InvokeInst_O* obj_gc_safe = reinterpret_cast<llvmo::InvokeInst_O*>(client);
    GCObjectAllocator<llvmo::InvokeInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    llvmo::IndirectBrInst_O* obj_gc_safe = reinterpret_cast<llvmo::IndirectBrInst_O*>(client);
    GCObjectAllocator<llvmo::IndirectBrInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    llvmo::FenceInst_O* obj_gc_safe = reinterpret_cast<llvmo::FenceInst_O*>(client);
    GCObjectAllocator<llvmo::FenceInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Constant_O:
{
    llvmo::Constant_O* obj_gc_safe = reinterpret_cast<llvmo::Constant_O*>(client);
    GCObjectAllocator<llvmo::Constant_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    llvmo::BlockAddress_O* obj_gc_safe = reinterpret_cast<llvmo::BlockAddress_O*>(client);
    GCObjectAllocator<llvmo::BlockAddress_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    llvmo::GlobalValue_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalValue_O*>(client);
    GCObjectAllocator<llvmo::GlobalValue_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    llvmo::GlobalVariable_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalVariable_O*>(client);
    GCObjectAllocator<llvmo::GlobalVariable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Function_O:
{
    llvmo::Function_O* obj_gc_safe = reinterpret_cast<llvmo::Function_O*>(client);
    GCObjectAllocator<llvmo::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    llvmo::ConstantArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantArray_O*>(client);
    GCObjectAllocator<llvmo::ConstantArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    llvmo::ConstantInt_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantInt_O*>(client);
    GCObjectAllocator<llvmo::ConstantInt_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    llvmo::ConstantDataSequential_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataSequential_O*>(client);
    GCObjectAllocator<llvmo::ConstantDataSequential_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    llvmo::ConstantDataArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataArray_O*>(client);
    GCObjectAllocator<llvmo::ConstantDataArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    llvmo::ConstantStruct_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantStruct_O*>(client);
    GCObjectAllocator<llvmo::ConstantStruct_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    llvmo::ConstantFP_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantFP_O*>(client);
    GCObjectAllocator<llvmo::ConstantFP_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    llvmo::UndefValue_O* obj_gc_safe = reinterpret_cast<llvmo::UndefValue_O*>(client);
    GCObjectAllocator<llvmo::UndefValue_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    llvmo::ConstantPointerNull_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantPointerNull_O*>(client);
    GCObjectAllocator<llvmo::ConstantPointerNull_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    llvmo::ConstantExpr_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantExpr_O*>(client);
    GCObjectAllocator<llvmo::ConstantExpr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    llvmo::BasicBlock_O* obj_gc_safe = reinterpret_cast<llvmo::BasicBlock_O*>(client);
    GCObjectAllocator<llvmo::BasicBlock_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    llvmo::IRBuilderBase_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilderBase_O*>(client);
    GCObjectAllocator<llvmo::IRBuilderBase_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    llvmo::IRBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilder_O*>(client);
    GCObjectAllocator<llvmo::IRBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBuilder_O:
{
    llvmo::DIBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::DIBuilder_O*>(client);
    GCObjectAllocator<llvmo::DIBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Metadata_O:
{
    llvmo::Metadata_O* obj_gc_safe = reinterpret_cast<llvmo::Metadata_O*>(client);
    GCObjectAllocator<llvmo::Metadata_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    llvmo::ValueAsMetadata_O* obj_gc_safe = reinterpret_cast<llvmo::ValueAsMetadata_O*>(client);
    GCObjectAllocator<llvmo::ValueAsMetadata_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDNode_O:
{
    llvmo::MDNode_O* obj_gc_safe = reinterpret_cast<llvmo::MDNode_O*>(client);
    GCObjectAllocator<llvmo::MDNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDString_O:
{
    llvmo::MDString_O* obj_gc_safe = reinterpret_cast<llvmo::MDString_O*>(client);
    GCObjectAllocator<llvmo::MDString_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    llvmo::ExecutionEngine_O* obj_gc_safe = reinterpret_cast<llvmo::ExecutionEngine_O*>(client);
    GCObjectAllocator<llvmo::ExecutionEngine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APFloat_O:
{
    llvmo::APFloat_O* obj_gc_safe = reinterpret_cast<llvmo::APFloat_O*>(client);
    GCObjectAllocator<llvmo::APFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    llvmo::PassManagerBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBuilder_O*>(client);
    GCObjectAllocator<llvmo::PassManagerBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    llvmo::DataLayout_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayout_O*>(client);
    GCObjectAllocator<llvmo::DataLayout_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Triple_O:
{
    llvmo::Triple_O* obj_gc_safe = reinterpret_cast<llvmo::Triple_O*>(client);
    GCObjectAllocator<llvmo::Triple_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APInt_O:
{
    llvmo::APInt_O* obj_gc_safe = reinterpret_cast<llvmo::APInt_O*>(client);
    GCObjectAllocator<llvmo::APInt_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    llvmo::PassManagerBase_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBase_O*>(client);
    GCObjectAllocator<llvmo::PassManagerBase_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    llvmo::FunctionPassManager_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPassManager_O*>(client);
    GCObjectAllocator<llvmo::FunctionPassManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManager_O:
{
    llvmo::PassManager_O* obj_gc_safe = reinterpret_cast<llvmo::PassManager_O*>(client);
    GCObjectAllocator<llvmo::PassManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    llvmo::TargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::TargetMachine_O*>(client);
    GCObjectAllocator<llvmo::TargetMachine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    llvmo::LLVMTargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMTargetMachine_O*>(client);
    GCObjectAllocator<llvmo::LLVMTargetMachine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    llvmo::TargetOptions_O* obj_gc_safe = reinterpret_cast<llvmo::TargetOptions_O*>(client);
    GCObjectAllocator<llvmo::TargetOptions_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Type_O:
{
    llvmo::Type_O* obj_gc_safe = reinterpret_cast<llvmo::Type_O*>(client);
    GCObjectAllocator<llvmo::Type_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    llvmo::IntegerType_O* obj_gc_safe = reinterpret_cast<llvmo::IntegerType_O*>(client);
    GCObjectAllocator<llvmo::IntegerType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    llvmo::CompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::CompositeType_O*>(client);
    GCObjectAllocator<llvmo::CompositeType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    llvmo::SequentialType_O* obj_gc_safe = reinterpret_cast<llvmo::SequentialType_O*>(client);
    GCObjectAllocator<llvmo::SequentialType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VectorType_O:
{
    llvmo::VectorType_O* obj_gc_safe = reinterpret_cast<llvmo::VectorType_O*>(client);
    GCObjectAllocator<llvmo::VectorType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PointerType_O:
{
    llvmo::PointerType_O* obj_gc_safe = reinterpret_cast<llvmo::PointerType_O*>(client);
    GCObjectAllocator<llvmo::PointerType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    llvmo::ArrayType_O* obj_gc_safe = reinterpret_cast<llvmo::ArrayType_O*>(client);
    GCObjectAllocator<llvmo::ArrayType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StructType_O:
{
    llvmo::StructType_O* obj_gc_safe = reinterpret_cast<llvmo::StructType_O*>(client);
    GCObjectAllocator<llvmo::StructType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    llvmo::FunctionType_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionType_O*>(client);
    GCObjectAllocator<llvmo::FunctionType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    llvmo::NamedMDNode_O* obj_gc_safe = reinterpret_cast<llvmo::NamedMDNode_O*>(client);
    GCObjectAllocator<llvmo::NamedMDNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Linker_O:
{
    llvmo::Linker_O* obj_gc_safe = reinterpret_cast<llvmo::Linker_O*>(client);
    GCObjectAllocator<llvmo::Linker_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Pass_O:
{
    llvmo::Pass_O* obj_gc_safe = reinterpret_cast<llvmo::Pass_O*>(client);
    GCObjectAllocator<llvmo::Pass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    llvmo::FunctionPass_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPass_O*>(client);
    GCObjectAllocator<llvmo::FunctionPass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    llvmo::ModulePass_O* obj_gc_safe = reinterpret_cast<llvmo::ModulePass_O*>(client);
    GCObjectAllocator<llvmo::ModulePass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    llvmo::ImmutablePass_O* obj_gc_safe = reinterpret_cast<llvmo::ImmutablePass_O*>(client);
    GCObjectAllocator<llvmo::ImmutablePass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    llvmo::DataLayoutPass_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayoutPass_O*>(client);
    GCObjectAllocator<llvmo::DataLayoutPass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    llvmo::TargetLibraryInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetLibraryInfo_O*>(client);
    GCObjectAllocator<llvmo::TargetLibraryInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    llvmo::MCSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::MCSubtargetInfo_O*>(client);
    GCObjectAllocator<llvmo::MCSubtargetInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    llvmo::TargetSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetSubtargetInfo_O*>(client);
    GCObjectAllocator<llvmo::TargetSubtargetInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Module_O:
{
    llvmo::Module_O* obj_gc_safe = reinterpret_cast<llvmo::Module_O*>(client);
    GCObjectAllocator<llvmo::Module_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    llvmo::EngineBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::EngineBuilder_O*>(client);
    GCObjectAllocator<llvmo::EngineBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForeignData_O:
{
    core::ForeignData_O* obj_gc_safe = reinterpret_cast<core::ForeignData_O*>(client);
    GCObjectAllocator<core::ForeignData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    llvmo::LLVMContext_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMContext_O*>(client);
    GCObjectAllocator<llvmo::LLVMContext_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Target_O:
{
    llvmo::Target_O* obj_gc_safe = reinterpret_cast<llvmo::Target_O*>(client);
    GCObjectAllocator<llvmo::Target_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadTimeValues_O:
{
    core::LoadTimeValues_O* obj_gc_safe = reinterpret_cast<core::LoadTimeValues_O*>(client);
    GCObjectAllocator<core::LoadTimeValues_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Binder_O:
{
    core::Binder_O* obj_gc_safe = reinterpret_cast<core::Binder_O*>(client);
    GCObjectAllocator<core::Binder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IntArray_O:
{
    core::IntArray_O* obj_gc_safe = reinterpret_cast<core::IntArray_O*>(client);
    GCObjectAllocator<core::IntArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    GCObjectAllocator<core::SourceManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    GCObjectAllocator<core::Record_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LightUserData_O:
{
    core::LightUserData_O* obj_gc_safe = reinterpret_cast<core::LightUserData_O*>(client);
    GCObjectAllocator<core::LightUserData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UserData_O:
{
    core::UserData_O* obj_gc_safe = reinterpret_cast<core::UserData_O*>(client);
    GCObjectAllocator<core::UserData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    GCObjectAllocator<core::Symbol_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Null_O:
{
    core::Null_O* obj_gc_safe = reinterpret_cast<core::Null_O*>(client);
    GCObjectAllocator<core::Null_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    GCObjectAllocator<core::SourcePosInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    GCObjectAllocator<core::Iterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    GCObjectAllocator<core::DirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    GCObjectAllocator<core::RecursiveDirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Regex_O:
{
    core::Regex_O* obj_gc_safe = reinterpret_cast<core::Regex_O*>(client);
    GCObjectAllocator<core::Regex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTimeDuration_O:
{
    core::PosixTimeDuration_O* obj_gc_safe = reinterpret_cast<core::PosixTimeDuration_O*>(client);
    GCObjectAllocator<core::PosixTimeDuration_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    GCObjectAllocator<core::SymbolToEnumConverter_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    GCObjectAllocator<core::CandoException_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Stream_O:
{
    core::Stream_O* obj_gc_safe = reinterpret_cast<core::Stream_O*>(client);
    GCObjectAllocator<core::Stream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__AnsiStream_O:
{
    core::AnsiStream_O* obj_gc_safe = reinterpret_cast<core::AnsiStream_O*>(client);
    GCObjectAllocator<core::AnsiStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStream_O:
{
    core::FileStream_O* obj_gc_safe = reinterpret_cast<core::FileStream_O*>(client);
    GCObjectAllocator<core::FileStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOStreamStream_O:
{
    core::IOStreamStream_O* obj_gc_safe = reinterpret_cast<core::IOStreamStream_O*>(client);
    GCObjectAllocator<core::IOStreamStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOFileStream_O:
{
    core::IOFileStream_O* obj_gc_safe = reinterpret_cast<core::IOFileStream_O*>(client);
    GCObjectAllocator<core::IOFileStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ConcatenatedStream_O:
{
    core::ConcatenatedStream_O* obj_gc_safe = reinterpret_cast<core::ConcatenatedStream_O*>(client);
    GCObjectAllocator<core::ConcatenatedStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringStream_O:
{
    core::StringStream_O* obj_gc_safe = reinterpret_cast<core::StringStream_O*>(client);
    GCObjectAllocator<core::StringStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringInputStream_O:
{
    core::StringInputStream_O* obj_gc_safe = reinterpret_cast<core::StringInputStream_O*>(client);
    GCObjectAllocator<core::StringInputStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringOutputStream_O:
{
    core::StringOutputStream_O* obj_gc_safe = reinterpret_cast<core::StringOutputStream_O*>(client);
    GCObjectAllocator<core::StringOutputStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SynonymStream_O:
{
    core::SynonymStream_O* obj_gc_safe = reinterpret_cast<core::SynonymStream_O*>(client);
    GCObjectAllocator<core::SynonymStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__EchoStream_O:
{
    core::EchoStream_O* obj_gc_safe = reinterpret_cast<core::EchoStream_O*>(client);
    GCObjectAllocator<core::EchoStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TwoWayStream_O:
{
    core::TwoWayStream_O* obj_gc_safe = reinterpret_cast<core::TwoWayStream_O*>(client);
    GCObjectAllocator<core::TwoWayStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BroadcastStream_O:
{
    core::BroadcastStream_O* obj_gc_safe = reinterpret_cast<core::BroadcastStream_O*>(client);
    GCObjectAllocator<core::BroadcastStream_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Reader_O:
{
    core::Reader_O* obj_gc_safe = reinterpret_cast<core::Reader_O*>(client);
    GCObjectAllocator<core::Reader_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    GCObjectAllocator<core::Cons_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O:
{
    core::Archive_O* obj_gc_safe = reinterpret_cast<core::Archive_O*>(client);
    GCObjectAllocator<core::Archive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O:
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    GCObjectAllocator<core::SaveArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpSaveArchive_O:
{
    core::SexpSaveArchive_O* obj_gc_safe = reinterpret_cast<core::SexpSaveArchive_O*>(client);
    GCObjectAllocator<core::SexpSaveArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    GCObjectAllocator<core::LoadArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpLoadArchive_O:
{
    core::SexpLoadArchive_O* obj_gc_safe = reinterpret_cast<core::SexpLoadArchive_O*>(client);
    GCObjectAllocator<core::SexpLoadArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTable_O:
{
    core::HashTable_O* obj_gc_safe = reinterpret_cast<core::HashTable_O*>(client);
    GCObjectAllocator<core::HashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEq_O:
{
    core::HashTableEq_O* obj_gc_safe = reinterpret_cast<core::HashTableEq_O*>(client);
    GCObjectAllocator<core::HashTableEq_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    core::HashTableEqualp_O* obj_gc_safe = reinterpret_cast<core::HashTableEqualp_O*>(client);
    GCObjectAllocator<core::HashTableEqualp_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEql_O:
{
    core::HashTableEql_O* obj_gc_safe = reinterpret_cast<core::HashTableEql_O*>(client);
    GCObjectAllocator<core::HashTableEql_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O:
{
    core::HashTableEqual_O* obj_gc_safe = reinterpret_cast<core::HashTableEqual_O*>(client);
    GCObjectAllocator<core::HashTableEqual_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_cffi__Pointer_O:
{
    cffi::Pointer_O* obj_gc_safe = reinterpret_cast<cffi::Pointer_O*>(client);
    GCObjectAllocator<cffi::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    GCObjectAllocator<core::CxxObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyMapping_O:
{
    core::WeakKeyMapping_O* obj_gc_safe = reinterpret_cast<core::WeakKeyMapping_O*>(client);
    GCObjectAllocator<core::WeakKeyMapping_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    core::LambdaListHandler_O* obj_gc_safe = reinterpret_cast<core::LambdaListHandler_O*>(client);
    GCObjectAllocator<core::LambdaListHandler_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InsertPoint_O:
{
    llvmo::InsertPoint_O* obj_gc_safe = reinterpret_cast<llvmo::InsertPoint_O*>(client);
    GCObjectAllocator<llvmo::InsertPoint_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    GCObjectAllocator<core::SourceFileInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SNode_O:
{
    core::SNode_O* obj_gc_safe = reinterpret_cast<core::SNode_O*>(client);
    GCObjectAllocator<core::SNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LeafSNode_O:
{
    core::LeafSNode_O* obj_gc_safe = reinterpret_cast<core::LeafSNode_O*>(client);
    GCObjectAllocator<core::LeafSNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BranchSNode_O:
{
    core::BranchSNode_O* obj_gc_safe = reinterpret_cast<core::BranchSNode_O*>(client);
    GCObjectAllocator<core::BranchSNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    GCObjectAllocator<core::Path_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__AstVisitor_O:
{
    asttooling::AstVisitor_O* obj_gc_safe = reinterpret_cast<asttooling::AstVisitor_O*>(client);
    GCObjectAllocator<asttooling::AstVisitor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    llvmo::AttributeSet_O* obj_gc_safe = reinterpret_cast<llvmo::AttributeSet_O*>(client);
    GCObjectAllocator<llvmo::AttributeSet_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StructureObject_O:
{
    core::StructureObject_O* obj_gc_safe = reinterpret_cast<core::StructureObject_O*>(client);
    GCObjectAllocator<core::StructureObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    core::InvocationHistoryFrameIterator_O* obj_gc_safe = reinterpret_cast<core::InvocationHistoryFrameIterator_O*>(client);
    GCObjectAllocator<core::InvocationHistoryFrameIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Package_O:
{
    core::Package_O* obj_gc_safe = reinterpret_cast<core::Package_O*>(client);
    GCObjectAllocator<core::Package_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    GCObjectAllocator<core::DirectoryEntry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Character_dummy_O:
{
    core::Character_dummy_O* obj_gc_safe = reinterpret_cast<core::Character_dummy_O*>(client);
    GCObjectAllocator<core::Character_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    GCObjectAllocator<core::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    GCObjectAllocator<core::CompiledFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O:
{
    core::SingleDispatchGenericFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunction_O*>(client);
    GCObjectAllocator<core::SingleDispatchGenericFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SpecialForm_O:
{
    core::SpecialForm_O* obj_gc_safe = reinterpret_cast<core::SpecialForm_O*>(client);
    GCObjectAllocator<core::SpecialForm_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O:
{
    core::SingleDispatchEffectiveMethodFunction_O* obj_gc_safe = reinterpret_cast<core::SingleDispatchEffectiveMethodFunction_O*>(client);
    GCObjectAllocator<core::SingleDispatchEffectiveMethodFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    GCObjectAllocator<core::Instance_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    GCObjectAllocator<core::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    clbind::ClassRegistry_O* obj_gc_safe = reinterpret_cast<clbind::ClassRegistry_O*>(client);
    GCObjectAllocator<clbind::ClassRegistry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugInfo_O:
{
    llvmo::DebugInfo_O* obj_gc_safe = reinterpret_cast<llvmo::DebugInfo_O*>(client);
    GCObjectAllocator<llvmo::DebugInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDerivedType_O:
{
    llvmo::DIDerivedType_O* obj_gc_safe = reinterpret_cast<llvmo::DIDerivedType_O*>(client);
    GCObjectAllocator<llvmo::DIDerivedType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIArray_O:
{
    llvmo::DIArray_O* obj_gc_safe = reinterpret_cast<llvmo::DIArray_O*>(client);
    GCObjectAllocator<llvmo::DIArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBasicType_O:
{
    llvmo::DIBasicType_O* obj_gc_safe = reinterpret_cast<llvmo::DIBasicType_O*>(client);
    GCObjectAllocator<llvmo::DIBasicType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubprogram_O:
{
    llvmo::DISubprogram_O* obj_gc_safe = reinterpret_cast<llvmo::DISubprogram_O*>(client);
    GCObjectAllocator<llvmo::DISubprogram_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DILexicalBlock_O:
{
    llvmo::DILexicalBlock_O* obj_gc_safe = reinterpret_cast<llvmo::DILexicalBlock_O*>(client);
    GCObjectAllocator<llvmo::DILexicalBlock_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompileUnit_O:
{
    llvmo::DICompileUnit_O* obj_gc_safe = reinterpret_cast<llvmo::DICompileUnit_O*>(client);
    GCObjectAllocator<llvmo::DICompileUnit_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDescriptor_O:
{
    llvmo::DIDescriptor_O* obj_gc_safe = reinterpret_cast<llvmo::DIDescriptor_O*>(client);
    GCObjectAllocator<llvmo::DIDescriptor_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIType_O:
{
    llvmo::DIType_O* obj_gc_safe = reinterpret_cast<llvmo::DIType_O*>(client);
    GCObjectAllocator<llvmo::DIType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubroutineType_O:
{
    llvmo::DISubroutineType_O* obj_gc_safe = reinterpret_cast<llvmo::DISubroutineType_O*>(client);
    GCObjectAllocator<llvmo::DISubroutineType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompositeType_O:
{
    llvmo::DICompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::DICompositeType_O*>(client);
    GCObjectAllocator<llvmo::DICompositeType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DITypeArray_O:
{
    llvmo::DITypeArray_O* obj_gc_safe = reinterpret_cast<llvmo::DITypeArray_O*>(client);
    GCObjectAllocator<llvmo::DITypeArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIFile_O:
{
    llvmo::DIFile_O* obj_gc_safe = reinterpret_cast<llvmo::DIFile_O*>(client);
    GCObjectAllocator<llvmo::DIFile_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIScope_O:
{
    llvmo::DIScope_O* obj_gc_safe = reinterpret_cast<llvmo::DIScope_O*>(client);
    GCObjectAllocator<llvmo::DIScope_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMultimap_O:
{
    core::SmallMultimap_O* obj_gc_safe = reinterpret_cast<core::SmallMultimap_O*>(client);
    GCObjectAllocator<core::SmallMultimap_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    GCObjectAllocator<core::Pathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    GCObjectAllocator<core::LogicalPathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTime_O:
{
    core::PosixTime_O* obj_gc_safe = reinterpret_cast<core::PosixTime_O*>(client);
    GCObjectAllocator<core::PosixTime_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMap_O:
{
    core::SmallMap_O* obj_gc_safe = reinterpret_cast<core::SmallMap_O*>(client);
    GCObjectAllocator<core::SmallMap_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__Cache:
{
    core::Cache* obj_gc_safe = reinterpret_cast<core::Cache*>(client);
    GCObjectAllocator<core::Cache>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_core__Lisp_O:
{
    core::Lisp_O* obj_gc_safe = reinterpret_cast<core::Lisp_O*>(client);
    GCObjectAllocator<core::Lisp_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::KeywordArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure:
{
    core::SingleDispatchGenericFunctionClosure* obj_gc_safe = reinterpret_cast<core::SingleDispatchGenericFunctionClosure*>(client);
    GCObjectAllocator<core::SingleDispatchGenericFunctionClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never deallocate gcstrings gctools::GCString_moveable<char>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::RequiredArgument>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_llvmo__CompiledClosure:
{
    llvmo::CompiledClosure* obj_gc_safe = reinterpret_cast<llvmo::CompiledClosure*>(client);
    GCObjectAllocator<llvmo::CompiledClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor:
{
    asttooling::internal::VariadicOperatorMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::VariadicOperatorMatcherDescriptor*>(client);
    GCObjectAllocator<asttooling::internal::VariadicOperatorMatcherDescriptor>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor:
{
    asttooling::internal::OverloadedMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::OverloadedMatcherDescriptor*>(client);
    GCObjectAllocator<asttooling::internal::OverloadedMatcherDescriptor>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolStorage>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ContextFrame>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor:
{
    asttooling::internal::FixedArgCountMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::FixedArgCountMatcherDescriptor*>(client);
    GCObjectAllocator<asttooling::internal::FixedArgCountMatcherDescriptor>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::T_O *>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor:
{
    asttooling::internal::FreeFuncMatcherDescriptor* obj_gc_safe = reinterpret_cast<asttooling::internal::FreeFuncMatcherDescriptor*>(client);
    GCObjectAllocator<asttooling::internal::FreeFuncMatcherDescriptor>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__MacroClosure:
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    GCObjectAllocator<core::MacroClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__ConsStepper:
{
    core::ConsStepper* obj_gc_safe = reinterpret_cast<core::ConsStepper*>(client);
    GCObjectAllocator<core::ConsStepper>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::AuxArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ParserValue>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__InstanceClosure:
{
    core::InstanceClosure* obj_gc_safe = reinterpret_cast<core::InstanceClosure*>(client);
    GCObjectAllocator<core::InstanceClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    asttooling::DerivableFrontendActionFactory* obj_gc_safe = reinterpret_cast<asttooling::DerivableFrontendActionFactory*>(client);
    GCObjectAllocator<asttooling::DerivableFrontendActionFactory>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,1>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    asttooling::DerivableMatchCallback* obj_gc_safe = reinterpret_cast<asttooling::DerivableMatchCallback*>(client);
    GCObjectAllocator<asttooling::DerivableMatchCallback>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ErrorContent>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    asttooling::DerivableASTFrontendAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableASTFrontendAction*>(client);
    GCObjectAllocator<asttooling::DerivableASTFrontendAction>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::Message>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>>"));}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,2>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__CoreExposer:
{
    core::CoreExposer* obj_gc_safe = reinterpret_cast<core::CoreExposer*>(client);
    GCObjectAllocator<core::CoreExposer>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    asttooling::DerivableSyntaxOnlyAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableSyntaxOnlyAction*>(client);
    GCObjectAllocator<asttooling::DerivableSyntaxOnlyAction>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::OptionalArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    GCObjectAllocator<core::BuiltinClosure>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__InterpretedClosure:
{
    core::InterpretedClosure* obj_gc_safe = reinterpret_cast<core::InterpretedClosure*>(client);
    GCObjectAllocator<core::InterpretedClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__VectorStepper:
{
    core::VectorStepper* obj_gc_safe = reinterpret_cast<core::VectorStepper*>(client);
    GCObjectAllocator<core::VectorStepper>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>"));}
#endif // defined(GC_OBJ_DEALLOCATOR)
#if defined(GC_OBJ_DEALLOCATOR_HELPERS)

#endif // defined(GC_OBJ_DEALLOCATOR_HELPERS)
#if defined(GC_OBJ_DEALLOCATOR_TABLE)
static void* OBJ_DEALLOCATOR_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_asttooling__RegMap__RegistryMaps
  /* 5 */ , &&obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_clbind__detail__class_map
  /* 6 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 7 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_clbind__DummyCreator
  /* 8 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__InstanceCreator
  /* 9 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator
  /* 10 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__T_O
  /* 11 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MultiStringBuffer_O
  /* 12 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ReadTable_O
  /* 13 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O
  /* 14 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O
  /* 15 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O
  /* 16 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O
  /* 17 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O
  /* 18 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O
  /* 19 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 20 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O
  /* 21 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O
  /* 22 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O
  /* 23 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O
  /* 24 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 25 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O
  /* 26 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O
  /* 27 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O
  /* 28 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 29 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O
  /* 30 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ActivationFrame_O
  /* 31 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 32 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O
  /* 33 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O
  /* 34 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 35 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 36 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 37 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 38 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 39 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 40 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 41 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 42 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 43 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 44 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 45 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 46 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 47 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 48 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O
  /* 49 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayObjects_O
  /* 50 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ArrayDisplaced_O
  /* 51 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O
  /* 52 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVector_O
  /* 53 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SimpleBitVector_O
  /* 54 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVectorWithFillPtr_O
  /* 55 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorDisplaced_O
  /* 56 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O
  /* 57 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O
  /* 58 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StrWithFillPtr_O
  /* 59 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O
  /* 60 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 61 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchMethod_O
  /* 62 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RandomState_O
  /* 63 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O
  /* 64 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugLoc_O
  /* 65 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Attribute_O
  /* 66 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RegexMatch_O
  /* 67 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakPointer_O
  /* 68 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VaList_dummy_O
  /* 69 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O
  /* 70 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O
  /* 71 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O
  /* 72 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O
  /* 73 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StdClass_O
  /* 74 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardClass_O
  /* 75 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FuncallableStandardClass_O
  /* 76 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StructureClass_O
  /* 77 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForwardReferencedClass_O
  /* 78 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxClass_O
  /* 79 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__BuiltInClass_O
  /* 80 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRep_O
  /* 81 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ExternalObject_O
  /* 82 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Value_O
  /* 83 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Argument_O
  /* 84 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__User_O
  /* 85 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Instruction_O
  /* 86 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicRMWInst_O
  /* 87 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LandingPadInst_O
  /* 88 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PHINode_O
  /* 89 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CallInst_O
  /* 90 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StoreInst_O
  /* 91 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnaryInstruction_O
  /* 92 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LoadInst_O
  /* 93 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AllocaInst_O
  /* 94 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VAArgInst_O
  /* 95 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O
  /* 96 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TerminatorInst_O
  /* 97 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnreachableInst_O
  /* 98 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SwitchInst_O
  /* 99 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ReturnInst_O
  /* 100 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ResumeInst_O
  /* 101 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BranchInst_O
  /* 102 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InvokeInst_O
  /* 103 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IndirectBrInst_O
  /* 104 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FenceInst_O
  /* 105 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Constant_O
  /* 106 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BlockAddress_O
  /* 107 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalValue_O
  /* 108 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalVariable_O
  /* 109 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Function_O
  /* 110 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantArray_O
  /* 111 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantInt_O
  /* 112 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataSequential_O
  /* 113 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataArray_O
  /* 114 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantStruct_O
  /* 115 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantFP_O
  /* 116 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UndefValue_O
  /* 117 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantPointerNull_O
  /* 118 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantExpr_O
  /* 119 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BasicBlock_O
  /* 120 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilderBase_O
  /* 121 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilder_O
  /* 122 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBuilder_O
  /* 123 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Metadata_O
  /* 124 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ValueAsMetadata_O
  /* 125 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDNode_O
  /* 126 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDString_O
  /* 127 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ExecutionEngine_O
  /* 128 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APFloat_O
  /* 129 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBuilder_O
  /* 130 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayout_O
  /* 131 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Triple_O
  /* 132 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APInt_O
  /* 133 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBase_O
  /* 134 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPassManager_O
  /* 135 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManager_O
  /* 136 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetMachine_O
  /* 137 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O
  /* 138 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetOptions_O
  /* 139 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Type_O
  /* 140 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IntegerType_O
  /* 141 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CompositeType_O
  /* 142 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SequentialType_O
  /* 143 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VectorType_O
  /* 144 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PointerType_O
  /* 145 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ArrayType_O
  /* 146 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StructType_O
  /* 147 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionType_O
  /* 148 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__NamedMDNode_O
  /* 149 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Linker_O
  /* 150 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Pass_O
  /* 151 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPass_O
  /* 152 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ModulePass_O
  /* 153 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ImmutablePass_O
  /* 154 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayoutPass_O
  /* 155 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O
  /* 156 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O
  /* 157 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O
  /* 158 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Module_O
  /* 159 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__EngineBuilder_O
  /* 160 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForeignData_O
  /* 161 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMContext_O
  /* 162 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Target_O
  /* 163 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadTimeValues_O
  /* 164 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Binder_O
  /* 165 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IntArray_O
  /* 166 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O
  /* 167 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O
  /* 168 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LightUserData_O
  /* 169 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UserData_O
  /* 170 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O
  /* 171 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Null_O
  /* 172 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 173 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__Iterator_O
  /* 174 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 175 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 176 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Regex_O
  /* 177 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTimeDuration_O
  /* 178 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 179 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O
  /* 180 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Stream_O
  /* 181 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__AnsiStream_O
  /* 182 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStream_O
  /* 183 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOStreamStream_O
  /* 184 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__IOFileStream_O
  /* 185 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ConcatenatedStream_O
  /* 186 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringStream_O
  /* 187 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringInputStream_O
  /* 188 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StringOutputStream_O
  /* 189 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SynonymStream_O
  /* 190 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__EchoStream_O
  /* 191 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TwoWayStream_O
  /* 192 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BroadcastStream_O
  /* 193 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Reader_O
  /* 194 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O
  /* 195 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O
  /* 196 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O
  /* 197 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpSaveArchive_O
  /* 198 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O
  /* 199 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SexpLoadArchive_O
  /* 200 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTable_O
  /* 201 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEq_O
  /* 202 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqualp_O
  /* 203 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEql_O
  /* 204 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O
  /* 205 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_cffi__Pointer_O
  /* 206 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O
  /* 207 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyMapping_O
  /* 208 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 209 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InsertPoint_O
  /* 210 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 211 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SNode_O
  /* 212 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LeafSNode_O
  /* 213 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BranchSNode_O
  /* 214 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O
  /* 215 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__AstVisitor_O
  /* 216 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AttributeSet_O
  /* 217 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StructureObject_O
  /* 218 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 219 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Package_O
  /* 220 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 221 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Character_dummy_O
  /* 222 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O
  /* 223 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O
  /* 224 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchGenericFunction_O
  /* 225 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SpecialForm_O
  /* 226 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O
  /* 227 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O
  /* 228 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O
  /* 229 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRegistry_O
  /* 230 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DebugInfo_O
  /* 231 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDerivedType_O
  /* 232 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIArray_O
  /* 233 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIBasicType_O
  /* 234 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubprogram_O
  /* 235 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DILexicalBlock_O
  /* 236 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompileUnit_O
  /* 237 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIDescriptor_O
  /* 238 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIType_O
  /* 239 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DISubroutineType_O
  /* 240 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DICompositeType_O
  /* 241 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DITypeArray_O
  /* 242 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIFile_O
  /* 243 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DIScope_O
  /* 244 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMultimap_O
  /* 245 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O
  /* 246 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O
  /* 247 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__PosixTime_O
  /* 248 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SmallMap_O
  /* 249 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__Cache
  /* 250 */ , &&obj_deallocate_unmanaged_instance_KIND_ROOTCLASSALLOC_core__Lisp_O
  /* 251 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 252 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 253 */ , &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 254 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__SingleDispatchGenericFunctionClosure
  /* 255 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 256 */ , &&obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 257 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 258 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_llvmo__CompiledClosure
  /* 259 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 260 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor
  /* 261 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___
  /* 262 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__OverloadedMatcherDescriptor
  /* 263 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_
  /* 264 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_
  /* 265 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__FixedArgCountMatcherDescriptor
  /* 266 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_
  /* 267 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_asttooling__internal__FreeFuncMatcherDescriptor
  /* 268 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__MacroClosure
  /* 269 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__ConsStepper
  /* 270 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 271 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_
  /* 272 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 273 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 274 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_
  /* 275 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___
  /* 276 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 277 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__InstanceClosure
  /* 278 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 279 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory
  /* 280 */ , &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__1_
  /* 281 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableMatchCallback
  /* 282 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_
  /* 283 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction
  /* 284 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_
  /* 285 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_asttooling__internal__MatcherDescriptor__
  /* 286 */ , &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__2_
  /* 287 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__CoreExposer
  /* 288 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 289 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction
  /* 290 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__
  /* 291 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 292 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 293 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 294 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 295 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__InterpretedClosure
  /* 296 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 297 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__VectorStepper
  /* 298 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__
};
#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)
#if defined(GC_GLOBALS)
 SMART_PTR_FIX(llvmo::IRBuilder_O::static_class);
 TAGGED_POINTER_FIX(core::Iterator_O::static_creator);
 SMART_PTR_FIX(llvmo::DIArray_O::static_class);
 TAGGED_POINTER_FIX(core::AnsiStream_O::static_creator);
 SMART_PTR_FIX(core::Array_O::static_class);
 TAGGED_POINTER_FIX(core::Stream_O::static_creator);
 SMART_PTR_FIX(llvmo::DIDescriptor_O::static_class);
 TAGGED_POINTER_FIX(core::SmallMultimap_O::static_creator);
 SMART_PTR_FIX(core::FileStatus_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DISubprogram_O::static_creator);
 SMART_PTR_FIX(core::HashTableEq_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::PosixTimeDuration_O::static_creator);
 SMART_PTR_FIX(llvmo::DITypeArray_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::PassManagerBase_O::static_class);
 TAGGED_POINTER_FIX(core::StdClass_O::static_creator);
 TAGGED_POINTER_FIX(core::Binder_O::static_creator);
 SMART_PTR_FIX(core::RegexMatch_O::static_class);
 SMART_PTR_FIX(core::IOFileStream_O::static_class);
 SMART_PTR_FIX(llvmo::ValueAsMetadata_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DIDescriptor_O::static_creator);
 SMART_PTR_FIX(llvmo::SequentialType_O::static_class);
 TAGGED_POINTER_FIX(core::HashTable_O::static_creator);
 SMART_PTR_FIX(core::LambdaListHandler_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::BranchInst_O::static_class);
 TAGGED_POINTER_FIX(core::StructureClass_O::static_creator);
 SMART_PTR_FIX(core::SymbolMacroletEnvironment_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::DataLayout_O::static_creator);
 SMART_PTR_FIX(core::ValueFrame_O::static_class);
 TAGGED_POINTER_FIX(llvmo::Type_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ExecutionEngine_O::static_creator);
 SMART_PTR_FIX(core::Rational_O::static_class_symbol);
 SMART_PTR_FIX(core::Specializer_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::FunctionValueEnvironment_O::static_creator);
 SMART_PTR_FIX(core::SingleDispatchMethod_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantPointerNull_O::static_class_symbol);
 SMART_PTR_FIX(core::ArrayDisplaced_O::static_class);
 SMART_PTR_FIX(llvmo::BlockAddress_O::static_class);
 SMART_PTR_FIX(llvmo::LoadInst_O::static_class_symbol);
 SMART_PTR_FIX(core::LogicalPathname_O::static_class);
 TAGGED_POINTER_FIX(core::StrWithFillPtr_O::static_creator);
 TAGGED_POINTER_FIX(core::Real_O::static_creator);
 SMART_PTR_FIX(core::Bignum_O::static_class_symbol);
 SMART_PTR_FIX(core::VectorObjectsWithFillPtr_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IndirectBrInst_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::SingleDispatchGenericFunction_O::static_creator);
 TAGGED_POINTER_FIX(core::MultiStringBuffer_O::static_creator);
 SMART_PTR_FIX(llvmo::SwitchInst_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::TargetSubtargetInfo_O::static_creator);
 TAGGED_POINTER_FIX(core::PosixTime_O::static_creator);
 SMART_PTR_FIX(core::String_O::static_class);
 SMART_PTR_FIX(llvmo::PointerType_O::static_class);
 SMART_PTR_FIX(core::Metaobject_O::static_class);
 SMART_PTR_FIX(core::Ratio_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::IntArray_O::static_creator);
 SMART_PTR_FIX(core::TagbodyEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::SourcePosInfo_O::static_class);
 SMART_PTR_FIX(llvmo::EngineBuilder_O::static_class);
 SMART_PTR_FIX(llvmo::DITypeArray_O::static_class);
 TAGGED_POINTER_FIX(core::Float_O::static_creator);
 SMART_PTR_FIX(llvmo::Type_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTableEqualp_O::static_class);
 SMART_PTR_FIX(core::CxxClass_O::static_class_symbol);
 TAGGED_POINTER_FIX(cffi::Pointer_O::static_creator);
 SMART_PTR_FIX(llvmo::PassManagerBuilder_O::static_class);
 SMART_PTR_FIX(core::CompileTimeEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::WeakKeyMapping_O::static_class);
 TAGGED_POINTER_FIX(core::StackValueEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::StoreInst_O::static_creator);
 SMART_PTR_FIX(llvmo::DebugInfo_O::static_class);
 SMART_PTR_FIX(core::LoadTimeValues_O::static_class);
 SMART_PTR_FIX(llvmo::LandingPadInst_O::static_class);
 SMART_PTR_FIX(llvmo::Attribute_O::static_class);
 SMART_PTR_FIX(llvmo::PassManager_O::static_class);
 TAGGED_POINTER_FIX(llvmo::ModulePass_O::static_creator);
 SMART_PTR_FIX(core::PosixTimeDuration_O::static_class_symbol);
 SMART_PTR_FIX(core::LambdaListHandler_O::static_class);
 SMART_PTR_FIX(core::SimpleBitVector_O::static_class_symbol);
 SMART_PTR_FIX(core::Complex_O::static_class);
 SMART_PTR_FIX(core::Regex_O::static_class);
 SMART_PTR_FIX(llvmo::LLVMTargetMachine_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::DirectoryEntry_O::static_creator);
 SMART_PTR_FIX(llvmo::Constant_O::static_class_symbol);
 TAGGED_POINTER_FIX(asttooling::AstVisitor_O::static_creator);
 TAGGED_POINTER_FIX(core::HashTableEql_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantDataArray_O::static_class);
 SMART_PTR_FIX(core::Class_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IndirectBrInst_O::static_class);
 SMART_PTR_FIX(llvmo::User_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::WeakHashTable_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::LLVMContext_O::static_creator);
 SMART_PTR_FIX(llvmo::AtomicRMWInst_O::static_class);
 SMART_PTR_FIX(llvmo::ArrayType_O::static_class);
 SMART_PTR_FIX(core::UserData_O::static_class);
 SMART_PTR_FIX(core::Record_O::static_class);
 TAGGED_POINTER_FIX(core::Package_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ConstantStruct_O::static_creator);
 TAGGED_POINTER_FIX(core::Pathname_O::static_creator);
 SMART_PTR_FIX(llvmo::Attribute_O::static_class_symbol);
 SMART_PTR_FIX(core::SmallMap_O::static_class_symbol);
 SMART_PTR_FIX(core::StringOutputStream_O::static_class_symbol);
 SMART_PTR_FIX(core::SNode_O::static_class);
 SMART_PTR_FIX(llvmo::LLVMContext_O::static_class);
 TAGGED_POINTER_FIX(llvmo::ConstantArray_O::static_creator);
 SMART_PTR_FIX(llvmo::Triple_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::NamedMDNode_O::static_class);
 SMART_PTR_FIX(llvmo::ExecutionEngine_O::static_class_symbol);
 SMART_PTR_FIX(core::GlueEnvironment_O::static_class);
 SMART_PTR_FIX(core::Record_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::DIScope_O::static_creator);
 SMART_PTR_FIX(core::StackValueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::Pathname_O::static_class);
 TAGGED_POINTER_FIX(core::ForwardReferencedClass_O::static_creator);
 SMART_PTR_FIX(llvmo::StructType_O::static_class);
 SMART_PTR_FIX(llvmo::UnreachableInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Integer_O::static_class_symbol);
 SMART_PTR_FIX(core::BitVector_O::static_class_symbol);
 SMART_PTR_FIX(core::CxxObject_O::static_class);
 SMART_PTR_FIX(core::SourceFileInfo_O::static_class);
 SMART_PTR_FIX(core::Ratio_O::static_class);
 SMART_PTR_FIX(core::TwoWayStream_O::static_class);
 SMART_PTR_FIX(core::Pathname_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::UnreachableInst_O::static_class);
 SMART_PTR_FIX(core::Metaobject_O::static_class_symbol);
 SMART_PTR_FIX(core::StackValueEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::DIArray_O::static_class_symbol);
 SIMPLE_POINTER_FIX(globalTaggedRunTimeValues);
 TAGGED_POINTER_FIX(core::ActivationFrame_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::CallInst_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::Argument_O::static_creator);
 SMART_PTR_FIX(core::Number_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::TagbodyEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::StructType_O::static_creator);
 SMART_PTR_FIX(core::DirectoryIterator_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetSubtargetInfo_O::static_class);
 SMART_PTR_FIX(core::StructureObject_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::InvocationHistoryFrameIterator_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::PassManagerBase_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::IRBuilderBase_O::static_creator);
 SMART_PTR_FIX(llvmo::CallInst_O::static_class);
 SMART_PTR_FIX(cffi::Pointer_O::static_class);
 SMART_PTR_FIX(core::VectorObjectsWithFillPtr_O::static_class);
 SMART_PTR_FIX(llvmo::ImmutablePass_O::static_class);
 TAGGED_POINTER_FIX(core::WrappedPointer_O::static_creator);
 SMART_PTR_FIX(core::LongFloat_O::static_class);
 SMART_PTR_FIX(core::SaveArchive_O::static_class);
 TAGGED_POINTER_FIX(core::TagbodyFrame_O::static_creator);
 SMART_PTR_FIX(core::DoubleFloat_O::static_class);
 SMART_PTR_FIX(llvmo::DISubroutineType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DISubroutineType_O::static_class);
 TAGGED_POINTER_FIX(core::Character_dummy_O::static_creator);
 TAGGED_POINTER_FIX(core::IOStreamStream_O::static_creator);
 TAGGED_POINTER_FIX(core::ExternalObject_O::static_creator);
 SMART_PTR_FIX(core::DoubleFloat_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::WeakKeyHashTable_O::static_creator);
 SMART_PTR_FIX(core::Specializer_O::static_class);
 SMART_PTR_FIX(llvmo::DICompileUnit_O::static_class_symbol);
 SMART_PTR_FIX(core::Environment_O::static_class);
 TAGGED_POINTER_FIX(core::WeakKeyMapping_O::static_creator);
 SMART_PTR_FIX(llvmo::LoadInst_O::static_class);
 SMART_PTR_FIX(core::SexpLoadArchive_O::static_class);
 SMART_PTR_FIX(core::SimpleBitVector_O::static_class);
 SMART_PTR_FIX(core::Float_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::RandomState_O::static_creator);
 SMART_PTR_FIX(core::Function_O::static_class);
 TAGGED_POINTER_FIX(_lisp);
 SMART_PTR_FIX(llvmo::FunctionType_O::static_class_symbol);
 SMART_PTR_FIX(core::DirectoryEntry_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::BuiltInClass_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::DIBuilder_O::static_creator);
 TAGGED_POINTER_FIX(core::SynonymStream_O::static_creator);
 SMART_PTR_FIX(core::CandoException_O::static_class_symbol);
 SMART_PTR_FIX(clbind::ClassRegistry_O::static_class);
 SMART_PTR_FIX(llvmo::LandingPadInst_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::HashTableEq_O::static_creator);
 SMART_PTR_FIX(llvmo::DILexicalBlock_O::static_class);
 SMART_PTR_FIX(llvmo::ImmutablePass_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::TargetLibraryInfo_O::static_creator);
 TAGGED_POINTER_FIX(core::CompileTimeEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(core::ValueEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::BranchInst_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::SourceManager_O::static_creator);
 SMART_PTR_FIX(core::Stream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ReturnInst_O::static_class);
 SMART_PTR_FIX(llvmo::LLVMTargetMachine_O::static_class);
 SMART_PTR_FIX(core::MultiStringBuffer_O::static_class_symbol);
 SMART_PTR_FIX(core::ReadTable_O::static_class);
 SMART_PTR_FIX(llvmo::DISubprogram_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::AtomicRMWInst_O::static_class_symbol);
 SMART_PTR_FIX(core::StructureClass_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::FileStream_O::static_creator);
 SMART_PTR_FIX(llvmo::AtomicCmpXchgInst_O::static_class_symbol);
 SMART_PTR_FIX(core::MultiStringBuffer_O::static_class);
 TAGGED_POINTER_FIX(llvmo::AllocaInst_O::static_creator);
 TAGGED_POINTER_FIX(core::Function_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::DIFile_O::static_creator);
 SMART_PTR_FIX(core::WeakKeyHashTable_O::static_class);
 SMART_PTR_FIX(core::FunctionValueEnvironment_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::SmallMap_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ImmutablePass_O::static_creator);
 SMART_PTR_FIX(core::FuncallableStandardClass_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::Rational_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::AtomicRMWInst_O::static_creator);
 SMART_PTR_FIX(core::WrappedPointer_O::static_class_symbol);
 SMART_PTR_FIX(core::ConcatenatedStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::VAArgInst_O::static_class_symbol);
 SMART_PTR_FIX(core::String_O::static_class_symbol);
 SMART_PTR_FIX(core::SingleDispatchGenericFunction_O::static_class);
 SMART_PTR_FIX(core::LightUserData_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ReturnInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::CompositeType_O::static_class);
 SMART_PTR_FIX(core::Real_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantInt_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::MCSubtargetInfo_O::static_class);
 SMART_PTR_FIX(llvmo::DIDerivedType_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::TargetMachine_O::static_creator);
 SMART_PTR_FIX(llvmo::UndefValue_O::static_class_symbol);
 SMART_PTR_FIX(core::Archive_O::static_class_symbol);
 SMART_PTR_FIX(core::CompileTimeEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::ValueAsMetadata_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::APInt_O::static_creator);
 SMART_PTR_FIX(core::ActivationFrame_O::static_class);
 SMART_PTR_FIX(llvmo::SequentialType_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::GlobalVariable_O::static_creator);
 TAGGED_POINTER_FIX(core::Ratio_O::static_creator);
 TAGGED_POINTER_FIX(core::Cons_O::static_creator);
 SMART_PTR_FIX(llvmo::Metadata_O::static_class_symbol);
 SMART_PTR_FIX(core::Vector_O::static_class_symbol);
 SMART_PTR_FIX(core::SymbolToEnumConverter_O::static_class);
 SMART_PTR_FIX(llvmo::DebugLoc_O::static_class);
 TAGGED_POINTER_FIX(core::BranchSNode_O::static_creator);
 SMART_PTR_FIX(llvmo::DIBasicType_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::Reader_O::static_creator);
 TAGGED_POINTER_FIX(core::ArrayObjects_O::static_creator);
 SMART_PTR_FIX(core::StandardObject_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::VectorObjectsWithFillPtr_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::BranchInst_O::static_creator);
 SMART_PTR_FIX(core::StrWithFillPtr_O::static_class);
 SMART_PTR_FIX(core::RecursiveDirectoryIterator_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::TwoWayStream_O::static_creator);
 TAGGED_POINTER_FIX(core::ShortFloat_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::AtomicCmpXchgInst_O::static_creator);
 TAGGED_POINTER_FIX(core::Archive_O::static_creator);
 SMART_PTR_FIX(llvmo::Argument_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::IntegerType_O::static_creator);
 SMART_PTR_FIX(llvmo::StoreInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Reader_O::static_class);
 TAGGED_POINTER_FIX(core::BroadcastStream_O::static_creator);
 TAGGED_POINTER_FIX(core::SpecialForm_O::static_creator);
 SMART_PTR_FIX(llvmo::GlobalValue_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::VaList_dummy_O::static_creator);
 SMART_PTR_FIX(core::RandomState_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::InsertPoint_O::static_creator);
 SMART_PTR_FIX(core::WeakKeyMapping_O::static_class_symbol);
 SMART_PTR_FIX(core::InvocationHistoryFrameIterator_O::static_class);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_unbound);
 SMART_PTR_FIX(core::ActivationFrame_O::static_class_symbol);
 SMART_PTR_FIX(core::SexpSaveArchive_O::static_class);
 TAGGED_POINTER_FIX(llvmo::TargetOptions_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantArray_O::static_class);
 SMART_PTR_FIX(llvmo::DataLayoutPass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::FunctionPass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::PassManager_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::IOFileStream_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantDataSequential_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::BasicBlock_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::TerminatorInst_O::static_creator);
 TAGGED_POINTER_FIX(core::SourcePosInfo_O::static_creator);
 SMART_PTR_FIX(core::Instance_O::static_class);
 TAGGED_POINTER_FIX(core::Specializer_O::static_creator);
 SMART_PTR_FIX(llvmo::DIScope_O::static_class_symbol);
 SMART_PTR_FIX(core::WeakHashTable_O::static_class_symbol);
 SMART_PTR_FIX(core::ShortFloat_O::static_class);
 SMART_PTR_FIX(core::AnsiStream_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::CandoException_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ResumeInst_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::PointerType_O::static_creator);
 SMART_PTR_FIX(core::ShortFloat_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTable_O::static_class);
 SMART_PTR_FIX(core::SNode_O::static_class_symbol);
 SMART_PTR_FIX(core::RuntimeVisibleEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTable_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::BlockAddress_O::static_class_symbol);
 SMART_PTR_FIX(core::VaList_dummy_O::static_class);
 SMART_PTR_FIX(core::Vector_O::static_class);
 TAGGED_POINTER_FIX(llvmo::GlobalValue_O::static_creator);
 SMART_PTR_FIX(llvmo::DIBasicType_O::static_class);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_nil);
 SMART_PTR_FIX(core::Rational_O::static_class);
 SMART_PTR_FIX(core::SmallMultimap_O::static_class);
 SMART_PTR_FIX(core::T_O::static_class);
 SMART_PTR_FIX(llvmo::VAArgInst_O::static_class);
 SMART_PTR_FIX(core::CandoException_O::static_class);
 SMART_PTR_FIX(core::Class_O::static_class);
 SMART_PTR_FIX(core::HashTableEqual_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Triple_O::static_class);
 SMART_PTR_FIX(core::CatchEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::PosixTime_O::static_class);
 SMART_PTR_FIX(llvmo::EngineBuilder_O::static_class_symbol);
 SMART_PTR_FIX(core::BuiltInClass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::FenceInst_O::static_class_symbol);
 SMART_PTR_FIX(core::Binder_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::StringInputStream_O::static_creator);
 SMART_PTR_FIX(core::WeakPointer_O::static_class);
 TAGGED_POINTER_FIX(llvmo::Attribute_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::LLVMTargetMachine_O::static_creator);
 SMART_PTR_FIX(core::RecursiveDirectoryIterator_O::static_class);
 SMART_PTR_FIX(llvmo::TargetOptions_O::static_class);
 SMART_PTR_FIX(core::WrappedPointer_O::static_class);
 TAGGED_POINTER_FIX(llvmo::IRBuilder_O::static_creator);
 SMART_PTR_FIX(llvmo::Target_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::CatchEnvironment_O::static_creator);
 SMART_PTR_FIX(llvmo::DIType_O::static_class);
 SMART_PTR_FIX(llvmo::Function_O::static_class);
 SMART_PTR_FIX(llvmo::GlobalVariable_O::static_class_symbol);
 SMART_PTR_FIX(core::SmallMap_O::static_class);
 TAGGED_POINTER_FIX(core::Integer_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::FunctionPass_O::static_creator);
 SMART_PTR_FIX(core::Regex_O::static_class_symbol);
 SMART_PTR_FIX(core::BitVectorWithFillPtr_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Pass_O::static_class);
 SMART_PTR_FIX(llvmo::FunctionPassManager_O::static_class);
 SMART_PTR_FIX(core::Reader_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DISubprogram_O::static_class);
 TAGGED_POINTER_FIX(core::CxxClass_O::static_creator);
 TAGGED_POINTER_FIX(core::Record_O::static_creator);
 SMART_PTR_FIX(core::ExternalObject_O::static_class);
 TAGGED_POINTER_FIX(core::WeakPointer_O::static_creator);
 SMART_PTR_FIX(llvmo::TargetMachine_O::static_class_symbol);
 SMART_PTR_FIX(core::InvocationHistoryFrameIterator_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetMachine_O::static_class);
 TAGGED_POINTER_FIX(llvmo::FunctionPassManager_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::DICompositeType_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ConstantFP_O::static_creator);
 SMART_PTR_FIX(llvmo::CompositeType_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::Triple_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantDataArray_O::static_class_symbol);
 SMART_PTR_FIX(core::LexicalEnvironment_O::static_class);
 SMART_PTR_FIX(core::SymbolMacroletEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::Module_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DIBasicType_O::static_creator);
 SMART_PTR_FIX(llvmo::UnaryInstruction_O::static_class);
 SMART_PTR_FIX(llvmo::Constant_O::static_class);
 SMART_PTR_FIX(llvmo::BasicBlock_O::static_class);
 SMART_PTR_FIX(core::Number_O::static_class);
 SMART_PTR_FIX(core::ValueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::PosixTime_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::SourceFileInfo_O::static_creator);
 SMART_PTR_FIX(llvmo::TargetLibraryInfo_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::Instruction_O::static_creator);
 SMART_PTR_FIX(core::FunctionContainerEnvironment_O::static_class);
 SMART_PTR_FIX(core::ArrayObjects_O::static_class_symbol);
 SMART_PTR_FIX(core::BuiltInClass_O::static_class);
 SMART_PTR_FIX(core::BranchSNode_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::ConstantPointerNull_O::static_creator);
 SMART_PTR_FIX(core::SmallMultimap_O::static_class_symbol);
 SMART_PTR_FIX(core::ExternalObject_O::static_class_symbol);
 SMART_PTR_FIX(core::ForwardReferencedClass_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::GlueEnvironment_O::static_creator);
 SMART_PTR_FIX(core::BitVector_O::static_class);
 TAGGED_POINTER_FIX(core::LoadArchive_O::static_creator);
 SMART_PTR_FIX(core::Array_O::static_class_symbol);
 TAGGED_POINTER_FIX(clbind::globalClassMap);
 TAGGED_POINTER_FIX(llvmo::UndefValue_O::static_creator);
 SMART_PTR_FIX(core::Str_O::static_class_symbol);
 SMART_PTR_FIX(core::Archive_O::static_class);
 SMART_PTR_FIX(core::RuntimeVisibleEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::ResumeInst_O::static_class);
 SMART_PTR_FIX(llvmo::VectorType_O::static_class);
 SMART_PTR_FIX(core::UnwindProtectEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::MacroletEnvironment_O::static_class);
 TAGGED_POINTER_FIX(llvmo::UnaryInstruction_O::static_creator);
 SMART_PTR_FIX(core::Iterator_O::static_class);
 SMART_PTR_FIX(llvmo::InvokeInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantInt_O::static_class);
 SMART_PTR_FIX(core::ValueFrame_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::FunctionFrame_O::static_creator);
 SMART_PTR_FIX(core::PosixTimeDuration_O::static_class);
 SMART_PTR_FIX(core::Pointer_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantArray_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::LLVMContext_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::ArrayDisplaced_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::UnreachableInst_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::PassManager_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantDataSequential_O::static_class);
 SMART_PTR_FIX(llvmo::FunctionPass_O::static_class);
 SMART_PTR_FIX(llvmo::ArrayType_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::DIType_O::static_creator);
 TAGGED_POINTER_FIX(core::LeafSNode_O::static_creator);
 SMART_PTR_FIX(core::VaList_dummy_O::static_class_symbol);
 SMART_PTR_FIX(core::VectorObjects_O::static_class);
 SMART_PTR_FIX(core::SingleFloat_dummy_O::static_class);
 SMART_PTR_FIX(llvmo::DataLayout_O::static_class);
 SMART_PTR_FIX(llvmo::DIBuilder_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DILexicalBlock_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::SexpLoadArchive_O::static_creator);
 SMART_PTR_FIX(llvmo::DataLayout_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::VAArgInst_O::static_creator);
 SMART_PTR_FIX(core::SourcePosInfo_O::static_class_symbol);
 TAGGED_POINTER_FIX(asttooling::RegMap::RegistryData);
 SMART_PTR_FIX(core::RegexMatch_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::Array_O::static_creator);
 SMART_PTR_FIX(core::CatchEnvironment_O::static_class);
 SMART_PTR_FIX(core::StructureObject_O::static_class);
 SMART_PTR_FIX(llvmo::PHINode_O::static_class);
 SMART_PTR_FIX(core::IntArray_O::static_class);
 SMART_PTR_FIX(core::WeakPointer_O::static_class_symbol);
 SMART_PTR_FIX(core::LeafSNode_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::LambdaListHandler_O::static_creator);
 TAGGED_POINTER_FIX(core::StringOutputStream_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::EngineBuilder_O::static_creator);
 SMART_PTR_FIX(core::Bignum_O::static_class);
 SMART_PTR_FIX(core::T_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::CxxObject_O::static_creator);
 SMART_PTR_FIX(llvmo::Target_O::static_class);
 TAGGED_POINTER_FIX(core::LoadTimeValues_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::DILexicalBlock_O::static_creator);
 SMART_PTR_FIX(llvmo::IRBuilderBase_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantStruct_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::Constant_O::static_creator);
 TAGGED_POINTER_FIX(core::SingleDispatchMethod_O::static_creator);
 TAGGED_POINTER_FIX(core::SingleDispatchEffectiveMethodFunction_O::static_creator);
 SMART_PTR_FIX(core::FunctionValueEnvironment_O::static_class);
 TAGGED_POINTER_FIX(core::Number_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::DIArray_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::MDString_O::static_creator);
 SMART_PTR_FIX(core::LoadArchive_O::static_class_symbol);
 SMART_PTR_FIX(core::Binder_O::static_class);
 SMART_PTR_FIX(llvmo::PHINode_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::PassManagerBuilder_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::VectorType_O::static_creator);
 SMART_PTR_FIX(core::TwoWayStream_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::T_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::Function_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantPointerNull_O::static_class);
 SMART_PTR_FIX(core::RandomState_O::static_class);
 SMART_PTR_FIX(core::Character_dummy_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::ConcatenatedStream_O::static_creator);
 SMART_PTR_FIX(llvmo::UnaryInstruction_O::static_class_symbol);
 SMART_PTR_FIX(core::IOFileStream_O::static_class_symbol);
 SMART_PTR_FIX(core::ArrayObjects_O::static_class);
 SMART_PTR_FIX(llvmo::MDNode_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::ConstantDataArray_O::static_creator);
 SMART_PTR_FIX(core::FileStream_O::static_class_symbol);
 SMART_PTR_FIX(core::SingleDispatchMethod_O::static_class);
 SMART_PTR_FIX(llvmo::IRBuilder_O::static_class_symbol);
 SMART_PTR_FIX(core::HashTableEq_O::static_class);
 TAGGED_POINTER_FIX(core::Null_O::static_creator);
 SMART_PTR_FIX(core::Instance_O::static_class_symbol);
 SMART_PTR_FIX(core::Complex_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Linker_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIScope_O::static_class);
 SMART_PTR_FIX(asttooling::AstVisitor_O::static_class);
 SMART_PTR_FIX(llvmo::Metadata_O::static_class);
 TAGGED_POINTER_FIX(core::Instance_O::static_creator);
 SMART_PTR_FIX(llvmo::FunctionType_O::static_class);
 SMART_PTR_FIX(core::Iterator_O::static_class_symbol);
 SMART_PTR_FIX(core::Stream_O::static_class);
 SMART_PTR_FIX(core::Fixnum_dummy_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::CallInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DICompileUnit_O::static_class);
 SMART_PTR_FIX(core::StructureClass_O::static_class);
 SMART_PTR_FIX(core::SaveArchive_O::static_class_symbol);
 SMART_PTR_FIX(core::UnwindProtectEnvironment_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DataLayoutPass_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantStruct_O::static_class);
 TAGGED_POINTER_FIX(core::Class_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::SwitchInst_O::static_creator);
 SMART_PTR_FIX(core::TagbodyFrame_O::static_class);
 SMART_PTR_FIX(core::ArrayDisplaced_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Instruction_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::IntegerType_O::static_class);
 TAGGED_POINTER_FIX(clbind::ClassRep_O::static_creator);
 SMART_PTR_FIX(core::FunctionFrame_O::static_class);
 SMART_PTR_FIX(llvmo::Module_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::LightUserData_O::static_creator);
 TAGGED_POINTER_FIX(core::MacroletEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::NamedMDNode_O::static_creator);
 SMART_PTR_FIX(core::ForwardReferencedClass_O::static_class);
 TAGGED_POINTER_FIX(core::StringStream_O::static_creator);
 SMART_PTR_FIX(core::Symbol_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::IndirectBrInst_O::static_creator);
 SMART_PTR_FIX(llvmo::Argument_O::static_class);
 TAGGED_POINTER_FIX(llvmo::CompositeType_O::static_creator);
 SMART_PTR_FIX(core::SingleDispatchEffectiveMethodFunction_O::static_class);
 TAGGED_POINTER_FIX(core::Fixnum_dummy_O::static_creator);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_sameAsKey);
 SMART_PTR_FIX(core::HashTableEql_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::Path_O::static_creator);
 SMART_PTR_FIX(core::BlockEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::Character_dummy_O::static_class);
 SMART_PTR_FIX(core::Path_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::String_O::static_creator);
 SMART_PTR_FIX(llvmo::UndefValue_O::static_class);
 SMART_PTR_FIX(llvmo::AttributeSet_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::DITypeArray_O::static_creator);
 TAGGED_POINTER_FIX(clbind::ClassRegistry_O::static_creator);
 TAGGED_POINTER_FIX(core::Str_O::static_creator);
 SMART_PTR_FIX(core::StandardClass_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::HashTableEqualp_O::static_creator);
 SMART_PTR_FIX(core::ReadTable_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::ReadTable_O::static_creator);
 TAGGED_POINTER_FIX(core::SymbolToEnumConverter_O::static_creator);
 SMART_PTR_FIX(llvmo::DIDerivedType_O::static_class);
 SMART_PTR_FIX(core::HashTableEqualp_O::static_class_symbol);
 SMART_PTR_FIX(core::DirectoryIterator_O::static_class);
 SMART_PTR_FIX(core::TagbodyFrame_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Function_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::SimpleBitVector_O::static_creator);
 SMART_PTR_FIX(clbind::ClassRep_O::static_class_symbol);
 SMART_PTR_FIX(core::FunctionFrame_O::static_class_symbol);
 SMART_PTR_FIX(core::SourceManager_O::static_class);
 SMART_PTR_FIX(clbind::ClassRegistry_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::ReturnInst_O::static_creator);
 TAGGED_POINTER_FIX(core::RegexMatch_O::static_creator);
 TAGGED_POINTER_FIX(core::FunctionContainerEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::FunctionType_O::static_creator);
 TAGGED_POINTER_FIX(core::SaveArchive_O::static_creator);
 SMART_PTR_FIX(core::SourceFileInfo_O::static_class_symbol);
 SMART_PTR_FIX(core::StandardClass_O::static_class);
 TAGGED_POINTER_FIX(core::Bignum_O::static_creator);
 SMART_PTR_FIX(llvmo::DIFile_O::static_class_symbol);
 SMART_PTR_FIX(core::Fixnum_dummy_O::static_class);
 SMART_PTR_FIX(llvmo::InsertPoint_O::static_class);
 TAGGED_POINTER_FIX(llvmo::Target_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::FenceInst_O::static_creator);
 SMART_PTR_FIX(core::ValueEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::DIType_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::ArrayType_O::static_creator);
 SMART_PTR_FIX(core::SexpLoadArchive_O::static_class_symbol);
 SMART_PTR_FIX(core::LeafSNode_O::static_class);
 TAGGED_POINTER_FIX(core::HashTableEqual_O::static_creator);
 SMART_PTR_FIX(llvmo::PassManagerBase_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::FileStatus_O::static_creator);
 SMART_PTR_FIX(core::Symbol_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DICompileUnit_O::static_creator);
 SMART_PTR_FIX(core::FileStatus_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::APFloat_O::static_class);
 SMART_PTR_FIX(core::StringOutputStream_O::static_class);
 SMART_PTR_FIX(llvmo::StoreInst_O::static_class);
 TAGGED_POINTER_FIX(core::EchoStream_O::static_creator);
 SMART_PTR_FIX(core::ConcatenatedStream_O::static_class);
 SMART_PTR_FIX(core::Package_O::static_class_symbol);
 SMART_PTR_FIX(core::VectorDisplaced_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TerminatorInst_O::static_class);
 SMART_PTR_FIX(core::LoadArchive_O::static_class);
 SMART_PTR_FIX(llvmo::TargetSubtargetInfo_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::AttributeSet_O::static_creator);
 SMART_PTR_FIX(core::AnsiStream_O::static_class);
 SMART_PTR_FIX(core::Null_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::ConstantDataSequential_O::static_creator);
 SMART_PTR_FIX(core::ForeignData_O::static_class_symbol);
 SMART_PTR_FIX(core::BroadcastStream_O::static_class);
 SMART_PTR_FIX(core::LightUserData_O::static_class);
 TAGGED_POINTER_FIX(core::Metaobject_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::Pass_O::static_creator);
 SMART_PTR_FIX(core::LongFloat_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::BlockEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(core::BitVectorWithFillPtr_O::static_creator);
 SMART_PTR_FIX(llvmo::AllocaInst_O::static_class_symbol);
 SMART_PTR_FIX(core::FunctionContainerEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Value_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::StructureObject_O::static_creator);
 TAGGED_POINTER_FIX(core::Symbol_O::static_creator);
 SMART_PTR_FIX(asttooling::AstVisitor_O::static_class_symbol);
 SMART_PTR_FIX(core::LogicalPathname_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::Regex_O::static_creator);
 SMART_PTR_FIX(core::FileStream_O::static_class);
 TAGGED_POINTER_FIX(llvmo::Metadata_O::static_creator);
 TAGGED_POINTER_FIX(core::Complex_O::static_creator);
 SMART_PTR_FIX(core::HashTableEqual_O::static_class);
 SMART_PTR_FIX(llvmo::Value_O::static_class);
 TAGGED_POINTER_FIX(core::UnwindProtectEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ValueAsMetadata_O::static_creator);
 SMART_PTR_FIX(core::CxxClass_O::static_class);
 SMART_PTR_FIX(llvmo::PointerType_O::static_class_symbol);
 SMART_PTR_FIX(core::Cons_O::static_class_symbol);
 SMART_PTR_FIX(core::Integer_O::static_class);
 TAGGED_POINTER_FIX(llvmo::LandingPadInst_O::static_creator);
 SMART_PTR_FIX(core::Str_O::static_class);
 TAGGED_POINTER_FIX(core::Environment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::User_O::static_creator);
 SMART_PTR_FIX(core::VectorDisplaced_O::static_class);
 SMART_PTR_FIX(core::StringStream_O::static_class);
 SMART_PTR_FIX(core::CompiledFunction_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantFP_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Type_O::static_class);
 TAGGED_POINTER_FIX(llvmo::InvokeInst_O::static_creator);
 TAGGED_POINTER_FIX(core::VectorObjects_O::static_creator);
 SMART_PTR_FIX(core::StandardObject_O::static_class);
 TAGGED_POINTER_FIX(llvmo::PassManagerBuilder_O::static_creator);
 SMART_PTR_FIX(llvmo::AllocaInst_O::static_class);
 SMART_PTR_FIX(llvmo::APInt_O::static_class);
 SMART_PTR_FIX(llvmo::ConstantExpr_O::static_class_symbol);
 SMART_PTR_FIX(core::StdClass_O::static_class_symbol);
 SMART_PTR_FIX(core::BitVectorWithFillPtr_O::static_class);
 SMART_PTR_FIX(llvmo::SwitchInst_O::static_class);
 TAGGED_POINTER_FIX(llvmo::ConstantInt_O::static_creator);
 SMART_PTR_FIX(llvmo::ExecutionEngine_O::static_class);
 SMART_PTR_FIX(llvmo::DIDescriptor_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::LongFloat_O::static_creator);
 SMART_PTR_FIX(core::CxxObject_O::static_class_symbol);
 SMART_PTR_FIX(core::StringInputStream_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::FuncallableStandardClass_O::static_creator);
 TAGGED_POINTER_FIX(core::BitVector_O::static_creator);
 SMART_PTR_FIX(core::SynonymStream_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DISubroutineType_O::static_creator);
 SMART_PTR_FIX(llvmo::DebugLoc_O::static_class_symbol);
 SMART_PTR_FIX(core::GlueEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DebugInfo_O::static_class_symbol);
 SMART_PTR_FIX(core::ForeignData_O::static_class);
 TAGGED_POINTER_FIX(llvmo::LoadInst_O::static_creator);
 SMART_PTR_FIX(core::StrWithFillPtr_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::DIDerivedType_O::static_creator);
 SMART_PTR_FIX(llvmo::Instruction_O::static_class);
 SMART_PTR_FIX(core::SpecialForm_O::static_class);
 SMART_PTR_FIX(core::BranchSNode_O::static_class);
 TAGGED_POINTER_FIX(core::SNode_O::static_creator);
 SMART_PTR_FIX(llvmo::DICompositeType_O::static_class);
 SMART_PTR_FIX(llvmo::IntegerType_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::Linker_O::static_creator);
 SMART_PTR_FIX(core::Environment_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::StandardObject_O::static_creator);
 SMART_PTR_FIX(core::IOStreamStream_O::static_class);
 TAGGED_POINTER_FIX(llvmo::PHINode_O::static_creator);
 TAGGED_POINTER_FIX(core::Vector_O::static_creator);
 SMART_PTR_FIX(llvmo::NamedMDNode_O::static_class_symbol);
 SMART_PTR_FIX(core::SingleDispatchEffectiveMethodFunction_O::static_class_symbol);
 SMART_PTR_FIX(core::CompiledFunction_O::static_class_symbol);
 SMART_PTR_FIX(core::EchoStream_O::static_class);
 SMART_PTR_FIX(llvmo::MDString_O::static_class);
 SMART_PTR_FIX(core::Pointer_O::static_class);
 SMART_PTR_FIX(llvmo::MCSubtargetInfo_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DICompositeType_O::static_class_symbol);
 SMART_PTR_FIX(core::MacroletEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::LoadTimeValues_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::RuntimeVisibleEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(core::StandardClass_O::static_creator);
 TAGGED_POINTER_FIX(core::ValueFrame_O::static_creator);
 SMART_PTR_FIX(core::VectorObjects_O::static_class_symbol);
 SMART_PTR_FIX(cffi::Pointer_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::InsertPoint_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::MDNode_O::static_class);
 SMART_PTR_FIX(core::IOStreamStream_O::static_class_symbol);
 SMART_PTR_FIX(core::Null_O::static_class);
 SMART_PTR_FIX(core::SexpSaveArchive_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetLibraryInfo_O::static_class);
 SMART_PTR_FIX(llvmo::Linker_O::static_class);
 TAGGED_POINTER_FIX(core::SexpSaveArchive_O::static_creator);
 SMART_PTR_FIX(llvmo::User_O::static_class);
 SMART_PTR_FIX(core::Package_O::static_class);
 SMART_PTR_FIX(core::SingleDispatchGenericFunction_O::static_class_symbol);
 SMART_PTR_FIX(core::SpecialForm_O::static_class_symbol);
 SMART_PTR_FIX(core::StringStream_O::static_class_symbol);
 SMART_PTR_FIX(core::IntArray_O::static_class_symbol);
 SMART_PTR_FIX(core::EchoStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::DIBuilder_O::static_class);
 SMART_PTR_FIX(core::HashTableEql_O::static_class);
 TAGGED_POINTER_FIX(llvmo::Value_O::static_creator);
 TAGGED_POINTER_FIX(core::SymbolMacroletEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::BlockAddress_O::static_creator);
 SMART_PTR_FIX(llvmo::APInt_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::MCSubtargetInfo_O::static_creator);
 SMART_PTR_FIX(core::Real_O::static_class);
 SMART_PTR_FIX(llvmo::ModulePass_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ConstantFP_O::static_class);
 SMART_PTR_FIX(core::BroadcastStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ResumeInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::ModulePass_O::static_class);
 SMART_PTR_FIX(clbind::ClassRep_O::static_class);
 TAGGED_POINTER_FIX(llvmo::MDNode_O::static_creator);
 SMART_PTR_FIX(core::SynonymStream_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::AttributeSet_O::static_class);
 SMART_PTR_FIX(llvmo::StructType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::Pass_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::LogicalPathname_O::static_creator);
 TAGGED_POINTER_FIX(core::LexicalEnvironment_O::static_creator);
 TAGGED_POINTER_FIX(core::DirectoryIterator_O::static_creator);
 SMART_PTR_FIX(core::LexicalEnvironment_O::static_class_symbol);
 SMART_PTR_FIX(core::SourceManager_O::static_class_symbol);
 SMART_PTR_FIX(core::DirectoryEntry_O::static_class);
 SMART_PTR_FIX(core::TagbodyEnvironment_O::static_class);
 SMART_PTR_FIX(core::FuncallableStandardClass_O::static_class);
 TAGGED_POINTER_FIX(core::UserData_O::static_creator);
 SMART_PTR_FIX(llvmo::GlobalVariable_O::static_class);
 TAGGED_POINTER_FIX(llvmo::Module_O::static_creator);
 SMART_PTR_FIX(llvmo::GlobalValue_O::static_class);
 SMART_PTR_FIX(core::BlockEnvironment_O::static_class);
 SMART_PTR_FIX(llvmo::IRBuilderBase_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::VectorType_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::FenceInst_O::static_class);
 SMART_PTR_FIX(llvmo::AtomicCmpXchgInst_O::static_class);
 SIMPLE_POINTER_FIX(gctools::global_tagged_Symbol_OP_deleted);
 SMART_PTR_FIX(llvmo::DIFile_O::static_class);
 TAGGED_POINTER_FIX(core::DoubleFloat_O::static_creator);
 TAGGED_POINTER_FIX(core::Pointer_O::static_creator);
 TAGGED_POINTER_FIX(core::SingleFloat_dummy_O::static_creator);
 SMART_PTR_FIX(core::StdClass_O::static_class);
 SMART_PTR_FIX(core::Path_O::static_class);
 SMART_PTR_FIX(llvmo::MDString_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::CompiledFunction_O::static_creator);
 SMART_PTR_FIX(llvmo::FunctionPassManager_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::SequentialType_O::static_creator);
 SMART_PTR_FIX(core::UserData_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::DebugLoc_O::static_creator);
 SMART_PTR_FIX(core::WeakKeyHashTable_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::APFloat_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TerminatorInst_O::static_class_symbol);
 SMART_PTR_FIX(llvmo::TargetOptions_O::static_class_symbol);
 TAGGED_POINTER_FIX(llvmo::APFloat_O::static_creator);
 TAGGED_POINTER_FIX(core::RecursiveDirectoryIterator_O::static_creator);
 TAGGED_POINTER_FIX(core::ForeignData_O::static_creator);
 SMART_PTR_FIX(llvmo::InvokeInst_O::static_class);
 SMART_PTR_FIX(core::StringInputStream_O::static_class);
 SMART_PTR_FIX(core::WeakHashTable_O::static_class);
 SMART_PTR_FIX(core::Function_O::static_class_symbol);
 TAGGED_POINTER_FIX(core::VectorDisplaced_O::static_creator);
 TAGGED_POINTER_FIX(llvmo::ConstantExpr_O::static_creator);
 SMART_PTR_FIX(llvmo::ConstantExpr_O::static_class);
 TAGGED_POINTER_FIX(llvmo::BasicBlock_O::static_creator);
 SMART_PTR_FIX(core::SingleFloat_dummy_O::static_class_symbol);
 SMART_PTR_FIX(core::SymbolToEnumConverter_O::static_class_symbol);
 SMART_PTR_FIX(core::Cons_O::static_class);
 TAGGED_POINTER_FIX(llvmo::DebugInfo_O::static_creator);
 SMART_PTR_FIX(core::Float_O::static_class);
 SMART_PTR_FIX(llvmo::DataLayoutPass_O::static_class);

#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
