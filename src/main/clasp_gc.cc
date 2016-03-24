#ifdef DECLARE_FORWARDS
 namespace core {
    class LogicalPathname_O;
    class ExceptionEntry;
    class UnwindProtectEnvironment_O;
    class Integer_O;
    class SequenceStepper_O;
    class Rational_O;
    class RecursiveDirectoryIterator_O;
    class ConsStepper_O;
    class Metaobject_O;
    class Str_O;
    class WeakKeyHashTable_O;
    class DirectoryIterator_O;
    class Symbol_O;
    class Float_O;
    class VectorObjects_O;
    class AuxArgument;
    class FunctionFrame_O;
    class Real_O;
    class CompiledClosure_O;
    class ShortFloat_O;
    class SingleFloat_dummy_O;
    class String_O;
    class DoubleFloat_O;
    class OptionalArgument;
    class LexicalEnvironment_O;
    class ActivationFrame_O;
    class ValueFrame_O;
    class Environment_O;
    class KeywordArgument;
    class Path_O;
    class StackValueEnvironment_O;
    class Pointer_O;
    class SymbolClassPair;
    class RuntimeVisibleEnvironment_O;
    class Fixnum_dummy_O;
    class TagbodyEnvironment_O;
    class VectorStepper_O;
    class Vector_O;
    class Ratio_O;
    class CandoException_O;
    class T_O;
    class Array_O;
    class SourceFileInfo_O;
    class LambdaListHandler_O;
    class Creator_O;
    class SymbolMacroletEnvironment_O;
    class CacheRecord;
    class MacroletEnvironment_O;
    class SymbolToEnumConverter_O;
    class CompiledFunction_O;
    class LongFloat_O;
    class TagbodyFrame_O;
    class Closure_O;
    class WeakHashTable_O;
    class Specializer_O;
    class General_O;
    class DynamicBinding;
    class FunctionValueEnvironment_O;
    class InstanceClosure_O;
    class Bignum_O;
    class BuiltinClosure_O;
    class GlueEnvironment_O;
    class RequiredArgument;
    class StandardObject_O;
    class FileStatus_O;
    class Cons_O;
    class Package_O;
    class FunctionContainerEnvironment_O;
    class InterpretedClosure_O;
    class SourceManager_O;
    class CatchEnvironment_O;
    class Functor_O;
    class Number_O;
    class MacroClosure_O;
    class Instance_O;
    class ValueEnvironment_O;
    class BlockEnvironment_O;
    class DirectoryEntry_O;
    class SourcePosInfo_O;
    class CxxObject_O;
    class Pathname_O;
    class Iterator_O;
    class CompileTimeEnvironment_O;
    class Complex_O;
    class Class_O;
    class FunctionClosure_O;
    class Function_O;
    class InvocationHistoryFrameIterator_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_CONS = 4, 
KIND_CHARACTER = 3, 
KIND_SINGLE_FLOAT = 2, 
KIND_FIXNUM = 1, 
KIND_BOOTSTRAP_core__T_O = 5,
KIND_LISPALLOC_core__General_O = 6,
KIND_LISPALLOC_core__SequenceStepper_O = 7,
KIND_LISPALLOC_core__ConsStepper_O = 8,
KIND_LISPALLOC_core__VectorStepper_O = 9,
KIND_BOOTSTRAP_core__Symbol_O = 10,
KIND_LISPALLOC_core__Environment_O = 11,
KIND_LISPALLOC_core__LexicalEnvironment_O = 12,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 13,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 14,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 15,
KIND_LISPALLOC_core__ValueEnvironment_O = 16,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 17,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 18,
KIND_LISPALLOC_core__StackValueEnvironment_O = 19,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 20,
KIND_LISPALLOC_core__MacroletEnvironment_O = 21,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 22,
KIND_LISPALLOC_core__CatchEnvironment_O = 23,
KIND_LISPALLOC_core__BlockEnvironment_O = 24,
KIND_LISPALLOC_core__ActivationFrame_O = 25,
KIND_LISPALLOC_core__FunctionFrame_O = 26,
KIND_LISPALLOC_core__ValueFrame_O = 27,
KIND_LISPALLOC_core__TagbodyFrame_O = 28,
KIND_LISPALLOC_core__GlueEnvironment_O = 29,
KIND_LISPALLOC_core__Path_O = 30,
KIND_LISPALLOC_core__Pointer_O = 31,
KIND_LISPALLOC_core__CandoException_O = 32,
KIND_LISPALLOC_core__Array_O = 33,
KIND_LISPALLOC_core__Vector_O = 34,
KIND_LISPALLOC_core__VectorObjects_O = 35,
KIND_LISPALLOC_core__String_O = 36,
KIND_BOOTSTRAP_core__Str_O = 37,
KIND_LISPALLOC_core__SourceFileInfo_O = 38,
KIND_LISPALLOC_core__LambdaListHandler_O = 39,
KIND_LISPALLOC_core__Creator_O = 40,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 41,
KIND_LISPALLOC_core__WeakHashTable_O = 42,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 43,
KIND_BOOTSTRAP_core__StandardObject_O = 44,
KIND_BOOTSTRAP_core__Metaobject_O = 45,
KIND_BOOTSTRAP_core__Specializer_O = 46,
KIND_BOOTSTRAP_core__Class_O = 47,
KIND_LISPALLOC_core__FileStatus_O = 48,
KIND_LISPALLOC_core__Package_O = 49,
KIND_LISPALLOC_core__SourceManager_O = 50,
KIND_LISPALLOC_core__Functor_O = 51,
KIND_LISPALLOC_core__Closure_O = 52,
KIND_LISPALLOC_core__FunctionClosure_O = 53,
KIND_LISPALLOC_core__CompiledClosure_O = 54,
KIND_LISPALLOC_core__InstanceClosure_O = 55,
KIND_LISPALLOC_core__BuiltinClosure_O = 56,
KIND_LISPALLOC_core__MacroClosure_O = 57,
KIND_LISPALLOC_core__InterpretedClosure_O = 58,
KIND_LISPALLOC_core__Number_O = 59,
KIND_LISPALLOC_core__Real_O = 60,
KIND_LISPALLOC_core__Rational_O = 61,
KIND_LISPALLOC_core__Integer_O = 62,
KIND_LISPALLOC_core__Fixnum_dummy_O = 63,
KIND_LISPALLOC_core__Bignum_O = 64,
KIND_LISPALLOC_core__Ratio_O = 65,
KIND_LISPALLOC_core__Float_O = 66,
KIND_LISPALLOC_core__ShortFloat_O = 67,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 68,
KIND_LISPALLOC_core__DoubleFloat_O = 69,
KIND_LISPALLOC_core__LongFloat_O = 70,
KIND_LISPALLOC_core__Complex_O = 71,
KIND_LISPALLOC_core__DirectoryEntry_O = 72,
KIND_LISPALLOC_core__SourcePosInfo_O = 73,
KIND_LISPALLOC_core__CxxObject_O = 74,
KIND_LISPALLOC_core__Pathname_O = 75,
KIND_LISPALLOC_core__LogicalPathname_O = 76,
KIND_LISPALLOC_core__Iterator_O = 77,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 78,
KIND_LISPALLOC_core__DirectoryIterator_O = 79,
KIND_LISPALLOC_core__Function_O = 80,
KIND_LISPALLOC_core__CompiledFunction_O = 81,
KIND_LISPALLOC_core__Instance_O = 82,
KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O = 83,
KIND_LISPALLOC_core__Cons_O = 84,
KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ = 85,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 86,
KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ = 87,
KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ = 88,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 89,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 90,
KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ = 91,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 92,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 93,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 94,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 95,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 96,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__ = 97,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 98,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 99,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 100,
  KIND_max = 100
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct Cast<core::InvocationHistoryFrameIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 83 83 
      return (kindVal == 83);
  };
};
template <typename FP> struct Cast<core::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 80 82 
      return ((80 <= kindVal) && (kindVal <= 82));
  };
};
template <typename FP> struct Cast<core::FunctionClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 53 58 
      return ((53 <= kindVal) && (kindVal <= 58));
  };
};
template <typename FP> struct Cast<core::Class_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 47 47 
      return (kindVal == 47);
  };
};
template <typename FP> struct Cast<core::Complex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 71 71 
      return (kindVal == 71);
  };
};
template <typename FP> struct Cast<core::CompileTimeEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 17 24 
      return ((17 <= kindVal) && (kindVal <= 24));
  };
};
template <typename FP> struct Cast<core::Iterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 77 79 
      return ((77 <= kindVal) && (kindVal <= 79));
  };
};
template <typename FP> struct Cast<core::Pathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 75 76 
      return ((75 <= kindVal) && (kindVal <= 76));
  };
};
template <typename FP> struct Cast<core::CxxObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 74 74 
      return (kindVal == 74);
  };
};
template <typename FP> struct Cast<core::SourcePosInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 73 73 
      return (kindVal == 73);
  };
};
template <typename FP> struct Cast<core::DirectoryEntry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 72 72 
      return (kindVal == 72);
  };
};
template <typename FP> struct Cast<core::BlockEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 24 24 
      return (kindVal == 24);
  };
};
template <typename FP> struct Cast<core::ValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 16 16 
      return (kindVal == 16);
  };
};
template <typename FP> struct Cast<core::Instance_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 82 82 
      return (kindVal == 82);
  };
};
template <typename FP> struct Cast<core::MacroClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 57 57 
      return (kindVal == 57);
  };
};
template <typename FP> struct Cast<core::Number_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 59 71 
      return ((59 <= kindVal) && (kindVal <= 71));
  };
};
template <typename FP> struct Cast<core::Functor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 51 58 
      return ((51 <= kindVal) && (kindVal <= 58));
  };
};
template <typename FP> struct Cast<core::CatchEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 23 23 
      return (kindVal == 23);
  };
};
template <typename FP> struct Cast<core::SourceManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 50 50 
      return (kindVal == 50);
  };
};
template <typename FP> struct Cast<core::InterpretedClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 58 58 
      return (kindVal == 58);
  };
};
template <typename FP> struct Cast<core::FunctionContainerEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 22 22 
      return (kindVal == 22);
  };
};
template <typename FP> struct Cast<core::Package_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 49 49 
      return (kindVal == 49);
  };
};
template <typename FP> struct Cast<core::Cons_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 84 84 
      return (kindVal == 84);
  };
};
template <typename FP> struct Cast<core::FileStatus_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 48 48 
      return (kindVal == 48);
  };
};
template <typename FP> struct Cast<core::StandardObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 44 47 
      return ((44 <= kindVal) && (kindVal <= 47));
  };
};
template <typename FP> struct Cast<core::GlueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 29 29 
      return (kindVal == 29);
  };
};
template <typename FP> struct Cast<core::BuiltinClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 56 57 
      return ((56 <= kindVal) && (kindVal <= 57));
  };
};
template <typename FP> struct Cast<core::Bignum_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 64 64 
      return (kindVal == 64);
  };
};
template <typename FP> struct Cast<core::InstanceClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 55 55 
      return (kindVal == 55);
  };
};
template <typename FP> struct Cast<core::FunctionValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 15 15 
      return (kindVal == 15);
  };
};
template <typename FP> struct Cast<core::General_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 6 83 
      return ((6 <= kindVal) && (kindVal <= 83));
  };
};
template <typename FP> struct Cast<core::Specializer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 46 47 
      return ((46 <= kindVal) && (kindVal <= 47));
  };
};
template <typename FP> struct Cast<core::WeakHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 42 43 
      return ((42 <= kindVal) && (kindVal <= 43));
  };
};
template <typename FP> struct Cast<core::Closure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 52 58 
      return ((52 <= kindVal) && (kindVal <= 58));
  };
};
template <typename FP> struct Cast<core::TagbodyFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 28 28 
      return (kindVal == 28);
  };
};
template <typename FP> struct Cast<core::LongFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 70 70 
      return (kindVal == 70);
  };
};
template <typename FP> struct Cast<core::CompiledFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 81 81 
      return (kindVal == 81);
  };
};
template <typename FP> struct Cast<core::SymbolToEnumConverter_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 41 41 
      return (kindVal == 41);
  };
};
template <typename FP> struct Cast<core::MacroletEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 21 21 
      return (kindVal == 21);
  };
};
template <typename FP> struct Cast<core::SymbolMacroletEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 20 20 
      return (kindVal == 20);
  };
};
template <typename FP> struct Cast<core::Creator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 40 40 
      return (kindVal == 40);
  };
};
template <typename FP> struct Cast<core::LambdaListHandler_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 39 39 
      return (kindVal == 39);
  };
};
template <typename FP> struct Cast<core::SourceFileInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 38 38 
      return (kindVal == 38);
  };
};
template <typename FP> struct Cast<core::Array_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 33 37 
      return ((33 <= kindVal) && (kindVal <= 37));
  };
};
template <typename FP> struct Cast<core::T_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 5 84 
      return ((5 <= kindVal) && (kindVal <= 84));
  };
};
template <typename FP> struct Cast<core::CandoException_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 32 32 
      return (kindVal == 32);
  };
};
template <typename FP> struct Cast<core::Ratio_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 65 65 
      return (kindVal == 65);
  };
};
template <typename FP> struct Cast<core::Vector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 34 37 
      return ((34 <= kindVal) && (kindVal <= 37));
  };
};
template <typename FP> struct Cast<core::VectorStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 9 9 
      return (kindVal == 9);
  };
};
template <typename FP> struct Cast<core::TagbodyEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 14 14 
      return (kindVal == 14);
  };
};
template <typename FP> struct Cast<core::Fixnum_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 63 63 
      return (kindVal == 63);
  };
};
template <typename FP> struct Cast<core::RuntimeVisibleEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 13 16 
      return ((13 <= kindVal) && (kindVal <= 16));
  };
};
template <typename FP> struct Cast<core::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 31 31 
      return (kindVal == 31);
  };
};
template <typename FP> struct Cast<core::StackValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 19 19 
      return (kindVal == 19);
  };
};
template <typename FP> struct Cast<core::Path_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 30 30 
      return (kindVal == 30);
  };
};
template <typename FP> struct Cast<core::Environment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 11 29 
      return ((11 <= kindVal) && (kindVal <= 29));
  };
};
template <typename FP> struct Cast<core::ValueFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 27 27 
      return (kindVal == 27);
  };
};
template <typename FP> struct Cast<core::ActivationFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 25 28 
      return ((25 <= kindVal) && (kindVal <= 28));
  };
};
template <typename FP> struct Cast<core::LexicalEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 12 24 
      return ((12 <= kindVal) && (kindVal <= 24));
  };
};
template <typename FP> struct Cast<core::DoubleFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 69 69 
      return (kindVal == 69);
  };
};
template <typename FP> struct Cast<core::String_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 36 37 
      return ((36 <= kindVal) && (kindVal <= 37));
  };
};
template <typename FP> struct Cast<core::SingleFloat_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 68 68 
      return (kindVal == 68);
  };
};
template <typename FP> struct Cast<core::ShortFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 67 67 
      return (kindVal == 67);
  };
};
template <typename FP> struct Cast<core::CompiledClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 54 54 
      return (kindVal == 54);
  };
};
template <typename FP> struct Cast<core::Real_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 60 70 
      return ((60 <= kindVal) && (kindVal <= 70));
  };
};
template <typename FP> struct Cast<core::FunctionFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 26 26 
      return (kindVal == 26);
  };
};
template <typename FP> struct Cast<core::VectorObjects_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 35 35 
      return (kindVal == 35);
  };
};
template <typename FP> struct Cast<core::Float_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 66 70 
      return ((66 <= kindVal) && (kindVal <= 70));
  };
};
template <typename FP> struct Cast<core::Symbol_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 10 10 
      return (kindVal == 10);
  };
};
template <typename FP> struct Cast<core::DirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 79 79 
      return (kindVal == 79);
  };
};
template <typename FP> struct Cast<core::WeakKeyHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 43 43 
      return (kindVal == 43);
  };
};
template <typename FP> struct Cast<core::Str_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 37 37 
      return (kindVal == 37);
  };
};
template <typename FP> struct Cast<core::Metaobject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 45 47 
      return ((45 <= kindVal) && (kindVal <= 47));
  };
};
template <typename FP> struct Cast<core::ConsStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 8 8 
      return (kindVal == 8);
  };
};
template <typename FP> struct Cast<core::RecursiveDirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 78 78 
      return (kindVal == 78);
  };
};
template <typename FP> struct Cast<core::Rational_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 61 65 
      return ((61 <= kindVal) && (kindVal <= 65));
  };
};
template <typename FP> struct Cast<core::Integer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 62 64 
      return ((62 <= kindVal) && (kindVal <= 64));
  };
};
template <typename FP> struct Cast<core::SequenceStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 7 9 
      return ((7 <= kindVal) && (kindVal <= 9));
  };
};
template <typename FP> struct Cast<core::UnwindProtectEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 18 18 
      return (kindVal == 18);
  };
};
template <typename FP> struct Cast<core::LogicalPathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 76 76 
      return (kindVal == 76);
  };
};
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
template <> class gctools::GCKind<core::InvocationHistoryFrameIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O ;
};
template <> class gctools::GCKind<core::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Function_O ;
};
template <> class gctools::GCKind<core::FunctionClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionClosure_O ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::KeywordArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ ;
};
template <> class gctools::GCKind<core::CompileTimeEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompileTimeEnvironment_O ;
};
template <> class gctools::GCKind<core::Iterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Iterator_O ;
};
template <> class gctools::GCKind<core::Pathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pathname_O ;
};
template <> class gctools::GCKind<core::CxxObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CxxObject_O ;
};
template <> class gctools::GCKind<core::SourcePosInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourcePosInfo_O ;
};
template <> class gctools::GCKind<core::DirectoryEntry_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DirectoryEntry_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::RequiredArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ ;
};
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<core::ValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueEnvironment_O ;
};
template <> class gctools::GCKind<core::Instance_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Instance_O ;
};
template <> class gctools::GCKind<core::MacroClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroClosure_O ;
};
template <> class gctools::GCKind<core::Number_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Number_O ;
};
template <> class gctools::GCKind<core::Functor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Functor_O ;
};
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::AuxArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ ;
};
template <> class gctools::GCKind<core::SourceManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceManager_O ;
};
template <> class gctools::GCKind<core::InterpretedClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InterpretedClosure_O ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<core::Package_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Package_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::FileStatus_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FileStatus_O ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
template <> class gctools::GCKind<core::BuiltinClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BuiltinClosure_O ;
};
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
};
template <> class gctools::GCKind<core::InstanceClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InstanceClosure_O ;
};
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<core::General_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__General_O ;
};
template <> class gctools::GCKind<core::Specializer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Specializer_O ;
};
template <> class gctools::GCKind<core::WeakHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakHashTable_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::ExceptionEntry>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ ;
};
template <> class gctools::GCKind<core::Closure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Closure_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
};
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::OptionalArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ ;
};
template <> class gctools::GCKind<core::LongFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LongFloat_O ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::SymbolMacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolMacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::Creator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Creator_O ;
};
template <> class gctools::GCKind<core::LambdaListHandler_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LambdaListHandler_O ;
};
template <> class gctools::GCKind<core::SourceFileInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceFileInfo_O ;
};
template <> class gctools::GCKind<core::Array_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Array_O ;
};
template <> class gctools::GCKind<core::T_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__T_O ;
};
template <> class gctools::GCKind<core::CandoException_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CandoException_O ;
};
template <> class gctools::GCKind<core::Ratio_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Ratio_O ;
};
template <> class gctools::GCKind<core::Vector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Vector_O ;
};
template <> class gctools::GCKind<core::VectorStepper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorStepper_O ;
};
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<core::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pointer_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::DynamicBinding>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ ;
};
template <> class gctools::GCKind<core::StackValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StackValueEnvironment_O ;
};
template <> class gctools::GCKind<core::Path_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Path_O ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ ;
};
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::ActivationFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ActivationFrame_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<core::DoubleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DoubleFloat_O ;
};
template <> class gctools::GCKind<core::String_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__String_O ;
};
template <> class gctools::GCKind<core::SingleFloat_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_dummy_O ;
};
template <> class gctools::GCKind<core::ShortFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ShortFloat_O ;
};
template <> class gctools::GCKind<core::CompiledClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledClosure_O ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<core::VectorObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjects_O ;
};
template <> class gctools::GCKind<core::Float_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Float_O ;
};
template <> class gctools::GCKind<core::Symbol_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Symbol_O ;
};
template <> class gctools::GCKind<core::DirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DirectoryIterator_O ;
};
template <> class gctools::GCKind<core::WeakKeyHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyHashTable_O ;
};
template <> class gctools::GCKind<core::Str_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Str_O ;
};
template <> class gctools::GCKind<core::Metaobject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Metaobject_O ;
};
template <> class gctools::GCKind<core::ConsStepper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ConsStepper_O ;
};
template <> class gctools::GCKind<core::RecursiveDirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RecursiveDirectoryIterator_O ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<core::SequenceStepper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SequenceStepper_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::LogicalPathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LogicalPathname_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolClassPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_OBJ_SCAN)
