#ifdef DECLARE_FORWARDS
 namespace core {
    class CoreExposer_O;
    class VectorObjectsWithFillPtr_O;
    class LogicalPathname_O;
    class ExceptionEntry;
    class UnwindProtectEnvironment_O;
    class Integer_O;
    class Record_O;
    class Exposer_O;
    class Rational_O;
    class RecursiveDirectoryIterator_O;
    class Metaobject_O;
    class Str_O;
    class WeakKeyHashTable_O;
    class Symbol_O;
    class DirectoryIterator_O;
    class Float_O;
    class VectorObjects_O;
    class HashTableEql_O;
    class FunctionFrame_O;
    class Real_O;
    class CompiledClosure_O;
    class ShortFloat_O;
    class SingleFloat_dummy_O;
    class SaveArchive_O;
    class String_O;
    class DoubleFloat_O;
    class SimpleBitVector_O;
    class LexicalEnvironment_O;
    class ActivationFrame_O;
    class HashTableEqual_O;
    class ValueFrame_O;
    class Environment_O;
    class Path_O;
    class StackValueEnvironment_O;
    class Pointer_O;
    class SymbolClassPair;
    class HashTableEqualp_O;
    class RuntimeVisibleEnvironment_O;
    class NamedFunction_O;
    class Fixnum_dummy_O;
    class TagbodyEnvironment_O;
    class Vector_O;
    class Ratio_O;
    class CandoException_O;
    class T_O;
    class Array_O;
    class InstanceCreator_O;
    class SourceFileInfo_O;
    class Creator_O;
    class SymbolMacroletEnvironment_O;
    class CacheRecord;
    class MacroletEnvironment_O;
    class SNode_O;
    class LeafSNode_O;
    class SymbolToEnumConverter_O;
    class CompiledFunction_O;
    class LongFloat_O;
    class TagbodyFrame_O;
    class Closure_O;
    class WeakHashTable_O;
    class Specializer_O;
    class General_O;
    class DynamicBinding;
    class BitVector_O;
    class BranchSNode_O;
    class FunctionValueEnvironment_O;
    class Bignum_O;
    class BuiltinClosure_O;
    class GlueEnvironment_O;
    class HashTableEq_O;
    class LoadArchive_O;
    class StandardObject_O;
    class FileStatus_O;
    class ClosureWithFrame_O;
    class Cons_O;
    class FunctionContainerEnvironment_O;
    class InterpretedClosure_O;
    class SourceManager_O;
    class CatchEnvironment_O;
    class Number_O;
    class MacroClosure_O;
    class Instance_O;
    class ValueEnvironment_O;
    class BlockEnvironment_O;
    class HashTable_O;
    class DirectoryEntry_O;
    class SourcePosInfo_O;
    class CxxObject_O;
    class Pathname_O;
    class Iterator_O;
    class CompileTimeEnvironment_O;
    class Complex_O;
    class Archive_O;
    class BitVectorWithFillPtr_O;
    class Class_O;
    class FunctionClosure_O;
    class Function_O;
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
KIND_LISPALLOC_core__Exposer_O = 7,
KIND_LISPALLOC_core__CoreExposer_O = 8,
KIND_LISPALLOC_core__Record_O = 9,
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
KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O = 36,
KIND_LISPALLOC_core__String_O = 37,
KIND_BOOTSTRAP_core__Str_O = 38,
KIND_LISPALLOC_core__BitVector_O = 39,
KIND_LISPALLOC_core__SimpleBitVector_O = 40,
KIND_LISPALLOC_core__BitVectorWithFillPtr_O = 41,
KIND_LISPALLOC_core__SourceFileInfo_O = 42,
KIND_LISPALLOC_core__Creator_O = 43,
KIND_LISPALLOC_core__InstanceCreator_O = 44,
KIND_LISPALLOC_core__SNode_O = 45,
KIND_LISPALLOC_core__LeafSNode_O = 46,
KIND_LISPALLOC_core__BranchSNode_O = 47,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 48,
KIND_LISPALLOC_core__WeakHashTable_O = 49,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 50,
KIND_BOOTSTRAP_core__StandardObject_O = 51,
KIND_BOOTSTRAP_core__Metaobject_O = 52,
KIND_BOOTSTRAP_core__Specializer_O = 53,
KIND_BOOTSTRAP_core__Class_O = 54,
KIND_LISPALLOC_core__FileStatus_O = 55,
KIND_LISPALLOC_core__SourceManager_O = 56,
KIND_LISPALLOC_core__Number_O = 57,
KIND_LISPALLOC_core__Real_O = 58,
KIND_LISPALLOC_core__Rational_O = 59,
KIND_LISPALLOC_core__Integer_O = 60,
KIND_LISPALLOC_core__Fixnum_dummy_O = 61,
KIND_LISPALLOC_core__Bignum_O = 62,
KIND_LISPALLOC_core__Ratio_O = 63,
KIND_LISPALLOC_core__Float_O = 64,
KIND_LISPALLOC_core__ShortFloat_O = 65,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 66,
KIND_LISPALLOC_core__DoubleFloat_O = 67,
KIND_LISPALLOC_core__LongFloat_O = 68,
KIND_LISPALLOC_core__Complex_O = 69,
KIND_LISPALLOC_core__HashTable_O = 70,
KIND_LISPALLOC_core__HashTableEql_O = 71,
KIND_LISPALLOC_core__HashTableEqual_O = 72,
KIND_LISPALLOC_core__HashTableEqualp_O = 73,
KIND_LISPALLOC_core__HashTableEq_O = 74,
KIND_LISPALLOC_core__DirectoryEntry_O = 75,
KIND_LISPALLOC_core__SourcePosInfo_O = 76,
KIND_LISPALLOC_core__CxxObject_O = 77,
KIND_LISPALLOC_core__Pathname_O = 78,
KIND_LISPALLOC_core__LogicalPathname_O = 79,
KIND_LISPALLOC_core__Iterator_O = 80,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 81,
KIND_LISPALLOC_core__DirectoryIterator_O = 82,
KIND_LISPALLOC_core__Archive_O = 83,
KIND_LISPALLOC_core__SaveArchive_O = 84,
KIND_LISPALLOC_core__LoadArchive_O = 85,
KIND_LISPALLOC_core__Function_O = 86,
KIND_LISPALLOC_core__NamedFunction_O = 87,
KIND_LISPALLOC_core__Closure_O = 88,
KIND_LISPALLOC_core__CompiledFunction_O = 89,
KIND_LISPALLOC_core__FunctionClosure_O = 90,
KIND_LISPALLOC_core__BuiltinClosure_O = 91,
KIND_LISPALLOC_core__MacroClosure_O = 92,
KIND_LISPALLOC_core__ClosureWithFrame_O = 93,
KIND_LISPALLOC_core__CompiledClosure_O = 94,
KIND_LISPALLOC_core__InterpretedClosure_O = 95,
KIND_LISPALLOC_core__Instance_O = 96,
KIND_LISPALLOC_core__Cons_O = 97,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 98,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 99,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 100,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 101,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 102,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 103,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__ = 104,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 105,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 106,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 107,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 108,
  KIND_max = 108
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct Cast<core::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 86 96 
      return ((86 <= kindVal) && (kindVal <= 96));
  };
};
template <typename FP> struct Cast<core::FunctionClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 90 95 
      return ((90 <= kindVal) && (kindVal <= 95));
  };
};
template <typename FP> struct Cast<core::Class_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 54 54 
      return (kindVal == 54);
  };
};
template <typename FP> struct Cast<core::BitVectorWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 41 41 
      return (kindVal == 41);
  };
};
template <typename FP> struct Cast<core::Archive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 83 85 
      return ((83 <= kindVal) && (kindVal <= 85));
  };
};
template <typename FP> struct Cast<core::Complex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 69 69 
      return (kindVal == 69);
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
      // low high --> 80 82 
      return ((80 <= kindVal) && (kindVal <= 82));
  };
};
template <typename FP> struct Cast<core::Pathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 78 79 
      return ((78 <= kindVal) && (kindVal <= 79));
  };
};
template <typename FP> struct Cast<core::CxxObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 77 77 
      return (kindVal == 77);
  };
};
template <typename FP> struct Cast<core::SourcePosInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 76 76 
      return (kindVal == 76);
  };
};
template <typename FP> struct Cast<core::DirectoryEntry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 75 75 
      return (kindVal == 75);
  };
};
template <typename FP> struct Cast<core::HashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 70 74 
      return ((70 <= kindVal) && (kindVal <= 74));
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
      // low high --> 96 96 
      return (kindVal == 96);
  };
};
template <typename FP> struct Cast<core::MacroClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 92 92 
      return (kindVal == 92);
  };
};
template <typename FP> struct Cast<core::Number_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 57 69 
      return ((57 <= kindVal) && (kindVal <= 69));
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
      // low high --> 56 56 
      return (kindVal == 56);
  };
};
template <typename FP> struct Cast<core::InterpretedClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 95 95 
      return (kindVal == 95);
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
template <typename FP> struct Cast<core::Cons_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 97 97 
      return (kindVal == 97);
  };
};
template <typename FP> struct Cast<core::ClosureWithFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 93 95 
      return ((93 <= kindVal) && (kindVal <= 95));
  };
};
template <typename FP> struct Cast<core::FileStatus_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 55 55 
      return (kindVal == 55);
  };
};
template <typename FP> struct Cast<core::StandardObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 51 54 
      return ((51 <= kindVal) && (kindVal <= 54));
  };
};
template <typename FP> struct Cast<core::LoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 85 85 
      return (kindVal == 85);
  };
};
template <typename FP> struct Cast<core::HashTableEq_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 74 74 
      return (kindVal == 74);
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
      // low high --> 91 92 
      return ((91 <= kindVal) && (kindVal <= 92));
  };
};
template <typename FP> struct Cast<core::Bignum_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 62 62 
      return (kindVal == 62);
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
template <typename FP> struct Cast<core::BranchSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 47 47 
      return (kindVal == 47);
  };
};
template <typename FP> struct Cast<core::BitVector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 39 41 
      return ((39 <= kindVal) && (kindVal <= 41));
  };
};
template <typename FP> struct Cast<core::General_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 6 96 
      return ((6 <= kindVal) && (kindVal <= 96));
  };
};
template <typename FP> struct Cast<core::Specializer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 53 54 
      return ((53 <= kindVal) && (kindVal <= 54));
  };
};
template <typename FP> struct Cast<core::WeakHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 49 50 
      return ((49 <= kindVal) && (kindVal <= 50));
  };
};
template <typename FP> struct Cast<core::Closure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 88 95 
      return ((88 <= kindVal) && (kindVal <= 95));
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
      // low high --> 68 68 
      return (kindVal == 68);
  };
};
template <typename FP> struct Cast<core::CompiledFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 89 89 
      return (kindVal == 89);
  };
};
template <typename FP> struct Cast<core::SymbolToEnumConverter_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 48 48 
      return (kindVal == 48);
  };
};
template <typename FP> struct Cast<core::LeafSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 46 46 
      return (kindVal == 46);
  };
};
template <typename FP> struct Cast<core::SNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 45 47 
      return ((45 <= kindVal) && (kindVal <= 47));
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
      // low high --> 43 44 
      return ((43 <= kindVal) && (kindVal <= 44));
  };
};
template <typename FP> struct Cast<core::SourceFileInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 42 42 
      return (kindVal == 42);
  };
};
template <typename FP> struct Cast<core::InstanceCreator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 44 44 
      return (kindVal == 44);
  };
};
template <typename FP> struct Cast<core::Array_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 33 41 
      return ((33 <= kindVal) && (kindVal <= 41));
  };
};
template <typename FP> struct Cast<core::T_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 5 97 
      return ((5 <= kindVal) && (kindVal <= 97));
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
      // low high --> 63 63 
      return (kindVal == 63);
  };
};
template <typename FP> struct Cast<core::Vector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 34 41 
      return ((34 <= kindVal) && (kindVal <= 41));
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
      // low high --> 61 61 
      return (kindVal == 61);
  };
};
template <typename FP> struct Cast<core::NamedFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 87 95 
      return ((87 <= kindVal) && (kindVal <= 95));
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
template <typename FP> struct Cast<core::HashTableEqualp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 73 73 
      return (kindVal == 73);
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
template <typename FP> struct Cast<core::HashTableEqual_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 72 72 
      return (kindVal == 72);
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
template <typename FP> struct Cast<core::SimpleBitVector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 40 40 
      return (kindVal == 40);
  };
};
template <typename FP> struct Cast<core::DoubleFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 67 67 
      return (kindVal == 67);
  };
};
template <typename FP> struct Cast<core::String_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 37 38 
      return ((37 <= kindVal) && (kindVal <= 38));
  };
};
template <typename FP> struct Cast<core::SaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 84 84 
      return (kindVal == 84);
  };
};
template <typename FP> struct Cast<core::SingleFloat_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 66 66 
      return (kindVal == 66);
  };
};
template <typename FP> struct Cast<core::ShortFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 65 65 
      return (kindVal == 65);
  };
};
template <typename FP> struct Cast<core::CompiledClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 94 94 
      return (kindVal == 94);
  };
};
template <typename FP> struct Cast<core::Real_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 58 68 
      return ((58 <= kindVal) && (kindVal <= 68));
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
template <typename FP> struct Cast<core::HashTableEql_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 71 71 
      return (kindVal == 71);
  };
};
template <typename FP> struct Cast<core::VectorObjects_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 35 36 
      return ((35 <= kindVal) && (kindVal <= 36));
  };
};
template <typename FP> struct Cast<core::Float_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 64 68 
      return ((64 <= kindVal) && (kindVal <= 68));
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
      // low high --> 82 82 
      return (kindVal == 82);
  };
};
template <typename FP> struct Cast<core::WeakKeyHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 50 50 
      return (kindVal == 50);
  };
};
template <typename FP> struct Cast<core::Metaobject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 52 54 
      return ((52 <= kindVal) && (kindVal <= 54));
  };
};
template <typename FP> struct Cast<core::Str_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 38 38 
      return (kindVal == 38);
  };
};
template <typename FP> struct Cast<core::RecursiveDirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 81 81 
      return (kindVal == 81);
  };
};
template <typename FP> struct Cast<core::Rational_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 59 63 
      return ((59 <= kindVal) && (kindVal <= 63));
  };
};
template <typename FP> struct Cast<core::Record_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 9 9 
      return (kindVal == 9);
  };
};
template <typename FP> struct Cast<core::Exposer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 7 8 
      return ((7 <= kindVal) && (kindVal <= 8));
  };
};
template <typename FP> struct Cast<core::Integer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 60 62 
      return ((60 <= kindVal) && (kindVal <= 62));
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
template <typename FP> struct Cast<core::VectorObjectsWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 36 36 
      return (kindVal == 36);
  };
};
template <typename FP> struct Cast<core::LogicalPathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 79 79 
      return (kindVal == 79);
  };
};
template <typename FP> struct Cast<core::CoreExposer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 8 8 
      return (kindVal == 8);
  };
};
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
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
template <> class gctools::GCKind<core::BitVectorWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BitVectorWithFillPtr_O ;
};
template <> class gctools::GCKind<core::Archive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Archive_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
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
template <> class gctools::GCKind<core::HashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTable_O ;
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
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
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
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::ClosureWithFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ClosureWithFrame_O ;
};
template <> class gctools::GCKind<core::FileStatus_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FileStatus_O ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<core::LoadArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LoadArchive_O ;
};
template <> class gctools::GCKind<core::HashTableEq_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEq_O ;
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
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<core::BranchSNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BranchSNode_O ;
};
template <> class gctools::GCKind<core::BitVector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BitVector_O ;
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
template <> class gctools::GCKind<core::LongFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LongFloat_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
template <> class gctools::GCKind<core::LeafSNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LeafSNode_O ;
};
template <> class gctools::GCKind<core::SNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SNode_O ;
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
template <> class gctools::GCKind<core::SourceFileInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceFileInfo_O ;
};
template <> class gctools::GCKind<core::InstanceCreator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InstanceCreator_O ;
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
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<core::NamedFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__NamedFunction_O ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<core::HashTableEqualp_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEqualp_O ;
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
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::HashTableEqual_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEqual_O ;
};
template <> class gctools::GCKind<core::ActivationFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ActivationFrame_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<core::SimpleBitVector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SimpleBitVector_O ;
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
template <> class gctools::GCKind<core::SaveArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SaveArchive_O ;
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
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<core::HashTableEql_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEql_O ;
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
template <> class gctools::GCKind<core::Metaobject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Metaobject_O ;
};
template <> class gctools::GCKind<core::Str_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Str_O ;
};
template <> class gctools::GCKind<core::RecursiveDirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RecursiveDirectoryIterator_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<core::Record_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Record_O ;
};
template <> class gctools::GCKind<core::Exposer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Exposer_O ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::VectorObjectsWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O ;
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
template <> class gctools::GCKind<core::CoreExposer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CoreExposer_O ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_OBJ_SCAN)
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)
{ class_kind, KIND_BOOTSTRAP_core__T_O, sizeof(core::T_O), 0, "core::T_O" },
{ class_kind, KIND_LISPALLOC_core__General_O, sizeof(core::General_O), 0, "core::General_O" },
{ class_kind, KIND_LISPALLOC_core__Exposer_O, sizeof(core::Exposer_O), 0, "core::Exposer_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(core::Exposer_O),_Package), "_Package" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__CoreExposer_O, sizeof(core::CoreExposer_O), 0, "core::CoreExposer_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(core::CoreExposer_O),_Package), "_Package" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Record_O, sizeof(core::Record_O), 0, "core::Record_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_alist), "_alist" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_replacement_table), "_replacement_table" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_Seen), "_Seen" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_BOOTSTRAP_core__Symbol_O, sizeof(core::Symbol_O), 0, "core::Symbol_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Str_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Name), "_Name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_HomePackage), "_HomePackage" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Value), "_Value" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Function), "_Function" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_SetfFunction), "_SetfFunction" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_IsSpecial), "_IsSpecial" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_IsConstant), "_IsConstant" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_ReadOnlyFunction), "_ReadOnlyFunction" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_PropertyList), "_PropertyList" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Environment_O, sizeof(core::Environment_O), 0, "core::Environment_O" },
{ class_kind, KIND_LISPALLOC_core__LexicalEnvironment_O, sizeof(core::LexicalEnvironment_O), 0, "core::LexicalEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O, sizeof(core::RuntimeVisibleEnvironment_O), 0, "core::RuntimeVisibleEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__TagbodyEnvironment_O, sizeof(core::TagbodyEnvironment_O), 0, "core::TagbodyEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Tags), "_Tags" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_TagCode._Vector._Contents), "_TagCode._Vector._Contents" }, // public: (T T T) fixable: TAGGED-POINTER-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ActivationFrame), "_ActivationFrame" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__FunctionValueEnvironment_O, sizeof(core::FunctionValueEnvironment_O), 0, "core::FunctionValueEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionIndices), "_FunctionIndices" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::FunctionFrame_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionFrame), "_FunctionFrame" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__ValueEnvironment_O, sizeof(core::ValueEnvironment_O), 0, "core::ValueEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_SymbolIndex), "_SymbolIndex" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ValueFrame_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ActivationFrame), "_ActivationFrame" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__CompileTimeEnvironment_O, sizeof(core::CompileTimeEnvironment_O), 0, "core::CompileTimeEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__UnwindProtectEnvironment_O, sizeof(core::UnwindProtectEnvironment_O), 0, "core::UnwindProtectEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_CleanupForm), "_CleanupForm" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__StackValueEnvironment_O, sizeof(core::StackValueEnvironment_O), 0, "core::StackValueEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Values), "_Values" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__SymbolMacroletEnvironment_O, sizeof(core::SymbolMacroletEnvironment_O), 0, "core::SymbolMacroletEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Macros), "_Macros" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__MacroletEnvironment_O, sizeof(core::MacroletEnvironment_O), 0, "core::MacroletEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Macros), "_Macros" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__FunctionContainerEnvironment_O, sizeof(core::FunctionContainerEnvironment_O), 0, "core::FunctionContainerEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__CatchEnvironment_O, sizeof(core::CatchEnvironment_O), 0, "core::CatchEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__BlockEnvironment_O, sizeof(core::BlockEnvironment_O), 0, "core::BlockEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_ParentEnvironment), "_ParentEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_Metadata), "_Metadata" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_BlockSymbol), "_BlockSymbol" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__ActivationFrame_O, sizeof(core::ActivationFrame_O), 0, "core::ActivationFrame_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ActivationFrame_O),_Parent), "_Parent" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__FunctionFrame_O, sizeof(core::FunctionFrame_O), 0, "core::FunctionFrame_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Parent), "_Parent" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Objects._Data), "_Objects._Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Objects._Capacity), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Objects._Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__ValueFrame_O, sizeof(core::ValueFrame_O), 0, "core::ValueFrame_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Parent), "_Parent" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_DebuggingInfo), "_DebuggingInfo" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Objects._Data), "_Objects._Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Objects._Capacity), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Objects._Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__TagbodyFrame_O, sizeof(core::TagbodyFrame_O), 0, "core::TagbodyFrame_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_Parent), "_Parent" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__GlueEnvironment_O, sizeof(core::GlueEnvironment_O), 0, "core::GlueEnvironment_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Map), "_Map" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Args), "_Args" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Path_O, sizeof(core::Path_O), 0, "core::Path_O" },
{ class_kind, KIND_LISPALLOC_core__Pointer_O, sizeof(core::Pointer_O), 0, "core::Pointer_O" },
{ class_kind, KIND_LISPALLOC_core__CandoException_O, sizeof(core::CandoException_O), 0, "core::CandoException_O" },
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::CandoException_O),_message._Contents), "_message._Contents" }, // public: (T T) fixable: TAGGED-POINTER-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Array_O, sizeof(core::Array_O), 0, "core::Array_O" },
{ class_kind, KIND_LISPALLOC_core__Vector_O, sizeof(core::Vector_O), 0, "core::Vector_O" },
{ class_kind, KIND_LISPALLOC_core__VectorObjects_O, sizeof(core::VectorObjects_O), 0, "core::VectorObjects_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_ElementType), "_ElementType" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_Adjustable), "_Adjustable" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_Values._Vector._Contents), "_Values._Vector._Contents" }, // public: (T T T) fixable: TAGGED-POINTER-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O, sizeof(core::VectorObjectsWithFillPtr_O), 0, "core::VectorObjectsWithFillPtr_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_ElementType), "_ElementType" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_Adjustable), "_Adjustable" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_Values._Vector._Contents), "_Values._Vector._Contents" }, // public: (T T T) fixable: TAGGED-POINTER-FIX good-name: T
// not-exposing {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_FillPtr), "_FillPtr" }, // public: (NIL) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__String_O, sizeof(core::String_O), 0, "core::String_O" },
{ class_kind, KIND_BOOTSTRAP_core__Str_O, sizeof(core::Str_O), 0, "core::Str_O" },
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::Str_O),_Contents._Contents), "_Contents._Contents" }, // public: (NIL T) fixable: TAGGED-POINTER-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__BitVector_O, sizeof(core::BitVector_O), 0, "core::BitVector_O" },
{ class_kind, KIND_LISPALLOC_core__SimpleBitVector_O, sizeof(core::SimpleBitVector_O), 0, "core::SimpleBitVector_O" },
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SimpleBitVector_O),_length), "_length" }, // public: (NIL) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__BitVectorWithFillPtr_O, sizeof(core::BitVectorWithFillPtr_O), 0, "core::BitVectorWithFillPtr_O" },
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::BitVectorWithFillPtr_O),_fill_ptr), "_fill_ptr" }, // public: (NIL) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::BitVectorWithFillPtr_O),_adjustable), "_adjustable" }, // public: (NIL) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__SourceFileInfo_O, sizeof(core::SourceFileInfo_O), 0, "core::SourceFileInfo_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Pathname_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_pathname), "_pathname" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_FileHandle), "_FileHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugNamestring), "_SourceDebugNamestring" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugOffset), "_SourceDebugOffset" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_TrackLineno), "_TrackLineno" }, // public: (T) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__Creator_O, sizeof(core::Creator_O), 0, "core::Creator_O" },
{ class_kind, KIND_LISPALLOC_core__InstanceCreator_O, sizeof(core::InstanceCreator_O), 0, "core::InstanceCreator_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::InstanceCreator_O),_className), "_className" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__SNode_O, sizeof(core::SNode_O), 0, "core::SNode_O" },
// not-exposing {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SNode_O),_RefCount), "_RefCount" }, // public: (NIL) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__LeafSNode_O, sizeof(core::LeafSNode_O), 0, "core::LeafSNode_O" },
// not-exposing {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LeafSNode_O),_RefCount), "_RefCount" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LeafSNode_O),_Object), "_Object" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__BranchSNode_O, sizeof(core::BranchSNode_O), 0, "core::BranchSNode_O" },
// not-exposing {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_RefCount), "_RefCount" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_Kind), "_Kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_SNodePList), "_SNodePList" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, POINTER_OFFSET, sizeof(UnknownType), offsetof(SAFE_TYPE_MACRO(core::BranchSNode_O),_VectorSNodes.theObject), "_VectorSNodes.theObject" }, // public: (T T) fixable: RAW-TAGGED-POINTER-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__SymbolToEnumConverter_O, sizeof(core::SymbolToEnumConverter_O), 0, "core::SymbolToEnumConverter_O" },
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_WhatTheEnumsRepresent._Contents), "_WhatTheEnumsRepresent._Contents" }, // public: (T T) fixable: TAGGED-POINTER-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToSymbol), "_EnumToSymbol" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_ArchiveSymbolToEnum), "_ArchiveSymbolToEnum" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToArchiveSymbol), "_EnumToArchiveSymbol" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_SymbolToEnum), "_SymbolToEnum" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__WeakHashTable_O, sizeof(core::WeakHashTable_O), 0, "core::WeakHashTable_O" },
{ class_kind, KIND_LISPALLOC_core__WeakKeyHashTable_O, sizeof(core::WeakKeyHashTable_O), 0, "core::WeakKeyHashTable_O" },
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Length), "_HashTable._Length" }, // public: (T T) fixable: NIL good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::WeakLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Keys), "_HashTable._Keys" }, // public: (T T) fixable: TAGGED-POINTER-FIX good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::StrongLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Values), "_HashTable._Values" }, // public: (T T) fixable: TAGGED-POINTER-FIX good-name: T
 {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._epoch), "_HashTable._LocationDependency._epoch" }, // public: (T T T) fixable: NIL good-name: T
 {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._rs), "_HashTable._LocationDependency._rs" }, // public: (T T T) fixable: NIL good-name: T
{ class_kind, KIND_BOOTSTRAP_core__StandardObject_O, sizeof(core::StandardObject_O), 0, "core::StandardObject_O" },
{ class_kind, KIND_BOOTSTRAP_core__Metaobject_O, sizeof(core::Metaobject_O), 0, "core::Metaobject_O" },
{ class_kind, KIND_BOOTSTRAP_core__Specializer_O, sizeof(core::Specializer_O), 0, "core::Specializer_O" },
{ class_kind, KIND_BOOTSTRAP_core__Class_O, sizeof(core::Class_O), 0, "core::Class_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_Signature_ClassSlots), "_Signature_ClassSlots" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_theCreator), "_theCreator" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" }, // public: (T T T) fixable: TAGGED-POINTER-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__FileStatus_O, sizeof(core::FileStatus_O), 0, "core::FileStatus_O" },
{ class_kind, KIND_LISPALLOC_core__SourceManager_O, sizeof(core::SourceManager_O), 0, "core::SourceManager_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::WeakKeyHashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SourceManager_O),_SourcePosInfo), "_SourcePosInfo" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Number_O, sizeof(core::Number_O), 0, "core::Number_O" },
{ class_kind, KIND_LISPALLOC_core__Real_O, sizeof(core::Real_O), 0, "core::Real_O" },
{ class_kind, KIND_LISPALLOC_core__Rational_O, sizeof(core::Rational_O), 0, "core::Rational_O" },
{ class_kind, KIND_LISPALLOC_core__Integer_O, sizeof(core::Integer_O), 0, "core::Integer_O" },
{ class_kind, KIND_LISPALLOC_core__Fixnum_dummy_O, sizeof(core::Fixnum_dummy_O), 0, "core::Fixnum_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__Bignum_O, sizeof(core::Bignum_O), 0, "core::Bignum_O" },
{ class_kind, KIND_LISPALLOC_core__Ratio_O, sizeof(core::Ratio_O), 0, "core::Ratio_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_numerator), "_numerator" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_denominator), "_denominator" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Float_O, sizeof(core::Float_O), 0, "core::Float_O" },
{ class_kind, KIND_LISPALLOC_core__ShortFloat_O, sizeof(core::ShortFloat_O), 0, "core::ShortFloat_O" },
// not-exposing {  fixed_field, ctype_float, sizeof(float), offsetof(SAFE_TYPE_MACRO(core::ShortFloat_O),_Value), "_Value" }, // public: (NIL) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__SingleFloat_dummy_O, sizeof(core::SingleFloat_dummy_O), 0, "core::SingleFloat_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__DoubleFloat_O, sizeof(core::DoubleFloat_O), 0, "core::DoubleFloat_O" },
// not-exposing {  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::DoubleFloat_O),_Value), "_Value" }, // public: (NIL) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__LongFloat_O, sizeof(core::LongFloat_O), 0, "core::LongFloat_O" },
{ class_kind, KIND_LISPALLOC_core__Complex_O, sizeof(core::Complex_O), 0, "core::Complex_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_real), "_real" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_imaginary), "_imaginary" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__HashTable_O, sizeof(core::HashTable_O), 0, "core::HashTable_O" },
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_InitialSize), "_InitialSize" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_RehashSize), "_RehashSize" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_RehashThreshold), "_RehashThreshold" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_HashTable), "_HashTable" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_HashTableCount), "_HashTableCount" }, // public: (NIL) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" }, // public: (NIL T) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" }, // public: (NIL T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyOuterArray[0]._inner.one), "_DummyOuterArray[0]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyOuterArray[0]._inner.two), "_DummyOuterArray[0]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyOuterArray[1]._inner.one), "_DummyOuterArray[1]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyOuterArray[1]._inner.two), "_DummyOuterArray[1]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyOuterArray[2]._inner.one), "_DummyOuterArray[2]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyOuterArray[2]._inner.two), "_DummyOuterArray[2]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyTArray[0]), "_DummyTArray[0]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTable_O),_DummyTArray[1]), "_DummyTArray[1]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__HashTableEql_O, sizeof(core::HashTableEql_O), 0, "core::HashTableEql_O" },
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_InitialSize), "_InitialSize" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_RehashSize), "_RehashSize" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_RehashThreshold), "_RehashThreshold" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_HashTable), "_HashTable" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_HashTableCount), "_HashTableCount" }, // public: (NIL) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" }, // public: (NIL T) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" }, // public: (NIL T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyOuterArray[0]._inner.one), "_DummyOuterArray[0]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyOuterArray[0]._inner.two), "_DummyOuterArray[0]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyOuterArray[1]._inner.one), "_DummyOuterArray[1]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyOuterArray[1]._inner.two), "_DummyOuterArray[1]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyOuterArray[2]._inner.one), "_DummyOuterArray[2]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyOuterArray[2]._inner.two), "_DummyOuterArray[2]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyTArray[0]), "_DummyTArray[0]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEql_O),_DummyTArray[1]), "_DummyTArray[1]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__HashTableEqual_O, sizeof(core::HashTableEqual_O), 0, "core::HashTableEqual_O" },
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_InitialSize), "_InitialSize" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_RehashSize), "_RehashSize" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_RehashThreshold), "_RehashThreshold" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_HashTable), "_HashTable" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_HashTableCount), "_HashTableCount" }, // public: (NIL) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" }, // public: (NIL T) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" }, // public: (NIL T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyOuterArray[0]._inner.one), "_DummyOuterArray[0]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyOuterArray[0]._inner.two), "_DummyOuterArray[0]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyOuterArray[1]._inner.one), "_DummyOuterArray[1]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyOuterArray[1]._inner.two), "_DummyOuterArray[1]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyOuterArray[2]._inner.one), "_DummyOuterArray[2]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyOuterArray[2]._inner.two), "_DummyOuterArray[2]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyTArray[0]), "_DummyTArray[0]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_DummyTArray[1]), "_DummyTArray[1]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__HashTableEqualp_O, sizeof(core::HashTableEqualp_O), 0, "core::HashTableEqualp_O" },
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_InitialSize), "_InitialSize" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_RehashSize), "_RehashSize" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_RehashThreshold), "_RehashThreshold" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_HashTable), "_HashTable" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_HashTableCount), "_HashTableCount" }, // public: (NIL) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" }, // public: (NIL T) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" }, // public: (NIL T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyOuterArray[0]._inner.one), "_DummyOuterArray[0]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyOuterArray[0]._inner.two), "_DummyOuterArray[0]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyOuterArray[1]._inner.one), "_DummyOuterArray[1]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyOuterArray[1]._inner.two), "_DummyOuterArray[1]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyOuterArray[2]._inner.one), "_DummyOuterArray[2]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyOuterArray[2]._inner.two), "_DummyOuterArray[2]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyTArray[0]), "_DummyTArray[0]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqualp_O),_DummyTArray[1]), "_DummyTArray[1]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__HashTableEq_O, sizeof(core::HashTableEq_O), 0, "core::HashTableEq_O" },
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_InitialSize), "_InitialSize" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_RehashSize), "_RehashSize" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_RehashThreshold), "_RehashThreshold" }, // public: (NIL) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_HashTable), "_HashTable" }, // public: (NIL) fixable: SMART-PTR-FIX good-name: T
// not-exposing {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_HashTableCount), "_HashTableCount" }, // public: (NIL) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" }, // public: (NIL T) fixable: NIL good-name: T
// not-exposing {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" }, // public: (NIL T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyOuterArray[0]._inner.one), "_DummyOuterArray[0]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyOuterArray[0]._inner.two), "_DummyOuterArray[0]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyOuterArray[1]._inner.one), "_DummyOuterArray[1]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyOuterArray[1]._inner.two), "_DummyOuterArray[1]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyOuterArray[2]._inner.one), "_DummyOuterArray[2]._inner.one" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyOuterArray[2]._inner.two), "_DummyOuterArray[2]._inner.two" }, // public: (NIL NIL T T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyTArray[0]), "_DummyTArray[0]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEq_O),_DummyTArray[1]), "_DummyTArray[1]" }, // public: (NIL NIL) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__DirectoryEntry_O, sizeof(core::DirectoryEntry_O), 0, "core::DirectoryEntry_O" },
{ class_kind, KIND_LISPALLOC_core__SourcePosInfo_O, sizeof(core::SourcePosInfo_O), 0, "core::SourcePosInfo_O" },
 {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_FileId), "_FileId" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Filepos), "_Filepos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Lineno), "_Lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Column), "_Column" }, // public: (T) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__CxxObject_O, sizeof(core::CxxObject_O), 0, "core::CxxObject_O" },
{ class_kind, KIND_LISPALLOC_core__Pathname_O, sizeof(core::Pathname_O), 0, "core::Pathname_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Host), "_Host" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Device), "_Device" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Directory), "_Directory" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Name), "_Name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Type), "_Type" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Version), "_Version" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__LogicalPathname_O, sizeof(core::LogicalPathname_O), 0, "core::LogicalPathname_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Host), "_Host" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Device), "_Device" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Directory), "_Directory" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Name), "_Name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Type), "_Type" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Version), "_Version" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Iterator_O, sizeof(core::Iterator_O), 0, "core::Iterator_O" },
{ class_kind, KIND_LISPALLOC_core__RecursiveDirectoryIterator_O, sizeof(core::RecursiveDirectoryIterator_O), 0, "core::RecursiveDirectoryIterator_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Path_O>), offsetof(SAFE_TYPE_MACRO(core::RecursiveDirectoryIterator_O),_Path), "_Path" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype__Bool, sizeof(_Bool), offsetof(SAFE_TYPE_MACRO(core::RecursiveDirectoryIterator_O),_EnterHidden), "_EnterHidden" }, // public: (T) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__DirectoryIterator_O, sizeof(core::DirectoryIterator_O), 0, "core::DirectoryIterator_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Path_O>), offsetof(SAFE_TYPE_MACRO(core::DirectoryIterator_O),_Path), "_Path" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Archive_O, sizeof(core::Archive_O), 0, "core::Archive_O" },
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Archive_O),_Version), "_Version" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, POINTER_OFFSET, sizeof(UnknownType), offsetof(SAFE_TYPE_MACRO(core::Archive_O),_TopNode.theObject), "_TopNode.theObject" }, // public: (T T) fixable: RAW-TAGGED-POINTER-FIX good-name: T
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Archive_O),_NextUniqueId), "_NextUniqueId" }, // public: (T) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__SaveArchive_O, sizeof(core::SaveArchive_O), 0, "core::SaveArchive_O" },
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_Version), "_Version" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, POINTER_OFFSET, sizeof(UnknownType), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_TopNode.theObject), "_TopNode.theObject" }, // public: (T T) fixable: RAW-TAGGED-POINTER-FIX good-name: T
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_NextUniqueId), "_NextUniqueId" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SaveArchive_O),_SNodeForObject), "_SNodeForObject" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__LoadArchive_O, sizeof(core::LoadArchive_O), 0, "core::LoadArchive_O" },
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_Version), "_Version" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, POINTER_OFFSET, sizeof(UnknownType), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_TopNode.theObject), "_TopNode.theObject" }, // public: (T T) fixable: RAW-TAGGED-POINTER-FIX good-name: T
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_NextUniqueId), "_NextUniqueId" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_ObjectForSNode), "_ObjectForSNode" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTable_O>), offsetof(SAFE_TYPE_MACRO(core::LoadArchive_O),_NodesToFinalize), "_NodesToFinalize" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Function_O, sizeof(core::Function_O), 0, "core::Function_O" },
{ class_kind, KIND_LISPALLOC_core__NamedFunction_O, sizeof(core::NamedFunction_O), 0, "core::NamedFunction_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::NamedFunction_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Closure_O, sizeof(core::Closure_O), 0, "core::Closure_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Closure_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__CompiledFunction_O, sizeof(core::CompiledFunction_O), 0, "core::CompiledFunction_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledFunction_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__FunctionClosure_O, sizeof(core::FunctionClosure_O), 0, "core::FunctionClosure_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),kind), "kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_cleavir_ast), "_cleavir_ast" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_filePos), "_filePos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_lineno), "_lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::FunctionClosure_O),_column), "_column" }, // public: (T) fixable: NIL good-name: T
{ class_kind, KIND_LISPALLOC_core__BuiltinClosure_O, sizeof(core::BuiltinClosure_O), 0, "core::BuiltinClosure_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),kind), "kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_cleavir_ast), "_cleavir_ast" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_filePos), "_filePos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_lineno), "_lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_column), "_column" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_lambdaListHandler), "_lambdaListHandler" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_declares), "_declares" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure_O),_docstring), "_docstring" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__MacroClosure_O, sizeof(core::MacroClosure_O), 0, "core::MacroClosure_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),kind), "kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_cleavir_ast), "_cleavir_ast" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_filePos), "_filePos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_lineno), "_lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_column), "_column" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_lambdaListHandler), "_lambdaListHandler" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_declares), "_declares" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure_O),_docstring), "_docstring" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__ClosureWithFrame_O, sizeof(core::ClosureWithFrame_O), 0, "core::ClosureWithFrame_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),kind), "kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_cleavir_ast), "_cleavir_ast" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_filePos), "_filePos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_lineno), "_lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_column), "_column" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ClosureWithFrame_O),_closedEnvironment), "_closedEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__CompiledClosure_O, sizeof(core::CompiledClosure_O), 0, "core::CompiledClosure_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),kind), "kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_cleavir_ast), "_cleavir_ast" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_filePos), "_filePos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_lineno), "_lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_column), "_column" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_closedEnvironment), "_closedEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),llvmFunction), "llvmFunction" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),associatedFunctions), "associatedFunctions" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_lambdaList), "_lambdaList" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_Slots._Data), "_Slots._Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_Slots._Capacity), offsetof(SAFE_TYPE_MACRO(core::CompiledClosure_O),_Slots._Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__InterpretedClosure_O, sizeof(core::InterpretedClosure_O), 0, "core::InterpretedClosure_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_name), "_name" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),kind), "kind" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_cleavir_ast), "_cleavir_ast" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_sourceFileInfoHandle), "_sourceFileInfoHandle" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_filePos), "_filePos" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_lineno), "_lineno" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_column), "_column" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_closedEnvironment), "_closedEnvironment" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_lambdaListHandler), "_lambdaListHandler" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_declares), "_declares" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_docstring), "_docstring" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::InterpretedClosure_O),_code), "_code" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Instance_O, sizeof(core::Instance_O), 0, "core::Instance_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Class), "_Class" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_isgf), "_isgf" }, // public: (T) fixable: NIL good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_lambda_list), "_lambda_list" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Slots._Vector._Contents), "_Slots._Vector._Contents" }, // public: (T T T) fixable: TAGGED-POINTER-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Sig), "_Sig" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ class_kind, KIND_LISPALLOC_core__Cons_O, sizeof(core::Cons_O), 0, "core::Cons_O" },
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Cons_O),_Car), "_Car" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Cons_O),_Cdr), "_Cdr" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_, sizeof(gctools::GCVector_moveable<core::ExceptionEntry>), 0, "gctools::GCVector_moveable<core::ExceptionEntry>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Data), "_Data" },
{  variable_capacity, sizeof(core::ExceptionEntry), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Capacity), NULL },
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_Key), "_Key" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), 0, "only" },
{ container_kind, KIND_GCSTRING_gctools__GCString_moveable_char_, sizeof(gctools::GCString_moveable<char>), 0, "gctools::GCString_moveable<char>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Data), "_Data" },
{  variable_capacity, sizeof(char), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Capacity), NULL },
{    variable_field, ctype_char, sizeof(char), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_, sizeof(gctools::GCVector_moveable<core::DynamicBinding>), 0, "gctools::GCVector_moveable<core::DynamicBinding>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Data), "_Data" },
{  variable_capacity, sizeof(core::DynamicBinding), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Capacity), NULL },
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Var), "_Var" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Val), "_Val" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__, sizeof(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>), 0, "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_, sizeof(gctools::GCVector_moveable<core::CacheRecord>), 0, "gctools::GCVector_moveable<core::CacheRecord>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Data), "_Data" },
{  variable_capacity, sizeof(core::CacheRecord), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Capacity), NULL },
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_key), "_key" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_value), "_value" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_generation), "_generation" }, // public: (T) fixable: NIL good-name: T
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_, sizeof(gctools::GCVector_moveable<core::SymbolClassPair>), 0, "gctools::GCVector_moveable<core::SymbolClassPair>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Data), "_Data" },
{  variable_capacity, sizeof(core::SymbolClassPair), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Capacity), NULL },
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),symbol), "symbol" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
 {    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),theClass), "theClass" }, // public: (T) fixable: SMART-PTR-FIX good-name: T
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), 0, "only" },

#endif // defined(GC_OBJ_SCAN_HELPERS)
#if defined(GC_OBJ_SCAN_TABLE)
static void* OBJ_SCAN_table[] = { 
   NULL
};
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
obj_finalize_KIND_BOOTSTRAP_core__T_O:
{
    core::T_O* obj_gc_safe = reinterpret_cast<core::T_O*>(client);
    obj_gc_safe->~T_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    obj_gc_safe->~General_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Exposer_O:
{
    core::Exposer_O* obj_gc_safe = reinterpret_cast<core::Exposer_O*>(client);
    obj_gc_safe->~Exposer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CoreExposer_O:
{
    core::CoreExposer_O* obj_gc_safe = reinterpret_cast<core::CoreExposer_O*>(client);
    obj_gc_safe->~CoreExposer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    obj_gc_safe->~Record_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    obj_gc_safe->~Symbol_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    obj_gc_safe->~Environment_O();
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
obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    obj_gc_safe->~TagbodyEnvironment_O();
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
obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    obj_gc_safe->~StackValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    obj_gc_safe->~SymbolMacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    obj_gc_safe->~MacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    obj_gc_safe->~FunctionContainerEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    obj_gc_safe->~CatchEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    obj_gc_safe->~BlockEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ActivationFrame_O:
{
    core::ActivationFrame_O* obj_gc_safe = reinterpret_cast<core::ActivationFrame_O*>(client);
    obj_gc_safe->~ActivationFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    obj_gc_safe->~FunctionFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    obj_gc_safe->~ValueFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    obj_gc_safe->~TagbodyFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    obj_gc_safe->~GlueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    obj_gc_safe->~Path_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    obj_gc_safe->~CandoException_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    obj_gc_safe->~Array_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    obj_gc_safe->~Vector_O();
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
obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    obj_gc_safe->~SourceFileInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Creator_O:
{
    core::Creator_O* obj_gc_safe = reinterpret_cast<core::Creator_O*>(client);
    obj_gc_safe->~Creator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__InstanceCreator_O:
{
    core::InstanceCreator_O* obj_gc_safe = reinterpret_cast<core::InstanceCreator_O*>(client);
    obj_gc_safe->~InstanceCreator_O();
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
obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    obj_gc_safe->~SymbolToEnumConverter_O();
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
obj_finalize_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    obj_gc_safe->~FileStatus_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    obj_gc_safe->~SourceManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    obj_gc_safe->~Number_O();
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
obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    obj_gc_safe->~Fixnum_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    obj_gc_safe->~Bignum_O();
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
obj_finalize_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    obj_gc_safe->~ShortFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    obj_gc_safe->~SingleFloat_dummy_O();
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
obj_finalize_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    obj_gc_safe->~Complex_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTable_O:
{
    core::HashTable_O* obj_gc_safe = reinterpret_cast<core::HashTable_O*>(client);
    obj_gc_safe->~HashTable_O();
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
obj_finalize_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    core::HashTableEqualp_O* obj_gc_safe = reinterpret_cast<core::HashTableEqualp_O*>(client);
    obj_gc_safe->~HashTableEqualp_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTableEq_O:
{
    core::HashTableEq_O* obj_gc_safe = reinterpret_cast<core::HashTableEq_O*>(client);
    obj_gc_safe->~HashTableEq_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    obj_gc_safe->~DirectoryEntry_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    obj_gc_safe->~SourcePosInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    obj_gc_safe->~CxxObject_O();
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
obj_finalize_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    obj_gc_safe->~Iterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    obj_gc_safe->~RecursiveDirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    obj_gc_safe->~DirectoryIterator_O();
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
obj_finalize_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    obj_gc_safe->~LoadArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__NamedFunction_O:
{
    core::NamedFunction_O* obj_gc_safe = reinterpret_cast<core::NamedFunction_O*>(client);
    obj_gc_safe->~NamedFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Closure_O:
{
    core::Closure_O* obj_gc_safe = reinterpret_cast<core::Closure_O*>(client);
    obj_gc_safe->~Closure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    obj_gc_safe->~CompiledFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionClosure_O:
{
    core::FunctionClosure_O* obj_gc_safe = reinterpret_cast<core::FunctionClosure_O*>(client);
    obj_gc_safe->~FunctionClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BuiltinClosure_O:
{
    core::BuiltinClosure_O* obj_gc_safe = reinterpret_cast<core::BuiltinClosure_O*>(client);
    obj_gc_safe->~BuiltinClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__MacroClosure_O:
{
    core::MacroClosure_O* obj_gc_safe = reinterpret_cast<core::MacroClosure_O*>(client);
    obj_gc_safe->~MacroClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ClosureWithFrame_O:
{
    core::ClosureWithFrame_O* obj_gc_safe = reinterpret_cast<core::ClosureWithFrame_O*>(client);
    obj_gc_safe->~ClosureWithFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledClosure_O:
{
    core::CompiledClosure_O* obj_gc_safe = reinterpret_cast<core::CompiledClosure_O*>(client);
    obj_gc_safe->~CompiledClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__InterpretedClosure_O:
{
    core::InterpretedClosure_O* obj_gc_safe = reinterpret_cast<core::InterpretedClosure_O*>(client);
    obj_gc_safe->~InterpretedClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    obj_gc_safe->~Instance_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    obj_gc_safe->~Cons_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_HELPERS)

#endif // defined(GC_OBJ_FINALIZE_HELPERS)
#if defined(GC_OBJ_FINALIZE_TABLE)
static void* OBJ_FINALIZE_table[] = { 
  /* 5 */ &&obj_finalize_KIND_BOOTSTRAP_core__T_O,
  /* 6 */ &&obj_finalize_KIND_LISPALLOC_core__General_O,
  /* 7 */ &&obj_finalize_KIND_LISPALLOC_core__Exposer_O,
  /* 8 */ &&obj_finalize_KIND_LISPALLOC_core__CoreExposer_O,
  /* 9 */ &&obj_finalize_KIND_LISPALLOC_core__Record_O,
  /* 10 */ &&obj_finalize_KIND_BOOTSTRAP_core__Symbol_O,
  /* 11 */ &&obj_finalize_KIND_LISPALLOC_core__Environment_O,
  /* 12 */ &&obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 13 */ &&obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 14 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 15 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 16 */ &&obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 17 */ &&obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 18 */ &&obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 19 */ &&obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 20 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 21 */ &&obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 22 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 23 */ &&obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 24 */ &&obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 25 */ &&obj_finalize_KIND_LISPALLOC_core__ActivationFrame_O,
  /* 26 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 27 */ &&obj_finalize_KIND_LISPALLOC_core__ValueFrame_O,
  /* 28 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 29 */ &&obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O,
  /* 30 */ &&obj_finalize_KIND_LISPALLOC_core__Path_O,
  /* 31 */ &&obj_finalize_KIND_LISPALLOC_core__Pointer_O,
  /* 32 */ &&obj_finalize_KIND_LISPALLOC_core__CandoException_O,
  /* 33 */ &&obj_finalize_KIND_LISPALLOC_core__Array_O,
  /* 34 */ &&obj_finalize_KIND_LISPALLOC_core__Vector_O,
  /* 35 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjects_O,
  /* 36 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O,
  /* 37 */ &&obj_finalize_KIND_LISPALLOC_core__String_O,
  /* 38 */ &&obj_finalize_KIND_BOOTSTRAP_core__Str_O,
  /* 39 */ &&obj_finalize_KIND_LISPALLOC_core__BitVector_O,
  /* 40 */ &&obj_finalize_KIND_LISPALLOC_core__SimpleBitVector_O,
  /* 41 */ &&obj_finalize_KIND_LISPALLOC_core__BitVectorWithFillPtr_O,
  /* 42 */ &&obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 43 */ &&obj_finalize_KIND_LISPALLOC_core__Creator_O,
  /* 44 */ &&obj_finalize_KIND_LISPALLOC_core__InstanceCreator_O,
  /* 45 */ &&obj_finalize_KIND_LISPALLOC_core__SNode_O,
  /* 46 */ &&obj_finalize_KIND_LISPALLOC_core__LeafSNode_O,
  /* 47 */ &&obj_finalize_KIND_LISPALLOC_core__BranchSNode_O,
  /* 48 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 49 */ &&obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 50 */ &&obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 51 */ &&obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 52 */ &&obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 53 */ &&obj_finalize_KIND_BOOTSTRAP_core__Specializer_O,
  /* 54 */ &&obj_finalize_KIND_BOOTSTRAP_core__Class_O,
  /* 55 */ &&obj_finalize_KIND_LISPALLOC_core__FileStatus_O,
  /* 56 */ &&obj_finalize_KIND_LISPALLOC_core__SourceManager_O,
  /* 57 */ &&obj_finalize_KIND_LISPALLOC_core__Number_O,
  /* 58 */ &&obj_finalize_KIND_LISPALLOC_core__Real_O,
  /* 59 */ &&obj_finalize_KIND_LISPALLOC_core__Rational_O,
  /* 60 */ &&obj_finalize_KIND_LISPALLOC_core__Integer_O,
  /* 61 */ &&obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 62 */ &&obj_finalize_KIND_LISPALLOC_core__Bignum_O,
  /* 63 */ &&obj_finalize_KIND_LISPALLOC_core__Ratio_O,
  /* 64 */ &&obj_finalize_KIND_LISPALLOC_core__Float_O,
  /* 65 */ &&obj_finalize_KIND_LISPALLOC_core__ShortFloat_O,
  /* 66 */ &&obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 67 */ &&obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 68 */ &&obj_finalize_KIND_LISPALLOC_core__LongFloat_O,
  /* 69 */ &&obj_finalize_KIND_LISPALLOC_core__Complex_O,
  /* 70 */ &&obj_finalize_KIND_LISPALLOC_core__HashTable_O,
  /* 71 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEql_O,
  /* 72 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEqual_O,
  /* 73 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEqualp_O,
  /* 74 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEq_O,
  /* 75 */ &&obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O,
  /* 76 */ &&obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 77 */ &&obj_finalize_KIND_LISPALLOC_core__CxxObject_O,
  /* 78 */ &&obj_finalize_KIND_LISPALLOC_core__Pathname_O,
  /* 79 */ &&obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O,
  /* 80 */ &&obj_finalize_KIND_LISPALLOC_core__Iterator_O,
  /* 81 */ &&obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O,
  /* 82 */ &&obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O,
  /* 83 */ &&obj_finalize_KIND_LISPALLOC_core__Archive_O,
  /* 84 */ &&obj_finalize_KIND_LISPALLOC_core__SaveArchive_O,
  /* 85 */ &&obj_finalize_KIND_LISPALLOC_core__LoadArchive_O,
  /* 86 */ &&obj_finalize_KIND_LISPALLOC_core__Function_O,
  /* 87 */ &&obj_finalize_KIND_LISPALLOC_core__NamedFunction_O,
  /* 88 */ &&obj_finalize_KIND_LISPALLOC_core__Closure_O,
  /* 89 */ &&obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 90 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionClosure_O,
  /* 91 */ &&obj_finalize_KIND_LISPALLOC_core__BuiltinClosure_O,
  /* 92 */ &&obj_finalize_KIND_LISPALLOC_core__MacroClosure_O,
  /* 93 */ &&obj_finalize_KIND_LISPALLOC_core__ClosureWithFrame_O,
  /* 94 */ &&obj_finalize_KIND_LISPALLOC_core__CompiledClosure_O,
  /* 95 */ &&obj_finalize_KIND_LISPALLOC_core__InterpretedClosure_O,
  /* 96 */ &&obj_finalize_KIND_LISPALLOC_core__Instance_O,
  /* 97 */ &&obj_finalize_KIND_LISPALLOC_core__Cons_O,
  /* 98 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 99 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 100 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 101 */ &&obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 102 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 103 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 104 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__,
  /* 105 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 106 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 107 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 108 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
   NULL
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_OBJ_DEALLOCATOR)
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__T_O:
{
    core::T_O* obj_gc_safe = reinterpret_cast<core::T_O*>(client);
    GC<core::T_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    GC<core::General_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Exposer_O:
{
    core::Exposer_O* obj_gc_safe = reinterpret_cast<core::Exposer_O*>(client);
    GC<core::Exposer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CoreExposer_O:
{
    core::CoreExposer_O* obj_gc_safe = reinterpret_cast<core::CoreExposer_O*>(client);
    GC<core::CoreExposer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    GC<core::Record_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    GC<core::Symbol_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    GC<core::Environment_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    GC<core::TagbodyEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    GC<core::StackValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    GC<core::SymbolMacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    GC<core::MacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    GC<core::FunctionContainerEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    GC<core::CatchEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    GC<core::BlockEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ActivationFrame_O:
{
    core::ActivationFrame_O* obj_gc_safe = reinterpret_cast<core::ActivationFrame_O*>(client);
    GC<core::ActivationFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    GC<core::FunctionFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    GC<core::ValueFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    GC<core::TagbodyFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    GC<core::GlueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    GC<core::Path_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    GC<core::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    GC<core::CandoException_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    GC<core::Array_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    GC<core::Vector_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    GC<core::SourceFileInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Creator_O:
{
    core::Creator_O* obj_gc_safe = reinterpret_cast<core::Creator_O*>(client);
    GC<core::Creator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InstanceCreator_O:
{
    core::InstanceCreator_O* obj_gc_safe = reinterpret_cast<core::InstanceCreator_O*>(client);
    GC<core::InstanceCreator_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    GC<core::SymbolToEnumConverter_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    GC<core::FileStatus_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    GC<core::SourceManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    GC<core::Number_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    GC<core::Fixnum_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    GC<core::Bignum_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    GC<core::ShortFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    GC<core::SingleFloat_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    GC<core::Complex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTable_O:
{
    core::HashTable_O* obj_gc_safe = reinterpret_cast<core::HashTable_O*>(client);
    GC<core::HashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqualp_O:
{
    core::HashTableEqualp_O* obj_gc_safe = reinterpret_cast<core::HashTableEqualp_O*>(client);
    GC<core::HashTableEqualp_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEq_O:
{
    core::HashTableEq_O* obj_gc_safe = reinterpret_cast<core::HashTableEq_O*>(client);
    GC<core::HashTableEq_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    GC<core::DirectoryEntry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    GC<core::SourcePosInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    GC<core::CxxObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    GC<core::Iterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    GC<core::RecursiveDirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    GC<core::DirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    GC<core::LoadArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    GC<core::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__NamedFunction_O:
{
    core::NamedFunction_O* obj_gc_safe = reinterpret_cast<core::NamedFunction_O*>(client);
    GC<core::NamedFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Closure_O:
{
    core::Closure_O* obj_gc_safe = reinterpret_cast<core::Closure_O*>(client);
    GC<core::Closure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    GC<core::CompiledFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionClosure_O:
{
    core::FunctionClosure_O* obj_gc_safe = reinterpret_cast<core::FunctionClosure_O*>(client);
    GC<core::FunctionClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BuiltinClosure_O:
{
    core::BuiltinClosure_O* obj_gc_safe = reinterpret_cast<core::BuiltinClosure_O*>(client);
    GC<core::BuiltinClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroClosure_O:
{
    core::MacroClosure_O* obj_gc_safe = reinterpret_cast<core::MacroClosure_O*>(client);
    GC<core::MacroClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ClosureWithFrame_O:
{
    core::ClosureWithFrame_O* obj_gc_safe = reinterpret_cast<core::ClosureWithFrame_O*>(client);
    GC<core::ClosureWithFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledClosure_O:
{
    core::CompiledClosure_O* obj_gc_safe = reinterpret_cast<core::CompiledClosure_O*>(client);
    GC<core::CompiledClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InterpretedClosure_O:
{
    core::InterpretedClosure_O* obj_gc_safe = reinterpret_cast<core::InterpretedClosure_O*>(client);
    GC<core::InterpretedClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    GC<core::Instance_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    GC<core::Cons_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never deallocate gcstrings gctools::GCString_moveable<char>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
#endif // defined(GC_OBJ_DEALLOCATOR)
#if defined(GC_OBJ_DEALLOCATOR_HELPERS)

#endif // defined(GC_OBJ_DEALLOCATOR_HELPERS)
#if defined(GC_OBJ_DEALLOCATOR_TABLE)
static void* OBJ_DEALLOCATOR_table[] = { 
  /* 5 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__T_O,
  /* 6 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O,
  /* 7 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Exposer_O,
  /* 8 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CoreExposer_O,
  /* 9 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O,
  /* 10 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O,
  /* 11 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O,
  /* 12 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 13 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 14 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 15 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 16 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 17 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 18 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 19 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 20 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 21 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 22 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 23 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 24 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 25 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ActivationFrame_O,
  /* 26 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 27 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O,
  /* 28 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 29 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O,
  /* 30 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O,
  /* 31 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O,
  /* 32 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O,
  /* 33 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O,
  /* 34 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O,
  /* 35 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O,
  /* 36 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O,
  /* 37 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O,
  /* 38 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O,
  /* 39 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVector_O,
  /* 40 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SimpleBitVector_O,
  /* 41 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BitVectorWithFillPtr_O,
  /* 42 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 43 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Creator_O,
  /* 44 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InstanceCreator_O,
  /* 45 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SNode_O,
  /* 46 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LeafSNode_O,
  /* 47 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BranchSNode_O,
  /* 48 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 49 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 50 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 51 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 52 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 53 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O,
  /* 54 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O,
  /* 55 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O,
  /* 56 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O,
  /* 57 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O,
  /* 58 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O,
  /* 59 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O,
  /* 60 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O,
  /* 61 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 62 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O,
  /* 63 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O,
  /* 64 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O,
  /* 65 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O,
  /* 66 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 67 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 68 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O,
  /* 69 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O,
  /* 70 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTable_O,
  /* 71 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEql_O,
  /* 72 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O,
  /* 73 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqualp_O,
  /* 74 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEq_O,
  /* 75 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O,
  /* 76 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 77 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O,
  /* 78 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O,
  /* 79 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O,
  /* 80 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O,
  /* 81 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O,
  /* 82 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O,
  /* 83 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O,
  /* 84 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O,
  /* 85 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O,
  /* 86 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O,
  /* 87 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__NamedFunction_O,
  /* 88 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Closure_O,
  /* 89 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 90 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionClosure_O,
  /* 91 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BuiltinClosure_O,
  /* 92 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroClosure_O,
  /* 93 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ClosureWithFrame_O,
  /* 94 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledClosure_O,
  /* 95 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InterpretedClosure_O,
  /* 96 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O,
  /* 97 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O,
  /* 98 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 99 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 100 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 101 */ &&obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 102 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 103 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 104 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__,
  /* 105 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 106 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 107 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 108 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
   NULL
};
#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)
#if defined(GC_GLOBALS)
#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
