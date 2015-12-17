#ifdef DECLARE_FORWARDS
 namespace core {
    class TagbodyFrame_O;
    class Real_O;
    class SymbolMacroletEnvironment_O;
    class Path_O;
    class MacroClosure;
    class String_O;
    class SourceManager_O;
    class CatchEnvironment_O;
    class FileStatus_O;
    class SourcePosInfo_O;
    class BlockEnvironment_O;
    class StackValueEnvironment_O;
    class Environment_O;
    class LexicalEnvironment_O;
    class LogicalPathname_O;
    class Function_O;
    class ExceptionEntry;
    class Str_O;
    class Symbol_O;
    class DirectoryEntry_O;
    class SaveArchive_O;
    class FunctionFrame_O;
    class Rational_O;
    class DirectoryIterator_O;
    class DynamicBinding;
    class FunctionValueEnvironment_O;
    class Vector_O;
    class CompiledFunction_O;
    class Array_O;
    class Specializer_O;
    class Complex_O;
    class GlueEnvironment_O;
    class FunctionContainerEnvironment_O;
    class Iterator_O;
    class DoubleFloat_O;
    class TagbodyEnvironment_O;
    class VectorObjects_O;
    class Number_O;
    class Integer_O;
    class MacroletEnvironment_O;
    class WeakKeyHashTable_O;
    class SingleFloat_dummy_O;
    class WeakHashTable_O;
    class Bignum_O;
    class Metaobject_O;
    class ValueFrame_O;
    class LongFloat_O;
    class VectorObjectsWithFillPtr_O;
    class Class_O;
    class Ratio_O;
    class CacheRecord;
    class ValueEnvironment_O;
    class RecursiveDirectoryIterator_O;
    class CxxObject_O;
    class Fixnum_dummy_O;
    class Pointer_O;
    class RuntimeVisibleEnvironment_O;
    class SymbolToEnumConverter_O;
    class Cons_O;
    class CandoException_O;
    class StandardObject_O;
    class SymbolClassPair;
    class Float_O;
    class ShortFloat_O;
    class Pathname_O;
    class UnwindProtectEnvironment_O;
    class LoadArchive_O;
    class CompileTimeEnvironment_O;
    class SourceFileInfo_O;
    class Archive_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_FIXNUM = 1, 
KIND_SINGLE_FLOAT = 2, 
KIND_CHARACTER = 3, 
KIND_TEMPLATED_CLASSALLOC_core__Creator = 4,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 5,
KIND_LISPALLOC_core__Archive_O = 6,
KIND_LISPALLOC_core__SourceFileInfo_O = 7,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 8,
KIND_LISPALLOC_core__LoadArchive_O = 9,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 10,
KIND_LISPALLOC_core__Pathname_O = 11,
KIND_LISPALLOC_core__ShortFloat_O = 12,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 13,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 14,
KIND_LISPALLOC_core__Float_O = 15,
KIND_BOOTSTRAP_core__StandardObject_O = 16,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 17,
KIND_LISPALLOC_core__CandoException_O = 18,
KIND_LISPALLOC_core__Cons_O = 19,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 20,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 21,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 22,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 23,
KIND_LISPALLOC_core__Pointer_O = 24,
KIND_LISPALLOC_core__Fixnum_dummy_O = 25,
KIND_LISPALLOC_core__CxxObject_O = 26,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 27,
KIND_LISPALLOC_core__ValueEnvironment_O = 28,
KIND_LISPALLOC_core__Ratio_O = 29,
KIND_BOOTSTRAP_core__Class_O = 30,
KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O = 31,
KIND_LISPALLOC_core__LongFloat_O = 32,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 33,
KIND_LISPALLOC_core__ValueFrame_O = 34,
KIND_BOOTSTRAP_core__Metaobject_O = 35,
KIND_LISPALLOC_core__Bignum_O = 36,
KIND_LISPALLOC_core__WeakHashTable_O = 37,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 38,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 39,
KIND_LISPALLOC_core__MacroletEnvironment_O = 40,
KIND_LISPALLOC_core__Integer_O = 41,
KIND_LISPALLOC_core__Number_O = 42,
KIND_LISPALLOC_core__VectorObjects_O = 43,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 44,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 45,
KIND_LISPALLOC_core__DoubleFloat_O = 46,
KIND_LISPALLOC_core__Iterator_O = 47,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 48,
KIND_LISPALLOC_core__GlueEnvironment_O = 49,
KIND_LISPALLOC_core__Complex_O = 50,
KIND_BOOTSTRAP_core__Specializer_O = 51,
KIND_LISPALLOC_core__Array_O = 52,
KIND_LISPALLOC_core__CompiledFunction_O = 53,
KIND_LISPALLOC_core__Vector_O = 54,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 55,
KIND_LISPALLOC_core__DirectoryIterator_O = 56,
KIND_LISPALLOC_core__Rational_O = 57,
KIND_LISPALLOC_core__FunctionFrame_O = 58,
KIND_LISPALLOC_core__SaveArchive_O = 59,
KIND_LISPALLOC_core__DirectoryEntry_O = 60,
KIND_BOOTSTRAP_core__Symbol_O = 61,
KIND_BOOTSTRAP_core__Str_O = 62,
KIND_LISPALLOC_core__Function_O = 63,
KIND_LISPALLOC_core__LogicalPathname_O = 64,
KIND_LISPALLOC_core__LexicalEnvironment_O = 65,
KIND_LISPALLOC_core__Environment_O = 66,
KIND_LISPALLOC_core__StackValueEnvironment_O = 67,
KIND_LISPALLOC_core__BlockEnvironment_O = 68,
KIND_LISPALLOC_core__SourcePosInfo_O = 69,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 70,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 71,
KIND_LISPALLOC_core__SourceManager_O = 72,
KIND_LISPALLOC_core__CatchEnvironment_O = 73,
KIND_LISPALLOC_core__FileStatus_O = 74,
KIND_CLASSALLOC_core__MacroClosure = 75,
KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure = 76,
KIND_LISPALLOC_core__String_O = 77,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 78,
KIND_LISPALLOC_core__Path_O = 79,
KIND_LISPALLOC_core__Real_O = 80,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 81,
KIND_LISPALLOC_core__TagbodyFrame_O = 82,
  KIND_max = 82
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct Cast<core::Creator*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 4 4 
      return (kindVal == 4);
  };
};
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<core::Archive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Archive_O ;
};
template <> class gctools::GCKind<core::SourceFileInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceFileInfo_O ;
};
template <> class gctools::GCKind<core::CompileTimeEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompileTimeEnvironment_O ;
};
template <> class gctools::GCKind<core::LoadArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LoadArchive_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::Pathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pathname_O ;
};
template <> class gctools::GCKind<core::ShortFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ShortFloat_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::ExceptionEntry>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<core::Float_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Float_O ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<core::CandoException_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CandoException_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ ;
};
template <> class gctools::GCKind<core::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pointer_O ;
};
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<core::CxxObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CxxObject_O ;
};
template <> class gctools::GCKind<core::RecursiveDirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RecursiveDirectoryIterator_O ;
};
template <> class gctools::GCKind<core::ValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueEnvironment_O ;
};
template <> class gctools::GCKind<core::Ratio_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Ratio_O ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<core::VectorObjectsWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O ;
};
template <> class gctools::GCKind<core::LongFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LongFloat_O ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::Creator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__Creator ;
};
template <> class gctools::GCKind<core::Metaobject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Metaobject_O ;
};
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
};
template <> class gctools::GCKind<core::WeakHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakHashTable_O ;
};
template <> class gctools::GCKind<core::SingleFloat_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_dummy_O ;
};
template <> class gctools::GCKind<core::WeakKeyHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyHashTable_O ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<core::Number_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Number_O ;
};
template <> class gctools::GCKind<core::VectorObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjects_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolClassPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ ;
};
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<core::DoubleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DoubleFloat_O ;
};
template <> class gctools::GCKind<core::Iterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Iterator_O ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
};
template <> class gctools::GCKind<core::Specializer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Specializer_O ;
};
template <> class gctools::GCKind<core::Array_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Array_O ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<core::Vector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Vector_O ;
};
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<core::DirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DirectoryIterator_O ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<core::SaveArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SaveArchive_O ;
};
template <> class gctools::GCKind<core::DirectoryEntry_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DirectoryEntry_O ;
};
template <> class gctools::GCKind<core::Symbol_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Symbol_O ;
};
template <> class gctools::GCKind<core::Str_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Str_O ;
};
template <> class gctools::GCKind<core::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Function_O ;
};
template <> class gctools::GCKind<core::LogicalPathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LogicalPathname_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
};
template <> class gctools::GCKind<core::StackValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StackValueEnvironment_O ;
};
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<core::SourcePosInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourcePosInfo_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
};
template <> class gctools::GCKind<core::SourceManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceManager_O ;
};
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
};
template <> class gctools::GCKind<core::FileStatus_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FileStatus_O ;
};
template <> class gctools::GCKind<core::MacroClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__MacroClosure ;
};
template <> class gctools::GCKind<core::BuiltinClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure ;
};
template <> class gctools::GCKind<core::String_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__String_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::DynamicBinding>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ ;
};
template <> class gctools::GCKind<core::Path_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Path_O ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<core::SymbolMacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolMacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_KIND_NAME_MAP)
kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
return "KIND_TEMPLATED_CLASSALLOC_core__Creator";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__";
}
kind_name_KIND_LISPALLOC_core__Archive_O:
{
return "KIND_LISPALLOC_core__Archive_O";
}
kind_name_KIND_LISPALLOC_core__SourceFileInfo_O:
{
return "KIND_LISPALLOC_core__SourceFileInfo_O";
}
kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
return "KIND_LISPALLOC_core__CompileTimeEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__LoadArchive_O:
{
return "KIND_LISPALLOC_core__LoadArchive_O";
}
kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
return "KIND_LISPALLOC_core__UnwindProtectEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Pathname_O:
{
return "KIND_LISPALLOC_core__Pathname_O";
}
kind_name_KIND_LISPALLOC_core__ShortFloat_O:
{
return "KIND_LISPALLOC_core__ShortFloat_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__";
}
kind_name_KIND_LISPALLOC_core__Float_O:
{
return "KIND_LISPALLOC_core__Float_O";
}
kind_name_KIND_BOOTSTRAP_core__StandardObject_O:
{
return "KIND_BOOTSTRAP_core__StandardObject_O";
}
kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
return "KIND_GCSTRING_gctools__GCString_moveable_char_";
}
kind_name_KIND_LISPALLOC_core__CandoException_O:
{
return "KIND_LISPALLOC_core__CandoException_O";
}
kind_name_KIND_LISPALLOC_core__Cons_O:
{
return "KIND_LISPALLOC_core__Cons_O";
}
kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
return "KIND_LISPALLOC_core__SymbolToEnumConverter_O";
}
kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
return "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__";
}
kind_name_KIND_LISPALLOC_core__Pointer_O:
{
return "KIND_LISPALLOC_core__Pointer_O";
}
kind_name_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
return "KIND_LISPALLOC_core__Fixnum_dummy_O";
}
kind_name_KIND_LISPALLOC_core__CxxObject_O:
{
return "KIND_LISPALLOC_core__CxxObject_O";
}
kind_name_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
return "KIND_LISPALLOC_core__RecursiveDirectoryIterator_O";
}
kind_name_KIND_LISPALLOC_core__ValueEnvironment_O:
{
return "KIND_LISPALLOC_core__ValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Ratio_O:
{
return "KIND_LISPALLOC_core__Ratio_O";
}
kind_name_KIND_BOOTSTRAP_core__Class_O:
{
return "KIND_BOOTSTRAP_core__Class_O";
}
kind_name_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
return "KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O";
}
kind_name_KIND_LISPALLOC_core__LongFloat_O:
{
return "KIND_LISPALLOC_core__LongFloat_O";
}
kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
return "KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_";
}
kind_name_KIND_LISPALLOC_core__ValueFrame_O:
{
return "KIND_LISPALLOC_core__ValueFrame_O";
}
kind_name_KIND_BOOTSTRAP_core__Metaobject_O:
{
return "KIND_BOOTSTRAP_core__Metaobject_O";
}
kind_name_KIND_LISPALLOC_core__Bignum_O:
{
return "KIND_LISPALLOC_core__Bignum_O";
}
kind_name_KIND_LISPALLOC_core__WeakHashTable_O:
{
return "KIND_LISPALLOC_core__WeakHashTable_O";
}
kind_name_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
return "KIND_LISPALLOC_core__SingleFloat_dummy_O";
}
kind_name_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
return "KIND_LISPALLOC_core__WeakKeyHashTable_O";
}
kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
return "KIND_LISPALLOC_core__MacroletEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Integer_O:
{
return "KIND_LISPALLOC_core__Integer_O";
}
kind_name_KIND_LISPALLOC_core__Number_O:
{
return "KIND_LISPALLOC_core__Number_O";
}
kind_name_KIND_LISPALLOC_core__VectorObjects_O:
{
return "KIND_LISPALLOC_core__VectorObjects_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_";
}
kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
return "KIND_LISPALLOC_core__TagbodyEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__DoubleFloat_O:
{
return "KIND_LISPALLOC_core__DoubleFloat_O";
}
kind_name_KIND_LISPALLOC_core__Iterator_O:
{
return "KIND_LISPALLOC_core__Iterator_O";
}
kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
return "KIND_LISPALLOC_core__FunctionContainerEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__GlueEnvironment_O:
{
return "KIND_LISPALLOC_core__GlueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Complex_O:
{
return "KIND_LISPALLOC_core__Complex_O";
}
kind_name_KIND_BOOTSTRAP_core__Specializer_O:
{
return "KIND_BOOTSTRAP_core__Specializer_O";
}
kind_name_KIND_LISPALLOC_core__Array_O:
{
return "KIND_LISPALLOC_core__Array_O";
}
kind_name_KIND_LISPALLOC_core__CompiledFunction_O:
{
return "KIND_LISPALLOC_core__CompiledFunction_O";
}
kind_name_KIND_LISPALLOC_core__Vector_O:
{
return "KIND_LISPALLOC_core__Vector_O";
}
kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
return "KIND_LISPALLOC_core__FunctionValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__DirectoryIterator_O:
{
return "KIND_LISPALLOC_core__DirectoryIterator_O";
}
kind_name_KIND_LISPALLOC_core__Rational_O:
{
return "KIND_LISPALLOC_core__Rational_O";
}
kind_name_KIND_LISPALLOC_core__FunctionFrame_O:
{
return "KIND_LISPALLOC_core__FunctionFrame_O";
}
kind_name_KIND_LISPALLOC_core__SaveArchive_O:
{
return "KIND_LISPALLOC_core__SaveArchive_O";
}
kind_name_KIND_LISPALLOC_core__DirectoryEntry_O:
{
return "KIND_LISPALLOC_core__DirectoryEntry_O";
}
kind_name_KIND_BOOTSTRAP_core__Symbol_O:
{
return "KIND_BOOTSTRAP_core__Symbol_O";
}
kind_name_KIND_BOOTSTRAP_core__Str_O:
{
return "KIND_BOOTSTRAP_core__Str_O";
}
kind_name_KIND_LISPALLOC_core__Function_O:
{
return "KIND_LISPALLOC_core__Function_O";
}
kind_name_KIND_LISPALLOC_core__LogicalPathname_O:
{
return "KIND_LISPALLOC_core__LogicalPathname_O";
}
kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
return "KIND_LISPALLOC_core__LexicalEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Environment_O:
{
return "KIND_LISPALLOC_core__Environment_O";
}
kind_name_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
return "KIND_LISPALLOC_core__StackValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__BlockEnvironment_O:
{
return "KIND_LISPALLOC_core__BlockEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__SourcePosInfo_O:
{
return "KIND_LISPALLOC_core__SourcePosInfo_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__";
}
kind_name_KIND_LISPALLOC_core__SourceManager_O:
{
return "KIND_LISPALLOC_core__SourceManager_O";
}
kind_name_KIND_LISPALLOC_core__CatchEnvironment_O:
{
return "KIND_LISPALLOC_core__CatchEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__FileStatus_O:
{
return "KIND_LISPALLOC_core__FileStatus_O";
}
kind_name_KIND_CLASSALLOC_core__MacroClosure:
{
return "KIND_CLASSALLOC_core__MacroClosure";
}
kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
return "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure";
}
kind_name_KIND_LISPALLOC_core__String_O:
{
return "KIND_LISPALLOC_core__String_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_";
}
kind_name_KIND_LISPALLOC_core__Path_O:
{
return "KIND_LISPALLOC_core__Path_O";
}
kind_name_KIND_LISPALLOC_core__Real_O:
{
return "KIND_LISPALLOC_core__Real_O";
}
kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
return "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__TagbodyFrame_O:
{
return "KIND_LISPALLOC_core__TagbodyFrame_O";
}
#endif // defined(GC_KIND_NAME_MAP)
#if defined(GC_KIND_NAME_MAP_HELPERS)

#endif // defined(GC_KIND_NAME_MAP_HELPERS)
#if defined(GC_KIND_NAME_MAP_TABLE)
static void* KIND_NAME_MAP_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 5 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 6 */ , &&kind_name_KIND_LISPALLOC_core__Archive_O
  /* 7 */ , &&kind_name_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 8 */ , &&kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 9 */ , &&kind_name_KIND_LISPALLOC_core__LoadArchive_O
  /* 10 */ , &&kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 11 */ , &&kind_name_KIND_LISPALLOC_core__Pathname_O
  /* 12 */ , &&kind_name_KIND_LISPALLOC_core__ShortFloat_O
  /* 13 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 14 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 15 */ , &&kind_name_KIND_LISPALLOC_core__Float_O
  /* 16 */ , &&kind_name_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 18 */ , &&kind_name_KIND_LISPALLOC_core__CandoException_O
  /* 19 */ , &&kind_name_KIND_LISPALLOC_core__Cons_O
  /* 20 */ , &&kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 21 */ , &&kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 22 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 23 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 24 */ , &&kind_name_KIND_LISPALLOC_core__Pointer_O
  /* 25 */ , &&kind_name_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 26 */ , &&kind_name_KIND_LISPALLOC_core__CxxObject_O
  /* 27 */ , &&kind_name_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 28 */ , &&kind_name_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 29 */ , &&kind_name_KIND_LISPALLOC_core__Ratio_O
  /* 30 */ , &&kind_name_KIND_BOOTSTRAP_core__Class_O
  /* 31 */ , &&kind_name_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 32 */ , &&kind_name_KIND_LISPALLOC_core__LongFloat_O
  /* 33 */ , &&kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 34 */ , &&kind_name_KIND_LISPALLOC_core__ValueFrame_O
  /* 35 */ , &&kind_name_KIND_BOOTSTRAP_core__Metaobject_O
  /* 36 */ , &&kind_name_KIND_LISPALLOC_core__Bignum_O
  /* 37 */ , &&kind_name_KIND_LISPALLOC_core__WeakHashTable_O
  /* 38 */ , &&kind_name_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 39 */ , &&kind_name_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 40 */ , &&kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 41 */ , &&kind_name_KIND_LISPALLOC_core__Integer_O
  /* 42 */ , &&kind_name_KIND_LISPALLOC_core__Number_O
  /* 43 */ , &&kind_name_KIND_LISPALLOC_core__VectorObjects_O
  /* 44 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 45 */ , &&kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 46 */ , &&kind_name_KIND_LISPALLOC_core__DoubleFloat_O
  /* 47 */ , &&kind_name_KIND_LISPALLOC_core__Iterator_O
  /* 48 */ , &&kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 49 */ , &&kind_name_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 50 */ , &&kind_name_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , &&kind_name_KIND_BOOTSTRAP_core__Specializer_O
  /* 52 */ , &&kind_name_KIND_LISPALLOC_core__Array_O
  /* 53 */ , &&kind_name_KIND_LISPALLOC_core__CompiledFunction_O
  /* 54 */ , &&kind_name_KIND_LISPALLOC_core__Vector_O
  /* 55 */ , &&kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 56 */ , &&kind_name_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 57 */ , &&kind_name_KIND_LISPALLOC_core__Rational_O
  /* 58 */ , &&kind_name_KIND_LISPALLOC_core__FunctionFrame_O
  /* 59 */ , &&kind_name_KIND_LISPALLOC_core__SaveArchive_O
  /* 60 */ , &&kind_name_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 61 */ , &&kind_name_KIND_BOOTSTRAP_core__Symbol_O
  /* 62 */ , &&kind_name_KIND_BOOTSTRAP_core__Str_O
  /* 63 */ , &&kind_name_KIND_LISPALLOC_core__Function_O
  /* 64 */ , &&kind_name_KIND_LISPALLOC_core__LogicalPathname_O
  /* 65 */ , &&kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 66 */ , &&kind_name_KIND_LISPALLOC_core__Environment_O
  /* 67 */ , &&kind_name_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 68 */ , &&kind_name_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 69 */ , &&kind_name_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 70 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 71 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 72 */ , &&kind_name_KIND_LISPALLOC_core__SourceManager_O
  /* 73 */ , &&kind_name_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 74 */ , &&kind_name_KIND_LISPALLOC_core__FileStatus_O
  /* 75 */ , &&kind_name_KIND_CLASSALLOC_core__MacroClosure
  /* 76 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 77 */ , &&kind_name_KIND_LISPALLOC_core__String_O
  /* 78 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 79 */ , &&kind_name_KIND_LISPALLOC_core__Path_O
  /* 80 */ , &&kind_name_KIND_LISPALLOC_core__Real_O
  /* 81 */ , &&kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 82 */ , &&kind_name_KIND_LISPALLOC_core__TagbodyFrame_O
};
#endif // defined(GC_KIND_NAME_MAP_TABLE)
#if defined(GC_OBJ_DUMP_MAP)
obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_core__Creator size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__Archive_O:
{
    typedef core::Archive_O type_KIND_LISPALLOC_core__Archive_O;
    sout << "KIND_LISPALLOC_core__Archive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Archive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    sout << "KIND_LISPALLOC_core__SourceFileInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    sout << "KIND_LISPALLOC_core__CompileTimeEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LoadArchive_O:
{
    typedef core::LoadArchive_O type_KIND_LISPALLOC_core__LoadArchive_O;
    sout << "KIND_LISPALLOC_core__LoadArchive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadArchive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    sout << "KIND_LISPALLOC_core__UnwindProtectEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Pathname_O:
{
    typedef core::Pathname_O type_KIND_LISPALLOC_core__Pathname_O;
    sout << "KIND_LISPALLOC_core__Pathname_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Pathname_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ShortFloat_O:
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    sout << "KIND_LISPALLOC_core__ShortFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    sout << "KIND_LISPALLOC_core__Float_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    sout << "KIND_BOOTSTRAP_core__StandardObject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__CandoException_O:
{
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    sout << "KIND_LISPALLOC_core__CandoException_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Cons_O:
{
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    sout << "KIND_LISPALLOC_core__Cons_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    sout << "KIND_LISPALLOC_core__SymbolToEnumConverter_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    sout << "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Pointer_O:
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    sout << "KIND_LISPALLOC_core__Pointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    sout << "KIND_LISPALLOC_core__Fixnum_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    sout << "KIND_LISPALLOC_core__CxxObject_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    typedef core::RecursiveDirectoryIterator_O type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O;
    sout << "KIND_LISPALLOC_core__RecursiveDirectoryIterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__ValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Ratio_O:
{
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    sout << "KIND_LISPALLOC_core__Ratio_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Class_O:
{
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    sout << "KIND_BOOTSTRAP_core__Class_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    typedef core::VectorObjectsWithFillPtr_O type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O;
    sout << "KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    sout << "KIND_LISPALLOC_core__LongFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__ValueFrame_O:
{
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    sout << "KIND_LISPALLOC_core__ValueFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    sout << "KIND_BOOTSTRAP_core__Metaobject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Bignum_O:
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    sout << "KIND_LISPALLOC_core__Bignum_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakHashTable_O:
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    sout << "KIND_LISPALLOC_core__WeakHashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    sout << "KIND_LISPALLOC_core__SingleFloat_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    sout << "KIND_LISPALLOC_core__WeakKeyHashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    sout << "KIND_LISPALLOC_core__MacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    sout << "KIND_LISPALLOC_core__Integer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    sout << "KIND_LISPALLOC_core__Number_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VectorObjects_O:
{
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    sout << "KIND_LISPALLOC_core__VectorObjects_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    sout << "KIND_LISPALLOC_core__TagbodyEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    sout << "KIND_LISPALLOC_core__DoubleFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Iterator_O:
{
    typedef core::Iterator_O type_KIND_LISPALLOC_core__Iterator_O;
    sout << "KIND_LISPALLOC_core__Iterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Iterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    sout << "KIND_LISPALLOC_core__FunctionContainerEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    sout << "KIND_LISPALLOC_core__GlueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Complex_O:
{
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    sout << "KIND_LISPALLOC_core__Complex_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    sout << "KIND_BOOTSTRAP_core__Specializer_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Array_O:
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    sout << "KIND_LISPALLOC_core__Array_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CompiledFunction_O:
{
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    sout << "KIND_LISPALLOC_core__CompiledFunction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    sout << "KIND_LISPALLOC_core__Vector_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__FunctionValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    typedef core::DirectoryIterator_O type_KIND_LISPALLOC_core__DirectoryIterator_O;
    sout << "KIND_LISPALLOC_core__DirectoryIterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryIterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Rational_O:
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    sout << "KIND_LISPALLOC_core__Rational_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionFrame_O:
{
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    sout << "KIND_LISPALLOC_core__FunctionFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SaveArchive_O:
{
    typedef core::SaveArchive_O type_KIND_LISPALLOC_core__SaveArchive_O;
    sout << "KIND_LISPALLOC_core__SaveArchive_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SaveArchive_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    typedef core::DirectoryEntry_O type_KIND_LISPALLOC_core__DirectoryEntry_O;
    sout << "KIND_LISPALLOC_core__DirectoryEntry_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryEntry_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Symbol_O:
{
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    sout << "KIND_BOOTSTRAP_core__Symbol_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Str_O:
{
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    sout << "KIND_BOOTSTRAP_core__Str_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Function_O:
{
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    sout << "KIND_LISPALLOC_core__Function_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LogicalPathname_O:
{
    typedef core::LogicalPathname_O type_KIND_LISPALLOC_core__LogicalPathname_O;
    sout << "KIND_LISPALLOC_core__LogicalPathname_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LogicalPathname_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    sout << "KIND_LISPALLOC_core__LexicalEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    sout << "KIND_LISPALLOC_core__Environment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    sout << "KIND_LISPALLOC_core__SourcePosInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourceManager_O:
{
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    sout << "KIND_LISPALLOC_core__SourceManager_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    sout << "KIND_LISPALLOC_core__CatchEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FileStatus_O:
{
    typedef core::FileStatus_O type_KIND_LISPALLOC_core__FileStatus_O;
    sout << "KIND_LISPALLOC_core__FileStatus_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStatus_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__MacroClosure:
{
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    sout << "KIND_CLASSALLOC_core__MacroClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__String_O:
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    sout << "KIND_LISPALLOC_core__String_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__Path_O:
{
    typedef core::Path_O type_KIND_LISPALLOC_core__Path_O;
    sout << "KIND_LISPALLOC_core__Path_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Path_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Real_O:
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    sout << "KIND_LISPALLOC_core__Real_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    sout << "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    sout << "KIND_LISPALLOC_core__TagbodyFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O))+global_alignup_sizeof_header) << "]" ;
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
  /* 4 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 5 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 6 */ , &&obj_dump_KIND_LISPALLOC_core__Archive_O
  /* 7 */ , &&obj_dump_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 8 */ , &&obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 9 */ , &&obj_dump_KIND_LISPALLOC_core__LoadArchive_O
  /* 10 */ , &&obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 11 */ , &&obj_dump_KIND_LISPALLOC_core__Pathname_O
  /* 12 */ , &&obj_dump_KIND_LISPALLOC_core__ShortFloat_O
  /* 13 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 14 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 15 */ , &&obj_dump_KIND_LISPALLOC_core__Float_O
  /* 16 */ , &&obj_dump_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_dump_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 18 */ , &&obj_dump_KIND_LISPALLOC_core__CandoException_O
  /* 19 */ , &&obj_dump_KIND_LISPALLOC_core__Cons_O
  /* 20 */ , &&obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 21 */ , &&obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 22 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 23 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 24 */ , &&obj_dump_KIND_LISPALLOC_core__Pointer_O
  /* 25 */ , &&obj_dump_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 26 */ , &&obj_dump_KIND_LISPALLOC_core__CxxObject_O
  /* 27 */ , &&obj_dump_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 28 */ , &&obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 29 */ , &&obj_dump_KIND_LISPALLOC_core__Ratio_O
  /* 30 */ , &&obj_dump_KIND_BOOTSTRAP_core__Class_O
  /* 31 */ , &&obj_dump_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 32 */ , &&obj_dump_KIND_LISPALLOC_core__LongFloat_O
  /* 33 */ , &&obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 34 */ , &&obj_dump_KIND_LISPALLOC_core__ValueFrame_O
  /* 35 */ , &&obj_dump_KIND_BOOTSTRAP_core__Metaobject_O
  /* 36 */ , &&obj_dump_KIND_LISPALLOC_core__Bignum_O
  /* 37 */ , &&obj_dump_KIND_LISPALLOC_core__WeakHashTable_O
  /* 38 */ , &&obj_dump_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 39 */ , &&obj_dump_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 40 */ , &&obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 41 */ , &&obj_dump_KIND_LISPALLOC_core__Integer_O
  /* 42 */ , &&obj_dump_KIND_LISPALLOC_core__Number_O
  /* 43 */ , &&obj_dump_KIND_LISPALLOC_core__VectorObjects_O
  /* 44 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 45 */ , &&obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 46 */ , &&obj_dump_KIND_LISPALLOC_core__DoubleFloat_O
  /* 47 */ , &&obj_dump_KIND_LISPALLOC_core__Iterator_O
  /* 48 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 49 */ , &&obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 50 */ , &&obj_dump_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , &&obj_dump_KIND_BOOTSTRAP_core__Specializer_O
  /* 52 */ , &&obj_dump_KIND_LISPALLOC_core__Array_O
  /* 53 */ , &&obj_dump_KIND_LISPALLOC_core__CompiledFunction_O
  /* 54 */ , &&obj_dump_KIND_LISPALLOC_core__Vector_O
  /* 55 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 56 */ , &&obj_dump_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 57 */ , &&obj_dump_KIND_LISPALLOC_core__Rational_O
  /* 58 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionFrame_O
  /* 59 */ , &&obj_dump_KIND_LISPALLOC_core__SaveArchive_O
  /* 60 */ , &&obj_dump_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 61 */ , &&obj_dump_KIND_BOOTSTRAP_core__Symbol_O
  /* 62 */ , &&obj_dump_KIND_BOOTSTRAP_core__Str_O
  /* 63 */ , &&obj_dump_KIND_LISPALLOC_core__Function_O
  /* 64 */ , &&obj_dump_KIND_LISPALLOC_core__LogicalPathname_O
  /* 65 */ , &&obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 66 */ , &&obj_dump_KIND_LISPALLOC_core__Environment_O
  /* 67 */ , &&obj_dump_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 68 */ , &&obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 69 */ , &&obj_dump_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 70 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 71 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 72 */ , &&obj_dump_KIND_LISPALLOC_core__SourceManager_O
  /* 73 */ , &&obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 74 */ , &&obj_dump_KIND_LISPALLOC_core__FileStatus_O
  /* 75 */ , &&obj_dump_KIND_CLASSALLOC_core__MacroClosure
  /* 76 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 77 */ , &&obj_dump_KIND_LISPALLOC_core__String_O
  /* 78 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 79 */ , &&obj_dump_KIND_LISPALLOC_core__Path_O
  /* 80 */ , &&obj_dump_KIND_LISPALLOC_core__Real_O
  /* 81 */ , &&obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 82 */ , &&obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O
};
#endif // defined(GC_OBJ_DUMP_MAP_TABLE)
#if defined(GC_OBJ_SKIP)
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__Archive_O:
{
    typedef core::Archive_O type_KIND_LISPALLOC_core__Archive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Archive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LoadArchive_O:
{
    typedef core::LoadArchive_O type_KIND_LISPALLOC_core__LoadArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadArchive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Pathname_O:
{
    typedef core::Pathname_O type_KIND_LISPALLOC_core__Pathname_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pathname_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ShortFloat_O:
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__CandoException_O:
{
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Cons_O:
{
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__Pointer_O:
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    typedef core::RecursiveDirectoryIterator_O type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Ratio_O:
{
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Class_O:
{
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    typedef core::VectorObjectsWithFillPtr_O type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__ValueFrame_O:
{
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Bignum_O:
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakHashTable_O:
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VectorObjects_O:
{
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Iterator_O:
{
    typedef core::Iterator_O type_KIND_LISPALLOC_core__Iterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Iterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Complex_O:
{
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Array_O:
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CompiledFunction_O:
{
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    typedef core::DirectoryIterator_O type_KIND_LISPALLOC_core__DirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryIterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Rational_O:
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionFrame_O:
{
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SaveArchive_O:
{
    typedef core::SaveArchive_O type_KIND_LISPALLOC_core__SaveArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SaveArchive_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    typedef core::DirectoryEntry_O type_KIND_LISPALLOC_core__DirectoryEntry_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryEntry_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Symbol_O:
{
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Str_O:
{
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Function_O:
{
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LogicalPathname_O:
{
    typedef core::LogicalPathname_O type_KIND_LISPALLOC_core__LogicalPathname_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LogicalPathname_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__SourceManager_O:
{
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FileStatus_O:
{
    typedef core::FileStatus_O type_KIND_LISPALLOC_core__FileStatus_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStatus_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__MacroClosure:
{
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__String_O:
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__Path_O:
{
    typedef core::Path_O type_KIND_LISPALLOC_core__Path_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Path_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Real_O:
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
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
  /* 4 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 5 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 6 */ , &&obj_skip_KIND_LISPALLOC_core__Archive_O
  /* 7 */ , &&obj_skip_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 8 */ , &&obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 9 */ , &&obj_skip_KIND_LISPALLOC_core__LoadArchive_O
  /* 10 */ , &&obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 11 */ , &&obj_skip_KIND_LISPALLOC_core__Pathname_O
  /* 12 */ , &&obj_skip_KIND_LISPALLOC_core__ShortFloat_O
  /* 13 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 14 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 15 */ , &&obj_skip_KIND_LISPALLOC_core__Float_O
  /* 16 */ , &&obj_skip_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_skip_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 18 */ , &&obj_skip_KIND_LISPALLOC_core__CandoException_O
  /* 19 */ , &&obj_skip_KIND_LISPALLOC_core__Cons_O
  /* 20 */ , &&obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 21 */ , &&obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 22 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 23 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 24 */ , &&obj_skip_KIND_LISPALLOC_core__Pointer_O
  /* 25 */ , &&obj_skip_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 26 */ , &&obj_skip_KIND_LISPALLOC_core__CxxObject_O
  /* 27 */ , &&obj_skip_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 28 */ , &&obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 29 */ , &&obj_skip_KIND_LISPALLOC_core__Ratio_O
  /* 30 */ , &&obj_skip_KIND_BOOTSTRAP_core__Class_O
  /* 31 */ , &&obj_skip_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 32 */ , &&obj_skip_KIND_LISPALLOC_core__LongFloat_O
  /* 33 */ , &&obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 34 */ , &&obj_skip_KIND_LISPALLOC_core__ValueFrame_O
  /* 35 */ , &&obj_skip_KIND_BOOTSTRAP_core__Metaobject_O
  /* 36 */ , &&obj_skip_KIND_LISPALLOC_core__Bignum_O
  /* 37 */ , &&obj_skip_KIND_LISPALLOC_core__WeakHashTable_O
  /* 38 */ , &&obj_skip_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 39 */ , &&obj_skip_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 40 */ , &&obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 41 */ , &&obj_skip_KIND_LISPALLOC_core__Integer_O
  /* 42 */ , &&obj_skip_KIND_LISPALLOC_core__Number_O
  /* 43 */ , &&obj_skip_KIND_LISPALLOC_core__VectorObjects_O
  /* 44 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 45 */ , &&obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 46 */ , &&obj_skip_KIND_LISPALLOC_core__DoubleFloat_O
  /* 47 */ , &&obj_skip_KIND_LISPALLOC_core__Iterator_O
  /* 48 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 49 */ , &&obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 50 */ , &&obj_skip_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , &&obj_skip_KIND_BOOTSTRAP_core__Specializer_O
  /* 52 */ , &&obj_skip_KIND_LISPALLOC_core__Array_O
  /* 53 */ , &&obj_skip_KIND_LISPALLOC_core__CompiledFunction_O
  /* 54 */ , &&obj_skip_KIND_LISPALLOC_core__Vector_O
  /* 55 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 56 */ , &&obj_skip_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 57 */ , &&obj_skip_KIND_LISPALLOC_core__Rational_O
  /* 58 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionFrame_O
  /* 59 */ , &&obj_skip_KIND_LISPALLOC_core__SaveArchive_O
  /* 60 */ , &&obj_skip_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 61 */ , &&obj_skip_KIND_BOOTSTRAP_core__Symbol_O
  /* 62 */ , &&obj_skip_KIND_BOOTSTRAP_core__Str_O
  /* 63 */ , &&obj_skip_KIND_LISPALLOC_core__Function_O
  /* 64 */ , &&obj_skip_KIND_LISPALLOC_core__LogicalPathname_O
  /* 65 */ , &&obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 66 */ , &&obj_skip_KIND_LISPALLOC_core__Environment_O
  /* 67 */ , &&obj_skip_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 68 */ , &&obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 69 */ , &&obj_skip_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 70 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 71 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 72 */ , &&obj_skip_KIND_LISPALLOC_core__SourceManager_O
  /* 73 */ , &&obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 74 */ , &&obj_skip_KIND_LISPALLOC_core__FileStatus_O
  /* 75 */ , &&obj_skip_KIND_CLASSALLOC_core__MacroClosure
  /* 76 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 77 */ , &&obj_skip_KIND_LISPALLOC_core__String_O
  /* 78 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 79 */ , &&obj_skip_KIND_LISPALLOC_core__Path_O
  /* 80 */ , &&obj_skip_KIND_LISPALLOC_core__Real_O
  /* 81 */ , &&obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 82 */ , &&obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O
};
#endif // defined(GC_OBJ_SKIP_TABLE)
#if defined(GC_OBJ_SCAN)
obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
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
obj_scan_KIND_LISPALLOC_core__Archive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Archive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SourceFileInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SourceFileInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CompileTimeEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LoadArchive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LoadArchive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::UnwindProtectEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Pathname_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Pathname_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ShortFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ShortFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
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
obj_scan_KIND_LISPALLOC_core__Float_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Float_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__StandardObject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::StandardObject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    // Should never be invoked
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CandoException_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CandoException_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Cons_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Cons_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SymbolToEnumConverter_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::RuntimeVisibleEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
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
obj_scan_KIND_LISPALLOC_core__Pointer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Pointer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Fixnum_dummy_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CxxObject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CxxObject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::RecursiveDirectoryIterator_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ValueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Ratio_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Ratio_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Class_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Class_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::VectorObjectsWithFillPtr_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LongFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LongFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
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
obj_scan_KIND_LISPALLOC_core__ValueFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::ValueFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Metaobject_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Metaobject_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Bignum_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Bignum_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__WeakHashTable_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::WeakHashTable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SingleFloat_dummy_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::WeakKeyHashTable_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::MacroletEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Integer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Integer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Number_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Number_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__VectorObjects_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::VectorObjects_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
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
obj_scan_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::TagbodyEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DoubleFloat_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::DoubleFloat_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Iterator_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Iterator_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FunctionContainerEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__GlueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::GlueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Complex_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Complex_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Specializer_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Specializer_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Array_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Array_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CompiledFunction_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CompiledFunction_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Vector_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Vector_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FunctionValueEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DirectoryIterator_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::DirectoryIterator_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Rational_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Rational_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FunctionFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SaveArchive_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SaveArchive_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DirectoryEntry_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::DirectoryEntry_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Symbol_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Symbol_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Str_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Str_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Function_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Function_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LogicalPathname_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LogicalPathname_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::LexicalEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Environment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Environment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
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
obj_scan_KIND_LISPALLOC_core__SourcePosInfo_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SourcePosInfo_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
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
obj_scan_KIND_LISPALLOC_core__SourceManager_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SourceManager_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CatchEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::CatchEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FileStatus_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::FileStatus_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_CLASSALLOC_core__MacroClosure:
{
  mps_res_t result = gctools::obj_scan_helper<core::MacroClosure>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
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
obj_scan_KIND_LISPALLOC_core__String_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::String_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
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
obj_scan_KIND_LISPALLOC_core__Path_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Path_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Real_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::Real_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::SymbolMacroletEnvironment_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__TagbodyFrame_O:
{
  mps_res_t result = gctools::obj_scan_helper<core::TagbodyFrame_O>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);
  if ( result != MPS_RES_OK ) return result;
}
goto TOP;
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Archive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Archive_O type_KIND_LISPALLOC_core__Archive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Archive_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::LoadArchive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ObjectForSNode);
    SMART_PTR_FIX(obj_gc_safe->_NodesToFinalize);
    typedef core::LoadArchive_O type_KIND_LISPALLOC_core__LoadArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LoadArchive_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::ShortFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::StandardObject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Pointer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::CxxObject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::LongFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Metaobject_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::WeakHashTable_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Integer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::DoubleFloat_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}
template <>
MAYBE_INLINE mps_res_t obj_scan_helper<core::Iterator_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Iterator_O type_KIND_LISPALLOC_core__Iterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Iterator_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Specializer_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::CompiledFunction_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::DirectoryIterator_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Path);
    typedef core::DirectoryIterator_O type_KIND_LISPALLOC_core__DirectoryIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DirectoryIterator_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::SaveArchive_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_SNodeForObject);
    typedef core::SaveArchive_O type_KIND_LISPALLOC_core__SaveArchive_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SaveArchive_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Str_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_Contents._Contents);
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Environment_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::SourcePosInfo_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::FileStatus_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::FileStatus_O type_KIND_LISPALLOC_core__FileStatus_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FileStatus_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::String_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::Real_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
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
MAYBE_INLINE mps_res_t obj_scan_helper<core::TagbodyFrame_O>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
    return MPS_RES_OK;
}

#endif // defined(GC_OBJ_SCAN_HELPERS)
#if defined(GC_OBJ_SCAN_TABLE)
static void* OBJ_SCAN_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 5 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 6 */ , &&obj_scan_KIND_LISPALLOC_core__Archive_O
  /* 7 */ , &&obj_scan_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 8 */ , &&obj_scan_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 9 */ , &&obj_scan_KIND_LISPALLOC_core__LoadArchive_O
  /* 10 */ , &&obj_scan_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 11 */ , &&obj_scan_KIND_LISPALLOC_core__Pathname_O
  /* 12 */ , &&obj_scan_KIND_LISPALLOC_core__ShortFloat_O
  /* 13 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 14 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 15 */ , &&obj_scan_KIND_LISPALLOC_core__Float_O
  /* 16 */ , &&obj_scan_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 18 */ , &&obj_scan_KIND_LISPALLOC_core__CandoException_O
  /* 19 */ , &&obj_scan_KIND_LISPALLOC_core__Cons_O
  /* 20 */ , &&obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 21 */ , &&obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 22 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 23 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 24 */ , &&obj_scan_KIND_LISPALLOC_core__Pointer_O
  /* 25 */ , &&obj_scan_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 26 */ , &&obj_scan_KIND_LISPALLOC_core__CxxObject_O
  /* 27 */ , &&obj_scan_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 28 */ , &&obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 29 */ , &&obj_scan_KIND_LISPALLOC_core__Ratio_O
  /* 30 */ , &&obj_scan_KIND_BOOTSTRAP_core__Class_O
  /* 31 */ , &&obj_scan_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 32 */ , &&obj_scan_KIND_LISPALLOC_core__LongFloat_O
  /* 33 */ , &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 34 */ , &&obj_scan_KIND_LISPALLOC_core__ValueFrame_O
  /* 35 */ , &&obj_scan_KIND_BOOTSTRAP_core__Metaobject_O
  /* 36 */ , &&obj_scan_KIND_LISPALLOC_core__Bignum_O
  /* 37 */ , &&obj_scan_KIND_LISPALLOC_core__WeakHashTable_O
  /* 38 */ , &&obj_scan_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 39 */ , &&obj_scan_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 40 */ , &&obj_scan_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 41 */ , &&obj_scan_KIND_LISPALLOC_core__Integer_O
  /* 42 */ , &&obj_scan_KIND_LISPALLOC_core__Number_O
  /* 43 */ , &&obj_scan_KIND_LISPALLOC_core__VectorObjects_O
  /* 44 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 45 */ , &&obj_scan_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 46 */ , &&obj_scan_KIND_LISPALLOC_core__DoubleFloat_O
  /* 47 */ , &&obj_scan_KIND_LISPALLOC_core__Iterator_O
  /* 48 */ , &&obj_scan_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 49 */ , &&obj_scan_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 50 */ , &&obj_scan_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , &&obj_scan_KIND_BOOTSTRAP_core__Specializer_O
  /* 52 */ , &&obj_scan_KIND_LISPALLOC_core__Array_O
  /* 53 */ , &&obj_scan_KIND_LISPALLOC_core__CompiledFunction_O
  /* 54 */ , &&obj_scan_KIND_LISPALLOC_core__Vector_O
  /* 55 */ , &&obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 56 */ , &&obj_scan_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 57 */ , &&obj_scan_KIND_LISPALLOC_core__Rational_O
  /* 58 */ , &&obj_scan_KIND_LISPALLOC_core__FunctionFrame_O
  /* 59 */ , &&obj_scan_KIND_LISPALLOC_core__SaveArchive_O
  /* 60 */ , &&obj_scan_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 61 */ , &&obj_scan_KIND_BOOTSTRAP_core__Symbol_O
  /* 62 */ , &&obj_scan_KIND_BOOTSTRAP_core__Str_O
  /* 63 */ , &&obj_scan_KIND_LISPALLOC_core__Function_O
  /* 64 */ , &&obj_scan_KIND_LISPALLOC_core__LogicalPathname_O
  /* 65 */ , &&obj_scan_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 66 */ , &&obj_scan_KIND_LISPALLOC_core__Environment_O
  /* 67 */ , &&obj_scan_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 68 */ , &&obj_scan_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 69 */ , &&obj_scan_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 70 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 71 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 72 */ , &&obj_scan_KIND_LISPALLOC_core__SourceManager_O
  /* 73 */ , &&obj_scan_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 74 */ , &&obj_scan_KIND_LISPALLOC_core__FileStatus_O
  /* 75 */ , &&obj_scan_KIND_CLASSALLOC_core__MacroClosure
  /* 76 */ , &&obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 77 */ , &&obj_scan_KIND_LISPALLOC_core__String_O
  /* 78 */ , &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 79 */ , &&obj_scan_KIND_LISPALLOC_core__Path_O
  /* 80 */ , &&obj_scan_KIND_LISPALLOC_core__Real_O
  /* 81 */ , &&obj_scan_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 82 */ , &&obj_scan_KIND_LISPALLOC_core__TagbodyFrame_O
};
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    obj_gc_safe->~Creator();
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_finalize_KIND_LISPALLOC_core__Archive_O:
{
    core::Archive_O* obj_gc_safe = reinterpret_cast<core::Archive_O*>(client);
    obj_gc_safe->~Archive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    obj_gc_safe->~SourceFileInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    obj_gc_safe->~CompileTimeEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    obj_gc_safe->~LoadArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    obj_gc_safe->~UnwindProtectEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    obj_gc_safe->~Pathname_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    obj_gc_safe->~ShortFloat_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_finalize_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    obj_gc_safe->~Float_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    obj_gc_safe->~StandardObject_O();
    return;
}
obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
obj_finalize_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    obj_gc_safe->~CandoException_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    obj_gc_safe->~Cons_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    obj_gc_safe->~SymbolToEnumConverter_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    obj_gc_safe->~RuntimeVisibleEnvironment_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_finalize_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    obj_gc_safe->~Fixnum_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    obj_gc_safe->~CxxObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    obj_gc_safe->~RecursiveDirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    obj_gc_safe->~ValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    obj_gc_safe->~Ratio_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    obj_gc_safe->~Class_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    obj_gc_safe->~VectorObjectsWithFillPtr_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    obj_gc_safe->~LongFloat_O();
    return;
}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_finalize_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    obj_gc_safe->~ValueFrame_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    obj_gc_safe->~Metaobject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    obj_gc_safe->~Bignum_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    obj_gc_safe->~WeakHashTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    obj_gc_safe->~SingleFloat_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    obj_gc_safe->~WeakKeyHashTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    obj_gc_safe->~MacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    obj_gc_safe->~Integer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    obj_gc_safe->~Number_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    obj_gc_safe->~VectorObjects_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    obj_gc_safe->~TagbodyEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    obj_gc_safe->~DoubleFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    obj_gc_safe->~Iterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    obj_gc_safe->~FunctionContainerEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    obj_gc_safe->~GlueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    obj_gc_safe->~Complex_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    obj_gc_safe->~Specializer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    obj_gc_safe->~Array_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    obj_gc_safe->~CompiledFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    obj_gc_safe->~Vector_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    obj_gc_safe->~FunctionValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    obj_gc_safe->~DirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    obj_gc_safe->~Rational_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    obj_gc_safe->~FunctionFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SaveArchive_O:
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    obj_gc_safe->~SaveArchive_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    obj_gc_safe->~DirectoryEntry_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    obj_gc_safe->~Symbol_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    obj_gc_safe->~Str_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    obj_gc_safe->~LogicalPathname_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    obj_gc_safe->~LexicalEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    obj_gc_safe->~Environment_O();
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
obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    obj_gc_safe->~SourcePosInfo_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_finalize_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    obj_gc_safe->~SourceManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    obj_gc_safe->~CatchEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    obj_gc_safe->~FileStatus_O();
    return;
}
obj_finalize_KIND_CLASSALLOC_core__MacroClosure:
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    obj_gc_safe->~MacroClosure();
    return;
}
obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    obj_gc_safe->~BuiltinClosure();
}
obj_finalize_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    obj_gc_safe->~String_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_finalize_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    obj_gc_safe->~Path_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    obj_gc_safe->~Real_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    obj_gc_safe->~SymbolMacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    obj_gc_safe->~TagbodyFrame_O();
    return;
}
#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_HELPERS)

#endif // defined(GC_OBJ_FINALIZE_HELPERS)
#if defined(GC_OBJ_FINALIZE_TABLE)
static void* OBJ_FINALIZE_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 5 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 6 */ , &&obj_finalize_KIND_LISPALLOC_core__Archive_O
  /* 7 */ , &&obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 8 */ , &&obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 9 */ , &&obj_finalize_KIND_LISPALLOC_core__LoadArchive_O
  /* 10 */ , &&obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 11 */ , &&obj_finalize_KIND_LISPALLOC_core__Pathname_O
  /* 12 */ , &&obj_finalize_KIND_LISPALLOC_core__ShortFloat_O
  /* 13 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 14 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 15 */ , &&obj_finalize_KIND_LISPALLOC_core__Float_O
  /* 16 */ , &&obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 18 */ , &&obj_finalize_KIND_LISPALLOC_core__CandoException_O
  /* 19 */ , &&obj_finalize_KIND_LISPALLOC_core__Cons_O
  /* 20 */ , &&obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 21 */ , &&obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 22 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 23 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 24 */ , &&obj_finalize_KIND_LISPALLOC_core__Pointer_O
  /* 25 */ , &&obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 26 */ , &&obj_finalize_KIND_LISPALLOC_core__CxxObject_O
  /* 27 */ , &&obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 28 */ , &&obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 29 */ , &&obj_finalize_KIND_LISPALLOC_core__Ratio_O
  /* 30 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Class_O
  /* 31 */ , &&obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 32 */ , &&obj_finalize_KIND_LISPALLOC_core__LongFloat_O
  /* 33 */ , &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 34 */ , &&obj_finalize_KIND_LISPALLOC_core__ValueFrame_O
  /* 35 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O
  /* 36 */ , &&obj_finalize_KIND_LISPALLOC_core__Bignum_O
  /* 37 */ , &&obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O
  /* 38 */ , &&obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 39 */ , &&obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 40 */ , &&obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 41 */ , &&obj_finalize_KIND_LISPALLOC_core__Integer_O
  /* 42 */ , &&obj_finalize_KIND_LISPALLOC_core__Number_O
  /* 43 */ , &&obj_finalize_KIND_LISPALLOC_core__VectorObjects_O
  /* 44 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 45 */ , &&obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 46 */ , &&obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O
  /* 47 */ , &&obj_finalize_KIND_LISPALLOC_core__Iterator_O
  /* 48 */ , &&obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 49 */ , &&obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 50 */ , &&obj_finalize_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Specializer_O
  /* 52 */ , &&obj_finalize_KIND_LISPALLOC_core__Array_O
  /* 53 */ , &&obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O
  /* 54 */ , &&obj_finalize_KIND_LISPALLOC_core__Vector_O
  /* 55 */ , &&obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 56 */ , &&obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 57 */ , &&obj_finalize_KIND_LISPALLOC_core__Rational_O
  /* 58 */ , &&obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O
  /* 59 */ , &&obj_finalize_KIND_LISPALLOC_core__SaveArchive_O
  /* 60 */ , &&obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 61 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Symbol_O
  /* 62 */ , &&obj_finalize_KIND_BOOTSTRAP_core__Str_O
  /* 63 */ , &&obj_finalize_KIND_LISPALLOC_core__Function_O
  /* 64 */ , &&obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O
  /* 65 */ , &&obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 66 */ , &&obj_finalize_KIND_LISPALLOC_core__Environment_O
  /* 67 */ , &&obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 68 */ , &&obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 69 */ , &&obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 70 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 71 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 72 */ , &&obj_finalize_KIND_LISPALLOC_core__SourceManager_O
  /* 73 */ , &&obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 74 */ , &&obj_finalize_KIND_LISPALLOC_core__FileStatus_O
  /* 75 */ , &&obj_finalize_KIND_CLASSALLOC_core__MacroClosure
  /* 76 */ , &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 77 */ , &&obj_finalize_KIND_LISPALLOC_core__String_O
  /* 78 */ , &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 79 */ , &&obj_finalize_KIND_LISPALLOC_core__Path_O
  /* 80 */ , &&obj_finalize_KIND_LISPALLOC_core__Real_O
  /* 81 */ , &&obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 82 */ , &&obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_OBJ_DEALLOCATOR)
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    GCObjectAllocator<core::Creator>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O:
{
    core::Archive_O* obj_gc_safe = reinterpret_cast<core::Archive_O*>(client);
    GCObjectAllocator<core::Archive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    GCObjectAllocator<core::SourceFileInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    GCObjectAllocator<core::CompileTimeEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O:
{
    core::LoadArchive_O* obj_gc_safe = reinterpret_cast<core::LoadArchive_O*>(client);
    GCObjectAllocator<core::LoadArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    GCObjectAllocator<core::UnwindProtectEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    GCObjectAllocator<core::Pathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    GCObjectAllocator<core::ShortFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    GCObjectAllocator<core::Float_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    GCObjectAllocator<core::StandardObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never deallocate gcstrings gctools::GCString_moveable<char>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    GCObjectAllocator<core::CandoException_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O:
{
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    GCObjectAllocator<core::Cons_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    GCObjectAllocator<core::SymbolToEnumConverter_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    GCObjectAllocator<core::RuntimeVisibleEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    GCObjectAllocator<core::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    GCObjectAllocator<core::Fixnum_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    GCObjectAllocator<core::CxxObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    GCObjectAllocator<core::RecursiveDirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    GCObjectAllocator<core::ValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    GCObjectAllocator<core::Ratio_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    GCObjectAllocator<core::Class_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    GCObjectAllocator<core::VectorObjectsWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    GCObjectAllocator<core::LongFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    GCObjectAllocator<core::ValueFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    GCObjectAllocator<core::Metaobject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    GCObjectAllocator<core::Bignum_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    GCObjectAllocator<core::WeakHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    GCObjectAllocator<core::SingleFloat_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    GCObjectAllocator<core::WeakKeyHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    GCObjectAllocator<core::MacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    GCObjectAllocator<core::Integer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    GCObjectAllocator<core::Number_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    GCObjectAllocator<core::VectorObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    GCObjectAllocator<core::TagbodyEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    GCObjectAllocator<core::DoubleFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    GCObjectAllocator<core::Iterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    GCObjectAllocator<core::FunctionContainerEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    GCObjectAllocator<core::GlueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    GCObjectAllocator<core::Complex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    GCObjectAllocator<core::Specializer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    GCObjectAllocator<core::Array_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    GCObjectAllocator<core::CompiledFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    GCObjectAllocator<core::Vector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    GCObjectAllocator<core::FunctionValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    GCObjectAllocator<core::DirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    GCObjectAllocator<core::Rational_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    GCObjectAllocator<core::FunctionFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O:
{
    core::SaveArchive_O* obj_gc_safe = reinterpret_cast<core::SaveArchive_O*>(client);
    GCObjectAllocator<core::SaveArchive_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    GCObjectAllocator<core::DirectoryEntry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    GCObjectAllocator<core::Symbol_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    GCObjectAllocator<core::Str_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    GCObjectAllocator<core::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    GCObjectAllocator<core::LogicalPathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    GCObjectAllocator<core::LexicalEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    GCObjectAllocator<core::Environment_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    GCObjectAllocator<core::SourcePosInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    GCObjectAllocator<core::SourceManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    GCObjectAllocator<core::CatchEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    GCObjectAllocator<core::FileStatus_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__MacroClosure:
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    GCObjectAllocator<core::MacroClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    GCObjectAllocator<core::BuiltinClosure>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    GCObjectAllocator<core::String_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    GCObjectAllocator<core::Path_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    GCObjectAllocator<core::Real_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    GCObjectAllocator<core::SymbolMacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    GCObjectAllocator<core::TagbodyFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
#endif // defined(GC_OBJ_DEALLOCATOR)
#if defined(GC_OBJ_DEALLOCATOR_HELPERS)

#endif // defined(GC_OBJ_DEALLOCATOR_HELPERS)
#if defined(GC_OBJ_DEALLOCATOR_TABLE)
static void* OBJ_DEALLOCATOR_table[] = { NULL 
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
       , NULL /* Skip entry for immediate */
  /* 4 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 5 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 6 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Archive_O
  /* 7 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 8 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 9 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LoadArchive_O
  /* 10 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 11 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O
  /* 12 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O
  /* 13 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 14 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 15 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O
  /* 16 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 18 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O
  /* 19 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Cons_O
  /* 20 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 21 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 22 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 23 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 24 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O
  /* 25 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 26 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O
  /* 27 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O
  /* 28 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 29 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O
  /* 30 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O
  /* 31 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O
  /* 32 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O
  /* 33 */ , &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 34 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O
  /* 35 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O
  /* 36 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O
  /* 37 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O
  /* 38 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 39 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 40 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 41 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O
  /* 42 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O
  /* 43 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O
  /* 44 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 45 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 46 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O
  /* 47 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O
  /* 48 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 49 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 50 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O
  /* 52 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O
  /* 53 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O
  /* 54 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O
  /* 55 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 56 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O
  /* 57 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O
  /* 58 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O
  /* 59 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SaveArchive_O
  /* 60 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O
  /* 61 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O
  /* 62 */ , &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O
  /* 63 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O
  /* 64 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O
  /* 65 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 66 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O
  /* 67 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 68 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 69 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 70 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 71 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 72 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O
  /* 73 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 74 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O
  /* 75 */ , &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__MacroClosure
  /* 76 */ , &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 77 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O
  /* 78 */ , &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 79 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O
  /* 80 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O
  /* 81 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 82 */ , &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O
};
#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)
#if defined(GC_GLOBALS)
 TAGGED_POINTER_FIX(core::Cons_O::static_creator);
 SMART_PTR_FIX(core::Cons_O::___staticClass);
 SMART_PTR_FIX(core::Cons_O::___staticClassSymbol);
#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
