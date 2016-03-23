#ifdef DECLARE_FORWARDS
 namespace core {
    class TagbodyFrame_O;
    class SymbolMacroletEnvironment_O;
    class Real_O;
    class Path_O;
    class InvocationHistoryFrameIterator_O;
    class String_O;
    class CatchEnvironment_O;
    class SourceManager_O;
    class FileStatus_O;
    class SourcePosInfo_O;
    class BlockEnvironment_O;
    class StackValueEnvironment_O;
    class Environment_O;
    class LexicalEnvironment_O;
    class Function_O;
    class ExceptionEntry;
    class LogicalPathname_O;
    class AuxArgument;
    class Str_O;
    class DirectoryEntry_O;
    class Symbol_O;
    class FunctionFrame_O;
    class MacroClosure_O;
    class DynamicBinding;
    class DirectoryIterator_O;
    class Rational_O;
    class FunctionValueEnvironment_O;
    class Vector_O;
    class CompiledFunction_O;
    class Array_O;
    class Specializer_O;
    class LambdaListHandler_O;
    class Complex_O;
    class GlueEnvironment_O;
    class FunctionContainerEnvironment_O;
    class Iterator_O;
    class DoubleFloat_O;
    class TagbodyEnvironment_O;
    class General_O;
    class VectorObjects_O;
    class Number_O;
    class Integer_O;
    class MacroletEnvironment_O;
    class OptionalArgument;
    class SingleFloat_dummy_O;
    class WeakKeyHashTable_O;
    class WeakHashTable_O;
    class Bignum_O;
    class KeywordArgument;
    class Metaobject_O;
    class Package_O;
    class ValueFrame_O;
    class LongFloat_O;
    class Class_O;
    class Ratio_O;
    class CacheRecord;
    class ValueEnvironment_O;
    class RecursiveDirectoryIterator_O;
    class CxxObject_O;
    class Fixnum_dummy_O;
    class Pointer_O;
    class Instance_O;
    class RuntimeVisibleEnvironment_O;
    class CandoException_O;
    class SymbolToEnumConverter_O;
    class StandardObject_O;
    class SymbolClassPair;
    class Float_O;
    class ShortFloat_O;
    class Pathname_O;
    class UnwindProtectEnvironment_O;
    class RequiredArgument;
    class CompileTimeEnvironment_O;
    class SourceFileInfo_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_CONS = 4, 
KIND_CHARACTER = 3, 
KIND_SINGLE_FLOAT = 2, 
KIND_FIXNUM = 1, 
KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O = 5,
KIND_LISPALLOC_core__Function_O = 6,
KIND_BOOTSTRAP_core__Class_O = 7,
KIND_LISPALLOC_core__Complex_O = 8,
KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ = 9,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 10,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 11,
KIND_LISPALLOC_core__Iterator_O = 12,
KIND_LISPALLOC_core__Pathname_O = 13,
KIND_LISPALLOC_core__CxxObject_O = 14,
KIND_LISPALLOC_core__SourcePosInfo_O = 15,
KIND_LISPALLOC_core__DirectoryEntry_O = 16,
KIND_LISPALLOC_core__BlockEnvironment_O = 17,
KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ = 18,
KIND_LISPALLOC_core__ValueEnvironment_O = 19,
KIND_LISPALLOC_core__Instance_O = 20,
KIND_LISPALLOC_core__MacroClosure_O = 21,
KIND_LISPALLOC_core__Number_O = 22,
KIND_LISPALLOC_core__CatchEnvironment_O = 23,
KIND_LISPALLOC_core__SourceManager_O = 24,
KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ = 25,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 26,
KIND_LISPALLOC_core__Package_O = 27,
KIND_LISPALLOC_core__FileStatus_O = 28,
KIND_BOOTSTRAP_core__StandardObject_O = 29,
KIND_LISPALLOC_core__GlueEnvironment_O = 30,
KIND_LISPALLOC_core__Bignum_O = 31,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 32,
KIND_LISPALLOC_core__General_O = 33,
KIND_BOOTSTRAP_core__Specializer_O = 34,
KIND_LISPALLOC_core__WeakHashTable_O = 35,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 36,
KIND_LISPALLOC_core__TagbodyFrame_O = 37,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 38,
KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ = 39,
KIND_LISPALLOC_core__LongFloat_O = 40,
KIND_LISPALLOC_core__CompiledFunction_O = 41,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 42,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 43,
KIND_LISPALLOC_core__MacroletEnvironment_O = 44,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 45,
KIND_LISPALLOC_core__LambdaListHandler_O = 46,
KIND_LISPALLOC_core__SourceFileInfo_O = 47,
KIND_LISPALLOC_core__Array_O = 48,
KIND_LISPALLOC_core__CandoException_O = 49,
KIND_LISPALLOC_core__Ratio_O = 50,
KIND_LISPALLOC_core__Vector_O = 51,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 52,
KIND_LISPALLOC_core__Fixnum_dummy_O = 53,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 54,
KIND_LISPALLOC_core__Pointer_O = 55,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 56,
KIND_LISPALLOC_core__StackValueEnvironment_O = 57,
KIND_LISPALLOC_core__Path_O = 58,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 59,
KIND_LISPALLOC_core__Environment_O = 60,
KIND_LISPALLOC_core__ValueFrame_O = 61,
KIND_LISPALLOC_core__LexicalEnvironment_O = 62,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 63,
KIND_LISPALLOC_core__DoubleFloat_O = 64,
KIND_LISPALLOC_core__String_O = 65,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 66,
KIND_LISPALLOC_core__ShortFloat_O = 67,
KIND_LISPALLOC_core__Real_O = 68,
KIND_LISPALLOC_core__FunctionFrame_O = 69,
KIND_LISPALLOC_core__VectorObjects_O = 70,
KIND_LISPALLOC_core__Float_O = 71,
KIND_BOOTSTRAP_core__Symbol_O = 72,
KIND_LISPALLOC_core__DirectoryIterator_O = 73,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 74,
KIND_BOOTSTRAP_core__Str_O = 75,
KIND_BOOTSTRAP_core__Metaobject_O = 76,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 77,
KIND_LISPALLOC_core__Rational_O = 78,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 79,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__ = 80,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 81,
KIND_LISPALLOC_core__Integer_O = 82,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 83,
KIND_LISPALLOC_core__LogicalPathname_O = 84,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 85,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 86,
  KIND_max = 86
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
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
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::RequiredArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ ;
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
template <> class gctools::GCKind<gctools::GCVector_moveable<core::AuxArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<core::Package_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Package_O ;
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
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
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
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
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
#if defined(GC_OBJ_SKIP)
#endif // defined(GC_OBJ_SKIP)
#if defined(GC_OBJ_SKIP_HELPERS)

#endif // defined(GC_OBJ_SKIP_HELPERS)
#if defined(GC_OBJ_SKIP_TABLE)
static void* OBJ_SKIP_table[] = { 
   NULL
};
#endif // defined(GC_OBJ_SKIP_TABLE)
#if defined(GC_OBJ_SCAN)
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    // Should never be invoked
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
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
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
}
goto SCAN_ADVANCE;
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)
{ class_kind, KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O, sizeof(core::InvocationHistoryFrameIterator_O), 0, "core::InvocationHistoryFrameIterator_O" },
{ class_kind, KIND_LISPALLOC_core__Function_O, sizeof(core::Function_O), 0, "core::Function_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::Function_O),closure), "closure" },
{ class_kind, KIND_BOOTSTRAP_core__Class_O, sizeof(core::Class_O), 0, "core::Class_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Creator_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__Complex_O, sizeof(core::Complex_O), 0, "core::Complex_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_real), "_real" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_imaginary), "_imaginary" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_, sizeof(gctools::GCVector_moveable<core::KeywordArgument>), 0, "gctools::GCVector_moveable<core::KeywordArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::KeywordArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::KeywordArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::KeywordArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::KeywordArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Default), "_Default" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Keyword), "_Keyword" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Sensor._ArgTarget), "_Sensor._ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::KeywordArgument),_Sensor._ArgTargetFrameIndex), "_Sensor._ArgTargetFrameIndex" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__CompileTimeEnvironment_O, sizeof(core::CompileTimeEnvironment_O), 0, "core::CompileTimeEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__Iterator_O, sizeof(core::Iterator_O), 0, "core::Iterator_O" },
{ class_kind, KIND_LISPALLOC_core__Pathname_O, sizeof(core::Pathname_O), 0, "core::Pathname_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Host), "_Host" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Device), "_Device" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Directory), "_Directory" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Type), "_Type" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Pathname_O),_Version), "_Version" },
{ class_kind, KIND_LISPALLOC_core__CxxObject_O, sizeof(core::CxxObject_O), 0, "core::CxxObject_O" },
{ class_kind, KIND_LISPALLOC_core__SourcePosInfo_O, sizeof(core::SourcePosInfo_O), 0, "core::SourcePosInfo_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_FileId), "_FileId" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Filepos), "_Filepos" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Lineno), "_Lineno" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Column), "_Column" },
{ class_kind, KIND_LISPALLOC_core__DirectoryEntry_O, sizeof(core::DirectoryEntry_O), 0, "core::DirectoryEntry_O" },
{ class_kind, KIND_LISPALLOC_core__BlockEnvironment_O, sizeof(core::BlockEnvironment_O), 0, "core::BlockEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_BlockSymbol), "_BlockSymbol" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_, sizeof(gctools::GCVector_moveable<core::RequiredArgument>), 0, "gctools::GCVector_moveable<core::RequiredArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::RequiredArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::RequiredArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::RequiredArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::RequiredArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RequiredArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::RequiredArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{ class_kind, KIND_LISPALLOC_core__ValueEnvironment_O, sizeof(core::ValueEnvironment_O), 0, "core::ValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_SymbolIndex), "_SymbolIndex" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ActivationFrame), "_ActivationFrame" },
{ class_kind, KIND_LISPALLOC_core__Instance_O, sizeof(core::Instance_O), 0, "core::Instance_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Sig), "_Sig" },
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
{ class_kind, KIND_LISPALLOC_core__Number_O, sizeof(core::Number_O), 0, "core::Number_O" },
{ class_kind, KIND_LISPALLOC_core__CatchEnvironment_O, sizeof(core::CatchEnvironment_O), 0, "core::CatchEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__SourceManager_O, sizeof(core::SourceManager_O), 0, "core::SourceManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::WeakKeyHashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SourceManager_O),_SourcePosInfo), "_SourcePosInfo" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_, sizeof(gctools::GCVector_moveable<core::AuxArgument>), 0, "gctools::GCVector_moveable<core::AuxArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::AuxArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::AuxArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::AuxArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::AuxArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::AuxArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::AuxArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::AuxArgument),_Expression), "_Expression" },
{ class_kind, KIND_LISPALLOC_core__FunctionContainerEnvironment_O, sizeof(core::FunctionContainerEnvironment_O), 0, "core::FunctionContainerEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__Package_O, sizeof(core::Package_O), 0, "core::Package_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_Name._Contents), "_Name._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_InternalSymbols), "_InternalSymbols" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_ExternalSymbols), "_ExternalSymbols" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_Shadowing), "_Shadowing" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_UsingPackages._Vector._Contents), "_UsingPackages._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_PackagesUsedBy._Vector._Contents), "_PackagesUsedBy._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Package_O),_Nicknames), "_Nicknames" },
{ class_kind, KIND_LISPALLOC_core__FileStatus_O, sizeof(core::FileStatus_O), 0, "core::FileStatus_O" },
{ class_kind, KIND_BOOTSTRAP_core__StandardObject_O, sizeof(core::StandardObject_O), 0, "core::StandardObject_O" },
{ class_kind, KIND_LISPALLOC_core__GlueEnvironment_O, sizeof(core::GlueEnvironment_O), 0, "core::GlueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Map), "_Map" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Args), "_Args" },
{ class_kind, KIND_LISPALLOC_core__Bignum_O, sizeof(core::Bignum_O), 0, "core::Bignum_O" },
{  fixed_field, ARRAY_OFFSET, sizeof(void), offsetof(SAFE_TYPE_MACRO(core::Bignum_O),_value.mp), "_value.mp" },
{ class_kind, KIND_LISPALLOC_core__FunctionValueEnvironment_O, sizeof(core::FunctionValueEnvironment_O), 0, "core::FunctionValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionIndices), "_FunctionIndices" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::FunctionFrame_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionFrame), "_FunctionFrame" },
{ class_kind, KIND_LISPALLOC_core__General_O, sizeof(core::General_O), 0, "core::General_O" },
{ class_kind, KIND_BOOTSTRAP_core__Specializer_O, sizeof(core::Specializer_O), 0, "core::Specializer_O" },
{ class_kind, KIND_LISPALLOC_core__WeakHashTable_O, sizeof(core::WeakHashTable_O), 0, "core::WeakHashTable_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_, sizeof(gctools::GCVector_moveable<core::ExceptionEntry>), 0, "gctools::GCVector_moveable<core::ExceptionEntry>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Data), "_Data" },
{  variable_capacity, sizeof(core::ExceptionEntry), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Capacity), NULL },
{    variable_field, ctype_core__FrameKind, sizeof(core::FrameKind), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_FrameKind), "_FrameKind" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_Key), "_Key" },
{ class_kind, KIND_LISPALLOC_core__TagbodyFrame_O, sizeof(core::TagbodyFrame_O), 0, "core::TagbodyFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_ParentFrame), "_ParentFrame" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_, sizeof(gctools::GCVector_moveable<core::OptionalArgument>), 0, "gctools::GCVector_moveable<core::OptionalArgument>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::OptionalArgument>),_Data), "_Data" },
{  variable_capacity, sizeof(core::OptionalArgument), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::OptionalArgument>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::OptionalArgument>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_ArgTarget), "_ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_ArgTargetFrameIndex), "_ArgTargetFrameIndex" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_Default), "_Default" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_Sensor._ArgTarget), "_Sensor._ArgTarget" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::OptionalArgument),_Sensor._ArgTargetFrameIndex), "_Sensor._ArgTargetFrameIndex" },
{ class_kind, KIND_LISPALLOC_core__LongFloat_O, sizeof(core::LongFloat_O), 0, "core::LongFloat_O" },
{ class_kind, KIND_LISPALLOC_core__CompiledFunction_O, sizeof(core::CompiledFunction_O), 0, "core::CompiledFunction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Closure_O>), offsetof(SAFE_TYPE_MACRO(core::CompiledFunction_O),closure), "closure" },
{ class_kind, KIND_LISPALLOC_core__SymbolToEnumConverter_O, sizeof(core::SymbolToEnumConverter_O), 0, "core::SymbolToEnumConverter_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_WhatTheEnumsRepresent._Contents), "_WhatTheEnumsRepresent._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToSymbol), "_EnumToSymbol" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_ArchiveSymbolToEnum), "_ArchiveSymbolToEnum" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToArchiveSymbol), "_EnumToArchiveSymbol" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_SymbolToEnum), "_SymbolToEnum" },
{ container_kind, KIND_GCSTRING_gctools__GCString_moveable_char_, sizeof(gctools::GCString_moveable<char>), 0, "gctools::GCString_moveable<char>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Data), "_Data" },
{  variable_capacity, sizeof(char), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Capacity), NULL },
{    variable_field, ctype_char, sizeof(char), 0, "only" },
{ container_jump_table_index, 7, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_core__MacroletEnvironment_O, sizeof(core::MacroletEnvironment_O), 0, "core::MacroletEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Macros), "_Macros" },
{ class_kind, KIND_LISPALLOC_core__SymbolMacroletEnvironment_O, sizeof(core::SymbolMacroletEnvironment_O), 0, "core::SymbolMacroletEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Macros), "_Macros" },
{ class_kind, KIND_LISPALLOC_core__LambdaListHandler_O, sizeof(core::LambdaListHandler_O), 0, "core::LambdaListHandler_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::LambdaListHandler_O),_ClassifiedSymbolList), "_ClassifiedSymbolList" },
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
{ class_kind, KIND_LISPALLOC_core__SourceFileInfo_O, sizeof(core::SourceFileInfo_O), 0, "core::SourceFileInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Pathname_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_pathname), "_pathname" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_FileHandle), "_FileHandle" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugNamestring), "_SourceDebugNamestring" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugOffset), "_SourceDebugOffset" },
{ class_kind, KIND_LISPALLOC_core__Array_O, sizeof(core::Array_O), 0, "core::Array_O" },
{ class_kind, KIND_LISPALLOC_core__CandoException_O, sizeof(core::CandoException_O), 0, "core::CandoException_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::CandoException_O),_message._Contents), "_message._Contents" },
{ class_kind, KIND_LISPALLOC_core__Ratio_O, sizeof(core::Ratio_O), 0, "core::Ratio_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_numerator), "_numerator" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_denominator), "_denominator" },
{ class_kind, KIND_LISPALLOC_core__Vector_O, sizeof(core::Vector_O), 0, "core::Vector_O" },
{ class_kind, KIND_LISPALLOC_core__TagbodyEnvironment_O, sizeof(core::TagbodyEnvironment_O), 0, "core::TagbodyEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Tags), "_Tags" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_TagCode._Vector._Contents), "_TagCode._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ActivationFrame), "_ActivationFrame" },
{ class_kind, KIND_LISPALLOC_core__Fixnum_dummy_O, sizeof(core::Fixnum_dummy_O), 0, "core::Fixnum_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O, sizeof(core::RuntimeVisibleEnvironment_O), 0, "core::RuntimeVisibleEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{ class_kind, KIND_LISPALLOC_core__Pointer_O, sizeof(core::Pointer_O), 0, "core::Pointer_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_, sizeof(gctools::GCVector_moveable<core::DynamicBinding>), 0, "gctools::GCVector_moveable<core::DynamicBinding>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Data), "_Data" },
{  variable_capacity, sizeof(core::DynamicBinding), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Var), "_Var" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Val), "_Val" },
{ class_kind, KIND_LISPALLOC_core__StackValueEnvironment_O, sizeof(core::StackValueEnvironment_O), 0, "core::StackValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Values), "_Values" },
{ class_kind, KIND_LISPALLOC_core__Path_O, sizeof(core::Path_O), 0, "core::Path_O" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_, sizeof(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>), 0, "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__Environment_O, sizeof(core::Environment_O), 0, "core::Environment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Environment_O),_EnvId), "_EnvId" },
{ class_kind, KIND_LISPALLOC_core__ValueFrame_O, sizeof(core::ValueFrame_O), 0, "core::ValueFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_ParentFrame), "_ParentFrame" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Objects._Array._Contents), "_Objects._Array._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_DebuggingInfo), "_DebuggingInfo" },
{ class_kind, KIND_LISPALLOC_core__LexicalEnvironment_O, sizeof(core::LexicalEnvironment_O), 0, "core::LexicalEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_Metadata), "_Metadata" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__DoubleFloat_O, sizeof(core::DoubleFloat_O), 0, "core::DoubleFloat_O" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::DoubleFloat_O),_Value), "_Value" },
{ class_kind, KIND_LISPALLOC_core__String_O, sizeof(core::String_O), 0, "core::String_O" },
{ class_kind, KIND_LISPALLOC_core__SingleFloat_dummy_O, sizeof(core::SingleFloat_dummy_O), 0, "core::SingleFloat_dummy_O" },
{ class_kind, KIND_LISPALLOC_core__ShortFloat_O, sizeof(core::ShortFloat_O), 0, "core::ShortFloat_O" },
{  fixed_field, ctype_float, sizeof(float), offsetof(SAFE_TYPE_MACRO(core::ShortFloat_O),_Value), "_Value" },
{ class_kind, KIND_LISPALLOC_core__Real_O, sizeof(core::Real_O), 0, "core::Real_O" },
{ class_kind, KIND_LISPALLOC_core__FunctionFrame_O, sizeof(core::FunctionFrame_O), 0, "core::FunctionFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_ParentFrame), "_ParentFrame" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Objects._Array._Contents), "_Objects._Array._Contents" },
{ class_kind, KIND_LISPALLOC_core__VectorObjects_O, sizeof(core::VectorObjects_O), 0, "core::VectorObjects_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_ElementType), "_ElementType" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_Values._Vector._Contents), "_Values._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__Float_O, sizeof(core::Float_O), 0, "core::Float_O" },
{ class_kind, KIND_BOOTSTRAP_core__Symbol_O, sizeof(core::Symbol_O), 0, "core::Symbol_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Str_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_HomePackage), "_HomePackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Value), "_Value" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Function), "_Function" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_SetfFunction), "_SetfFunction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_PropertyList), "_PropertyList" },
{ class_kind, KIND_LISPALLOC_core__DirectoryIterator_O, sizeof(core::DirectoryIterator_O), 0, "core::DirectoryIterator_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Path_O>), offsetof(SAFE_TYPE_MACRO(core::DirectoryIterator_O),_Path), "_Path" },
{ class_kind, KIND_LISPALLOC_core__WeakKeyHashTable_O, sizeof(core::WeakKeyHashTable_O), 0, "core::WeakKeyHashTable_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Length), "_HashTable._Length" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::WeakLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Keys), "_HashTable._Keys" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::StrongLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Values), "_HashTable._Values" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._epoch), "_HashTable._LocationDependency._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._rs), "_HashTable._LocationDependency._rs" },
{ class_kind, KIND_BOOTSTRAP_core__Str_O, sizeof(core::Str_O), 0, "core::Str_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::Str_O),_Contents._Contents), "_Contents._Contents" },
{ class_kind, KIND_BOOTSTRAP_core__Metaobject_O, sizeof(core::Metaobject_O), 0, "core::Metaobject_O" },
{ class_kind, KIND_LISPALLOC_core__RecursiveDirectoryIterator_O, sizeof(core::RecursiveDirectoryIterator_O), 0, "core::RecursiveDirectoryIterator_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Path_O>), offsetof(SAFE_TYPE_MACRO(core::RecursiveDirectoryIterator_O),_Path), "_Path" },
{ class_kind, KIND_LISPALLOC_core__Rational_O, sizeof(core::Rational_O), 0, "core::Rational_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SequenceStepper_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SequenceStepper_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_, sizeof(gctools::GCVector_moveable<core::CacheRecord>), 0, "gctools::GCVector_moveable<core::CacheRecord>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Data), "_Data" },
{  variable_capacity, sizeof(core::CacheRecord), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_key), "_key" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_value), "_value" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_generation), "_generation" },
{ class_kind, KIND_LISPALLOC_core__Integer_O, sizeof(core::Integer_O), 0, "core::Integer_O" },
{ class_kind, KIND_LISPALLOC_core__UnwindProtectEnvironment_O, sizeof(core::UnwindProtectEnvironment_O), 0, "core::UnwindProtectEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_CleanupForm), "_CleanupForm" },
{ class_kind, KIND_LISPALLOC_core__LogicalPathname_O, sizeof(core::LogicalPathname_O), 0, "core::LogicalPathname_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Host), "_Host" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Device), "_Device" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Directory), "_Directory" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Type), "_Type" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LogicalPathname_O),_Version), "_Version" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_, sizeof(gctools::GCVector_moveable<core::SymbolClassPair>), 0, "gctools::GCVector_moveable<core::SymbolClassPair>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Data), "_Data" },
{  variable_capacity, sizeof(core::SymbolClassPair), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),symbol), "symbol" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),theClass), "theClass" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), 0, "only" },

#endif // defined(GC_OBJ_SCAN_HELPERS)
#if defined(GC_OBJ_SCAN_TABLE)
static void* OBJ_SCAN_table[] = { 
  /* 9 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_,
  /* 10 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 18 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_,
  /* 25 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_,
  /* 36 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 38 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 39 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_,
  /* 43 */ &&obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 56 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 59 */ &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 63 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 79 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 80 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__,
  /* 81 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 85 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 86 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
   NULL
};
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
obj_finalize_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    core::InvocationHistoryFrameIterator_O* obj_gc_safe = reinterpret_cast<core::InvocationHistoryFrameIterator_O*>(client);
    obj_gc_safe->~InvocationHistoryFrameIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    obj_gc_safe->~Class_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    obj_gc_safe->~Complex_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::KeywordArgument>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    obj_gc_safe->~CompileTimeEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    obj_gc_safe->~Iterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    obj_gc_safe->~Pathname_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    obj_gc_safe->~CxxObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    obj_gc_safe->~SourcePosInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    obj_gc_safe->~DirectoryEntry_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    obj_gc_safe->~BlockEnvironment_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::RequiredArgument>"));}
obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    obj_gc_safe->~ValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    obj_gc_safe->~Instance_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__MacroClosure_O:
{
    core::MacroClosure_O* obj_gc_safe = reinterpret_cast<core::MacroClosure_O*>(client);
    obj_gc_safe->~MacroClosure_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    obj_gc_safe->~Number_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    obj_gc_safe->~CatchEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    obj_gc_safe->~SourceManager_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::AuxArgument>"));}
obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    obj_gc_safe->~FunctionContainerEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Package_O:
{
    core::Package_O* obj_gc_safe = reinterpret_cast<core::Package_O*>(client);
    obj_gc_safe->~Package_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    obj_gc_safe->~FileStatus_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    obj_gc_safe->~StandardObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    obj_gc_safe->~GlueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    obj_gc_safe->~Bignum_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    obj_gc_safe->~FunctionValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    obj_gc_safe->~General_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    obj_gc_safe->~Specializer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    obj_gc_safe->~WeakHashTable_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    obj_gc_safe->~TagbodyFrame_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::OptionalArgument>"));}
obj_finalize_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    obj_gc_safe->~LongFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    obj_gc_safe->~CompiledFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    obj_gc_safe->~SymbolToEnumConverter_O();
    return;
}
obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    obj_gc_safe->~MacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    obj_gc_safe->~SymbolMacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    core::LambdaListHandler_O* obj_gc_safe = reinterpret_cast<core::LambdaListHandler_O*>(client);
    obj_gc_safe->~LambdaListHandler_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    obj_gc_safe->~SourceFileInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    obj_gc_safe->~Array_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    obj_gc_safe->~CandoException_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    obj_gc_safe->~Ratio_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    obj_gc_safe->~Vector_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    obj_gc_safe->~TagbodyEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    obj_gc_safe->~Fixnum_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    obj_gc_safe->~RuntimeVisibleEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    obj_gc_safe->~StackValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    obj_gc_safe->~Path_O();
    return;
}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_finalize_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    obj_gc_safe->~Environment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    obj_gc_safe->~ValueFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    obj_gc_safe->~LexicalEnvironment_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    obj_gc_safe->~DoubleFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    obj_gc_safe->~String_O();
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
obj_finalize_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    obj_gc_safe->~Real_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    obj_gc_safe->~FunctionFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    obj_gc_safe->~VectorObjects_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    obj_gc_safe->~Float_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    obj_gc_safe->~Symbol_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    obj_gc_safe->~DirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    obj_gc_safe->~WeakKeyHashTable_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    obj_gc_safe->~Str_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    obj_gc_safe->~Metaobject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    obj_gc_safe->~RecursiveDirectoryIterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    obj_gc_safe->~Rational_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_finalize_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    obj_gc_safe->~Integer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    obj_gc_safe->~UnwindProtectEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    obj_gc_safe->~LogicalPathname_O();
    return;
}
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
  /* 5 */ &&obj_finalize_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O,
  /* 6 */ &&obj_finalize_KIND_LISPALLOC_core__Function_O,
  /* 7 */ &&obj_finalize_KIND_BOOTSTRAP_core__Class_O,
  /* 8 */ &&obj_finalize_KIND_LISPALLOC_core__Complex_O,
  /* 9 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_,
  /* 10 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 11 */ &&obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 12 */ &&obj_finalize_KIND_LISPALLOC_core__Iterator_O,
  /* 13 */ &&obj_finalize_KIND_LISPALLOC_core__Pathname_O,
  /* 14 */ &&obj_finalize_KIND_LISPALLOC_core__CxxObject_O,
  /* 15 */ &&obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 16 */ &&obj_finalize_KIND_LISPALLOC_core__DirectoryEntry_O,
  /* 17 */ &&obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 18 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_,
  /* 19 */ &&obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 20 */ &&obj_finalize_KIND_LISPALLOC_core__Instance_O,
  /* 21 */ &&obj_finalize_KIND_LISPALLOC_core__MacroClosure_O,
  /* 22 */ &&obj_finalize_KIND_LISPALLOC_core__Number_O,
  /* 23 */ &&obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 24 */ &&obj_finalize_KIND_LISPALLOC_core__SourceManager_O,
  /* 25 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_,
  /* 26 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 27 */ &&obj_finalize_KIND_LISPALLOC_core__Package_O,
  /* 28 */ &&obj_finalize_KIND_LISPALLOC_core__FileStatus_O,
  /* 29 */ &&obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 30 */ &&obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O,
  /* 31 */ &&obj_finalize_KIND_LISPALLOC_core__Bignum_O,
  /* 32 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 33 */ &&obj_finalize_KIND_LISPALLOC_core__General_O,
  /* 34 */ &&obj_finalize_KIND_BOOTSTRAP_core__Specializer_O,
  /* 35 */ &&obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 36 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 37 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 38 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 39 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_,
  /* 40 */ &&obj_finalize_KIND_LISPALLOC_core__LongFloat_O,
  /* 41 */ &&obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 42 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 43 */ &&obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 44 */ &&obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 45 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 46 */ &&obj_finalize_KIND_LISPALLOC_core__LambdaListHandler_O,
  /* 47 */ &&obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 48 */ &&obj_finalize_KIND_LISPALLOC_core__Array_O,
  /* 49 */ &&obj_finalize_KIND_LISPALLOC_core__CandoException_O,
  /* 50 */ &&obj_finalize_KIND_LISPALLOC_core__Ratio_O,
  /* 51 */ &&obj_finalize_KIND_LISPALLOC_core__Vector_O,
  /* 52 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 53 */ &&obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 54 */ &&obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 55 */ &&obj_finalize_KIND_LISPALLOC_core__Pointer_O,
  /* 56 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 57 */ &&obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 58 */ &&obj_finalize_KIND_LISPALLOC_core__Path_O,
  /* 59 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 60 */ &&obj_finalize_KIND_LISPALLOC_core__Environment_O,
  /* 61 */ &&obj_finalize_KIND_LISPALLOC_core__ValueFrame_O,
  /* 62 */ &&obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 63 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 64 */ &&obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 65 */ &&obj_finalize_KIND_LISPALLOC_core__String_O,
  /* 66 */ &&obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 67 */ &&obj_finalize_KIND_LISPALLOC_core__ShortFloat_O,
  /* 68 */ &&obj_finalize_KIND_LISPALLOC_core__Real_O,
  /* 69 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 70 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjects_O,
  /* 71 */ &&obj_finalize_KIND_LISPALLOC_core__Float_O,
  /* 72 */ &&obj_finalize_KIND_BOOTSTRAP_core__Symbol_O,
  /* 73 */ &&obj_finalize_KIND_LISPALLOC_core__DirectoryIterator_O,
  /* 74 */ &&obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 75 */ &&obj_finalize_KIND_BOOTSTRAP_core__Str_O,
  /* 76 */ &&obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 77 */ &&obj_finalize_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O,
  /* 78 */ &&obj_finalize_KIND_LISPALLOC_core__Rational_O,
  /* 79 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 80 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__,
  /* 81 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 82 */ &&obj_finalize_KIND_LISPALLOC_core__Integer_O,
  /* 83 */ &&obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 84 */ &&obj_finalize_KIND_LISPALLOC_core__LogicalPathname_O,
  /* 85 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 86 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
   NULL
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_OBJ_DEALLOCATOR)
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    core::InvocationHistoryFrameIterator_O* obj_gc_safe = reinterpret_cast<core::InvocationHistoryFrameIterator_O*>(client);
    GC<core::InvocationHistoryFrameIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    GC<core::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    GC<core::Class_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    GC<core::Complex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::KeywordArgument>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    GC<core::CompileTimeEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    GC<core::Iterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O:
{
    core::Pathname_O* obj_gc_safe = reinterpret_cast<core::Pathname_O*>(client);
    GC<core::Pathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    GC<core::CxxObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    GC<core::SourcePosInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O:
{
    core::DirectoryEntry_O* obj_gc_safe = reinterpret_cast<core::DirectoryEntry_O*>(client);
    GC<core::DirectoryEntry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    GC<core::BlockEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::RequiredArgument>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    GC<core::ValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    GC<core::Instance_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroClosure_O:
{
    core::MacroClosure_O* obj_gc_safe = reinterpret_cast<core::MacroClosure_O*>(client);
    GC<core::MacroClosure_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    GC<core::Number_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    GC<core::CatchEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    GC<core::SourceManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::AuxArgument>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    GC<core::FunctionContainerEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Package_O:
{
    core::Package_O* obj_gc_safe = reinterpret_cast<core::Package_O*>(client);
    GC<core::Package_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O:
{
    core::FileStatus_O* obj_gc_safe = reinterpret_cast<core::FileStatus_O*>(client);
    GC<core::FileStatus_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    GC<core::StandardObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    GC<core::GlueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    GC<core::Bignum_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    GC<core::FunctionValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    GC<core::General_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    GC<core::Specializer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    GC<core::WeakHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    GC<core::TagbodyFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::OptionalArgument>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    GC<core::LongFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    GC<core::CompiledFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    GC<core::SymbolToEnumConverter_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never deallocate gcstrings gctools::GCString_moveable<char>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    GC<core::MacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    GC<core::SymbolMacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    core::LambdaListHandler_O* obj_gc_safe = reinterpret_cast<core::LambdaListHandler_O*>(client);
    GC<core::LambdaListHandler_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    GC<core::SourceFileInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    GC<core::Array_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    GC<core::CandoException_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    GC<core::Ratio_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    GC<core::Vector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    GC<core::TagbodyEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    GC<core::Fixnum_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    GC<core::RuntimeVisibleEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    GC<core::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    GC<core::StackValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O:
{
    core::Path_O* obj_gc_safe = reinterpret_cast<core::Path_O*>(client);
    GC<core::Path_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    GC<core::Environment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    GC<core::ValueFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    GC<core::LexicalEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    GC<core::DoubleFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    GC<core::String_O>::deallocate_unmanaged_instance(obj_gc_safe);
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
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    GC<core::Real_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    GC<core::FunctionFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    GC<core::VectorObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    GC<core::Float_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    GC<core::Symbol_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O:
{
    core::DirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::DirectoryIterator_O*>(client);
    GC<core::DirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    GC<core::WeakKeyHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    GC<core::Str_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    GC<core::Metaobject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O:
{
    core::RecursiveDirectoryIterator_O* obj_gc_safe = reinterpret_cast<core::RecursiveDirectoryIterator_O*>(client);
    GC<core::RecursiveDirectoryIterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    GC<core::Rational_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    GC<core::Integer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    GC<core::UnwindProtectEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O:
{
    core::LogicalPathname_O* obj_gc_safe = reinterpret_cast<core::LogicalPathname_O*>(client);
    GC<core::LogicalPathname_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
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
  /* 5 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O,
  /* 6 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O,
  /* 7 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O,
  /* 8 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O,
  /* 9 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_,
  /* 10 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 11 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 12 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O,
  /* 13 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pathname_O,
  /* 14 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O,
  /* 15 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 16 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryEntry_O,
  /* 17 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 18 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_,
  /* 19 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 20 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O,
  /* 21 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroClosure_O,
  /* 22 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O,
  /* 23 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 24 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O,
  /* 25 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_,
  /* 26 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 27 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Package_O,
  /* 28 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FileStatus_O,
  /* 29 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 30 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O,
  /* 31 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O,
  /* 32 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 33 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O,
  /* 34 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O,
  /* 35 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 36 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 37 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 38 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 39 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_,
  /* 40 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O,
  /* 41 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 42 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 43 */ &&obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 44 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 45 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 46 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LambdaListHandler_O,
  /* 47 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 48 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O,
  /* 49 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O,
  /* 50 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O,
  /* 51 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O,
  /* 52 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 53 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 54 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 55 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O,
  /* 56 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 57 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 58 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Path_O,
  /* 59 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 60 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O,
  /* 61 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O,
  /* 62 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 63 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 64 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 65 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O,
  /* 66 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 67 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O,
  /* 68 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O,
  /* 69 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 70 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O,
  /* 71 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O,
  /* 72 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O,
  /* 73 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DirectoryIterator_O,
  /* 74 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 75 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O,
  /* 76 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 77 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RecursiveDirectoryIterator_O,
  /* 78 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O,
  /* 79 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 80 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__,
  /* 81 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 82 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O,
  /* 83 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 84 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LogicalPathname_O,
  /* 85 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 86 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
   NULL
};
#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)
#if defined(GC_GLOBALS)
#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
