#ifdef DECLARE_FORWARDS
 namespace core {
    class CompileTimeEnvironment_O;
    class Complex_O;
    class SourceManager_O;
    class Class_O;
    class HashTableEq_O;
    class FunctionContainerEnvironment_O;
    class Symbol_O;
    class WeakHashTable_O;
    class StackValueEnvironment_O;
    class ExceptionEntry;
    class SymbolMacroletEnvironment_O;
    class WeakKeyHashTable_O;
    class Rational_O;
    class Pointer_O;
    class LambdaListHandler_O;
    class SourceFileInfo_O;
    class Bignum_O;
    class LexicalEnvironment_O;
    class DynamicBinding;
    class Real_O;
    class CompiledFunction_O;
    class TagbodyEnvironment_O;
    class SourcePosInfo_O;
    class OptionalArgument;
    class SymbolClassPair;
    class Array_O;
    class TagbodyFrame_O;
    class ShortFloat_O;
    class BlockEnvironment_O;
    class UnwindProtectEnvironment_O;
    class MacroClosure;
    class ValueFrame_O;
    class SingleFloat_dummy_O;
    class Record_O;
    class String_O;
    class Cons_O;
    class Fixnum_dummy_O;
    class CatchEnvironment_O;
    class GlueEnvironment_O;
    class VectorObjects_O;
    class InvocationHistoryFrameIterator_O;
    class MacroletEnvironment_O;
    class KeywordArgument;
    class CxxObject_O;
    class CacheRecord;
    class SymbolToEnumConverter_O;
    class Specializer_O;
    class FunctionValueEnvironment_O;
    class Str_O;
    class Integer_O;
    class Vector_O;
    class Number_O;
    class RuntimeVisibleEnvironment_O;
    class DoubleFloat_O;
    class StandardObject_O;
    class LongFloat_O;
    class Float_O;
    class Metaobject_O;
    class RequiredArgument;
    class FunctionFrame_O;
    class AuxArgument;
    class Environment_O;
    class Ratio_O;
    class ValueEnvironment_O;
    class CandoException_O;
    class Function_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_TEMPLATED_CLASSALLOC_core__Creator = 1,
KIND_LISPALLOC_core__Function_O = 2,
KIND_LISPALLOC_core__CandoException_O = 3,
KIND_LISPALLOC_core__ValueEnvironment_O = 4,
KIND_LISPALLOC_core__Ratio_O = 5,
KIND_LISPALLOC_core__Environment_O = 6,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 7,
KIND_LISPALLOC_core__FunctionFrame_O = 8,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 9,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 10,
KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ = 11,
KIND_BOOTSTRAP_core__Metaobject_O = 12,
KIND_LISPALLOC_core__Float_O = 13,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 14,
KIND_LISPALLOC_core__LongFloat_O = 15,
KIND_BOOTSTRAP_core__StandardObject_O = 16,
KIND_LISPALLOC_core__DoubleFloat_O = 17,
KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ = 18,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 19,
KIND_LISPALLOC_core__Number_O = 20,
KIND_LISPALLOC_core__Vector_O = 21,
KIND_LISPALLOC_core__Integer_O = 22,
KIND_BOOTSTRAP_core__Str_O = 23,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 24,
KIND_BOOTSTRAP_core__Specializer_O = 25,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 26,
KIND_LISPALLOC_core__CxxObject_O = 27,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__ = 28,
KIND_LISPALLOC_core__MacroletEnvironment_O = 29,
KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ = 30,
KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O = 31,
KIND_LISPALLOC_core__VectorObjects_O = 32,
KIND_LISPALLOC_core__GlueEnvironment_O = 33,
KIND_LISPALLOC_core__CatchEnvironment_O = 34,
KIND_LISPALLOC_core__Fixnum_dummy_O = 35,
KIND_LISPALLOC_core__Cons_O = 36,
KIND_LISPALLOC_core__String_O = 37,
KIND_LISPALLOC_core__Record_O = 38,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 39,
KIND_CLASSALLOC_core__MacroClosure = 40,
KIND_LISPALLOC_core__ValueFrame_O = 41,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 42,
KIND_LISPALLOC_core__BlockEnvironment_O = 43,
KIND_LISPALLOC_core__ShortFloat_O = 44,
KIND_LISPALLOC_core__TagbodyFrame_O = 45,
KIND_LISPALLOC_core__Array_O = 46,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 47,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 48,
KIND_LISPALLOC_core__SourcePosInfo_O = 49,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 50,
KIND_LISPALLOC_core__CompiledFunction_O = 51,
KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ = 52,
KIND_LISPALLOC_core__Real_O = 53,
KIND_LISPALLOC_core__LexicalEnvironment_O = 54,
KIND_LISPALLOC_core__Bignum_O = 55,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 56,
KIND_LISPALLOC_core__SourceFileInfo_O = 57,
KIND_LISPALLOC_core__LambdaListHandler_O = 58,
KIND_LISPALLOC_core__Pointer_O = 59,
KIND_LISPALLOC_core__Rational_O = 60,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 61,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 62,
KIND_LISPALLOC_core__StackValueEnvironment_O = 63,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 64,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 65,
KIND_LISPALLOC_core__WeakHashTable_O = 66,
KIND_BOOTSTRAP_core__Symbol_O = 67,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 68,
KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure = 69,
KIND_BOOTSTRAP_core__Class_O = 70,
KIND_LISPALLOC_core__HashTableEq_O = 71,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 72,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 73,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 74,
KIND_LISPALLOC_core__Complex_O = 75,
KIND_LISPALLOC_core__SourceManager_O = 76,
  KIND_max = 76
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct Cast<core::Creator*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 1 1 
      return (kindVal == 1);
  };
};
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
template <> class gctools::GCKind<core::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Function_O ;
};
template <> class gctools::GCKind<core::CandoException_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CandoException_O ;
};
template <> class gctools::GCKind<core::ValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueEnvironment_O ;
};
template <> class gctools::GCKind<core::Ratio_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Ratio_O ;
};
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::OptionalArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ ;
};
template <> class gctools::GCKind<core::Metaobject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Metaobject_O ;
};
template <> class gctools::GCKind<core::Float_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Float_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::DynamicBinding>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ ;
};
template <> class gctools::GCKind<core::LongFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LongFloat_O ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<core::DoubleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DoubleFloat_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::RequiredArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<core::Number_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Number_O ;
};
template <> class gctools::GCKind<core::Vector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Vector_O ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<core::Str_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Str_O ;
};
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<core::Specializer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Specializer_O ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
template <> class gctools::GCKind<core::CxxObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CxxObject_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__ ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::KeywordArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ ;
};
template <> class gctools::GCKind<core::InvocationHistoryFrameIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O ;
};
template <> class gctools::GCKind<core::VectorObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjects_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
};
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::String_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__String_O ;
};
template <> class gctools::GCKind<core::Record_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Record_O ;
};
template <> class gctools::GCKind<core::SingleFloat_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_dummy_O ;
};
template <> class gctools::GCKind<core::MacroClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__MacroClosure ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<core::ShortFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ShortFloat_O ;
};
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
template <> class gctools::GCKind<core::Array_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Array_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<core::Creator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__Creator ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ ;
};
template <> class gctools::GCKind<core::SourcePosInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourcePosInfo_O ;
};
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::AuxArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ ;
};
template <> class gctools::GCKind<core::SourceFileInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceFileInfo_O ;
};
template <> class gctools::GCKind<core::LambdaListHandler_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LambdaListHandler_O ;
};
template <> class gctools::GCKind<core::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pointer_O ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<core::WeakKeyHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyHashTable_O ;
};
template <> class gctools::GCKind<core::SymbolMacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolMacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::StackValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StackValueEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::ExceptionEntry>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolClassPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ ;
};
template <> class gctools::GCKind<core::WeakHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakHashTable_O ;
};
template <> class gctools::GCKind<core::Symbol_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Symbol_O ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<core::BuiltinClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<core::HashTableEq_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEq_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<core::CompileTimeEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompileTimeEnvironment_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
};
template <> class gctools::GCKind<core::SourceManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceManager_O ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_KIND_NAME_MAP)
kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
return "KIND_TEMPLATED_CLASSALLOC_core__Creator";
}
kind_name_KIND_LISPALLOC_core__Function_O:
{
return "KIND_LISPALLOC_core__Function_O";
}
kind_name_KIND_LISPALLOC_core__CandoException_O:
{
return "KIND_LISPALLOC_core__CandoException_O";
}
kind_name_KIND_LISPALLOC_core__ValueEnvironment_O:
{
return "KIND_LISPALLOC_core__ValueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Ratio_O:
{
return "KIND_LISPALLOC_core__Ratio_O";
}
kind_name_KIND_LISPALLOC_core__Environment_O:
{
return "KIND_LISPALLOC_core__Environment_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__";
}
kind_name_KIND_LISPALLOC_core__FunctionFrame_O:
{
return "KIND_LISPALLOC_core__FunctionFrame_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_";
}
kind_name_KIND_BOOTSTRAP_core__Metaobject_O:
{
return "KIND_BOOTSTRAP_core__Metaobject_O";
}
kind_name_KIND_LISPALLOC_core__Float_O:
{
return "KIND_LISPALLOC_core__Float_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_";
}
kind_name_KIND_LISPALLOC_core__LongFloat_O:
{
return "KIND_LISPALLOC_core__LongFloat_O";
}
kind_name_KIND_BOOTSTRAP_core__StandardObject_O:
{
return "KIND_BOOTSTRAP_core__StandardObject_O";
}
kind_name_KIND_LISPALLOC_core__DoubleFloat_O:
{
return "KIND_LISPALLOC_core__DoubleFloat_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_";
}
kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
return "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Number_O:
{
return "KIND_LISPALLOC_core__Number_O";
}
kind_name_KIND_LISPALLOC_core__Vector_O:
{
return "KIND_LISPALLOC_core__Vector_O";
}
kind_name_KIND_LISPALLOC_core__Integer_O:
{
return "KIND_LISPALLOC_core__Integer_O";
}
kind_name_KIND_BOOTSTRAP_core__Str_O:
{
return "KIND_BOOTSTRAP_core__Str_O";
}
kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
return "KIND_LISPALLOC_core__FunctionValueEnvironment_O";
}
kind_name_KIND_BOOTSTRAP_core__Specializer_O:
{
return "KIND_BOOTSTRAP_core__Specializer_O";
}
kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
return "KIND_LISPALLOC_core__SymbolToEnumConverter_O";
}
kind_name_KIND_LISPALLOC_core__CxxObject_O:
{
return "KIND_LISPALLOC_core__CxxObject_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__";
}
kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
return "KIND_LISPALLOC_core__MacroletEnvironment_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_";
}
kind_name_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
return "KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O";
}
kind_name_KIND_LISPALLOC_core__VectorObjects_O:
{
return "KIND_LISPALLOC_core__VectorObjects_O";
}
kind_name_KIND_LISPALLOC_core__GlueEnvironment_O:
{
return "KIND_LISPALLOC_core__GlueEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__CatchEnvironment_O:
{
return "KIND_LISPALLOC_core__CatchEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
return "KIND_LISPALLOC_core__Fixnum_dummy_O";
}
kind_name_KIND_LISPALLOC_core__Cons_O:
{
return "KIND_LISPALLOC_core__Cons_O";
}
kind_name_KIND_LISPALLOC_core__String_O:
{
return "KIND_LISPALLOC_core__String_O";
}
kind_name_KIND_LISPALLOC_core__Record_O:
{
return "KIND_LISPALLOC_core__Record_O";
}
kind_name_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
return "KIND_LISPALLOC_core__SingleFloat_dummy_O";
}
kind_name_KIND_CLASSALLOC_core__MacroClosure:
{
return "KIND_CLASSALLOC_core__MacroClosure";
}
kind_name_KIND_LISPALLOC_core__ValueFrame_O:
{
return "KIND_LISPALLOC_core__ValueFrame_O";
}
kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
return "KIND_LISPALLOC_core__UnwindProtectEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__BlockEnvironment_O:
{
return "KIND_LISPALLOC_core__BlockEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__ShortFloat_O:
{
return "KIND_LISPALLOC_core__ShortFloat_O";
}
kind_name_KIND_LISPALLOC_core__TagbodyFrame_O:
{
return "KIND_LISPALLOC_core__TagbodyFrame_O";
}
kind_name_KIND_LISPALLOC_core__Array_O:
{
return "KIND_LISPALLOC_core__Array_O";
}
kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
return "KIND_GCSTRING_gctools__GCString_moveable_char_";
}
kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
return "KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_";
}
kind_name_KIND_LISPALLOC_core__SourcePosInfo_O:
{
return "KIND_LISPALLOC_core__SourcePosInfo_O";
}
kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
return "KIND_LISPALLOC_core__TagbodyEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__CompiledFunction_O:
{
return "KIND_LISPALLOC_core__CompiledFunction_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_";
}
kind_name_KIND_LISPALLOC_core__Real_O:
{
return "KIND_LISPALLOC_core__Real_O";
}
kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
return "KIND_LISPALLOC_core__LexicalEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Bignum_O:
{
return "KIND_LISPALLOC_core__Bignum_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__";
}
kind_name_KIND_LISPALLOC_core__SourceFileInfo_O:
{
return "KIND_LISPALLOC_core__SourceFileInfo_O";
}
kind_name_KIND_LISPALLOC_core__LambdaListHandler_O:
{
return "KIND_LISPALLOC_core__LambdaListHandler_O";
}
kind_name_KIND_LISPALLOC_core__Pointer_O:
{
return "KIND_LISPALLOC_core__Pointer_O";
}
kind_name_KIND_LISPALLOC_core__Rational_O:
{
return "KIND_LISPALLOC_core__Rational_O";
}
kind_name_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
return "KIND_LISPALLOC_core__WeakKeyHashTable_O";
}
kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
return "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
return "KIND_LISPALLOC_core__StackValueEnvironment_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_";
}
kind_name_KIND_LISPALLOC_core__WeakHashTable_O:
{
return "KIND_LISPALLOC_core__WeakHashTable_O";
}
kind_name_KIND_BOOTSTRAP_core__Symbol_O:
{
return "KIND_BOOTSTRAP_core__Symbol_O";
}
kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
return "KIND_LISPALLOC_core__FunctionContainerEnvironment_O";
}
kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
return "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure";
}
kind_name_KIND_BOOTSTRAP_core__Class_O:
{
return "KIND_BOOTSTRAP_core__Class_O";
}
kind_name_KIND_LISPALLOC_core__HashTableEq_O:
{
return "KIND_LISPALLOC_core__HashTableEq_O";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_";
}
kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__";
}
kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
return "KIND_LISPALLOC_core__CompileTimeEnvironment_O";
}
kind_name_KIND_LISPALLOC_core__Complex_O:
{
return "KIND_LISPALLOC_core__Complex_O";
}
kind_name_KIND_LISPALLOC_core__SourceManager_O:
{
return "KIND_LISPALLOC_core__SourceManager_O";
}
#endif // defined(GC_KIND_NAME_MAP)
#if defined(GC_KIND_NAME_MAP_TABLE)
static void* KIND_NAME_MAP_table[] = { NULL 
  /* 1 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , &&kind_name_KIND_LISPALLOC_core__Function_O
  /* 3 */ , &&kind_name_KIND_LISPALLOC_core__CandoException_O
  /* 4 */ , &&kind_name_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 5 */ , &&kind_name_KIND_LISPALLOC_core__Ratio_O
  /* 6 */ , &&kind_name_KIND_LISPALLOC_core__Environment_O
  /* 7 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 8 */ , &&kind_name_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 10 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 11 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 12 */ , &&kind_name_KIND_BOOTSTRAP_core__Metaobject_O
  /* 13 */ , &&kind_name_KIND_LISPALLOC_core__Float_O
  /* 14 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 15 */ , &&kind_name_KIND_LISPALLOC_core__LongFloat_O
  /* 16 */ , &&kind_name_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&kind_name_KIND_LISPALLOC_core__DoubleFloat_O
  /* 18 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 19 */ , &&kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 20 */ , &&kind_name_KIND_LISPALLOC_core__Number_O
  /* 21 */ , &&kind_name_KIND_LISPALLOC_core__Vector_O
  /* 22 */ , &&kind_name_KIND_LISPALLOC_core__Integer_O
  /* 23 */ , &&kind_name_KIND_BOOTSTRAP_core__Str_O
  /* 24 */ , &&kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 25 */ , &&kind_name_KIND_BOOTSTRAP_core__Specializer_O
  /* 26 */ , &&kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 27 */ , &&kind_name_KIND_LISPALLOC_core__CxxObject_O
  /* 28 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 29 */ , &&kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 30 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 31 */ , &&kind_name_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 32 */ , &&kind_name_KIND_LISPALLOC_core__VectorObjects_O
  /* 33 */ , &&kind_name_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 34 */ , &&kind_name_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 35 */ , &&kind_name_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 36 */ , &&kind_name_KIND_LISPALLOC_core__Cons_O
  /* 37 */ , &&kind_name_KIND_LISPALLOC_core__String_O
  /* 38 */ , &&kind_name_KIND_LISPALLOC_core__Record_O
  /* 39 */ , &&kind_name_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 40 */ , &&kind_name_KIND_CLASSALLOC_core__MacroClosure
  /* 41 */ , &&kind_name_KIND_LISPALLOC_core__ValueFrame_O
  /* 42 */ , &&kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 43 */ , &&kind_name_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 44 */ , &&kind_name_KIND_LISPALLOC_core__ShortFloat_O
  /* 45 */ , &&kind_name_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 46 */ , &&kind_name_KIND_LISPALLOC_core__Array_O
  /* 47 */ , &&kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 48 */ , &&kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 49 */ , &&kind_name_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 50 */ , &&kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 51 */ , &&kind_name_KIND_LISPALLOC_core__CompiledFunction_O
  /* 52 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 53 */ , &&kind_name_KIND_LISPALLOC_core__Real_O
  /* 54 */ , &&kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 55 */ , &&kind_name_KIND_LISPALLOC_core__Bignum_O
  /* 56 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 57 */ , &&kind_name_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 58 */ , &&kind_name_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 59 */ , &&kind_name_KIND_LISPALLOC_core__Pointer_O
  /* 60 */ , &&kind_name_KIND_LISPALLOC_core__Rational_O
  /* 61 */ , &&kind_name_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 62 */ , &&kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 63 */ , &&kind_name_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 64 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 65 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 66 */ , &&kind_name_KIND_LISPALLOC_core__WeakHashTable_O
  /* 67 */ , &&kind_name_KIND_BOOTSTRAP_core__Symbol_O
  /* 68 */ , &&kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 69 */ , &&kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 70 */ , &&kind_name_KIND_BOOTSTRAP_core__Class_O
  /* 71 */ , &&kind_name_KIND_LISPALLOC_core__HashTableEq_O
  /* 72 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 73 */ , &&kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 74 */ , &&kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 75 */ , &&kind_name_KIND_LISPALLOC_core__Complex_O
  /* 76 */ , &&kind_name_KIND_LISPALLOC_core__SourceManager_O
};
#endif // defined(GC_KIND_NAME_MAP_TABLE)
#if defined(GC_OBJ_DUMP_MAP)
obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_core__Creator size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Function_O:
{
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    sout << "KIND_LISPALLOC_core__Function_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CandoException_O:
{
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    sout << "KIND_LISPALLOC_core__CandoException_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    sout << "KIND_LISPALLOC_core__Environment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__FunctionFrame_O:
{
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    sout << "KIND_LISPALLOC_core__FunctionFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
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
obj_dump_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    sout << "KIND_BOOTSTRAP_core__Metaobject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    sout << "KIND_LISPALLOC_core__Float_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    sout << "KIND_LISPALLOC_core__LongFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    sout << "KIND_BOOTSTRAP_core__StandardObject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    sout << "KIND_LISPALLOC_core__DoubleFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    sout << "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    sout << "KIND_LISPALLOC_core__Number_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    sout << "KIND_LISPALLOC_core__Vector_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    sout << "KIND_LISPALLOC_core__Integer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Str_O:
{
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    sout << "KIND_BOOTSTRAP_core__Str_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__FunctionValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    sout << "KIND_BOOTSTRAP_core__Specializer_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    sout << "KIND_LISPALLOC_core__SymbolToEnumConverter_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    sout << "KIND_LISPALLOC_core__CxxObject_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    sout << "KIND_LISPALLOC_core__MacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    typedef core::InvocationHistoryFrameIterator_O type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O;
    sout << "KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__VectorObjects_O:
{
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    sout << "KIND_LISPALLOC_core__VectorObjects_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    sout << "KIND_LISPALLOC_core__GlueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    sout << "KIND_LISPALLOC_core__CatchEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    sout << "KIND_LISPALLOC_core__Fixnum_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Cons_O:
{
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    sout << "KIND_LISPALLOC_core__Cons_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__String_O:
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    sout << "KIND_LISPALLOC_core__String_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Record_O:
{
    typedef core::Record_O type_KIND_LISPALLOC_core__Record_O;
    sout << "KIND_LISPALLOC_core__Record_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Record_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    sout << "KIND_LISPALLOC_core__SingleFloat_dummy_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_CLASSALLOC_core__MacroClosure:
{
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    sout << "KIND_CLASSALLOC_core__MacroClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ValueFrame_O:
{
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    sout << "KIND_LISPALLOC_core__ValueFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    sout << "KIND_LISPALLOC_core__UnwindProtectEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    sout << "KIND_LISPALLOC_core__BlockEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__ShortFloat_O:
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    sout << "KIND_LISPALLOC_core__ShortFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    sout << "KIND_LISPALLOC_core__TagbodyFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Array_O:
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    sout << "KIND_LISPALLOC_core__Array_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    sout << "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    sout << "KIND_LISPALLOC_core__SourcePosInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    sout << "KIND_LISPALLOC_core__TagbodyEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CompiledFunction_O:
{
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    sout << "KIND_LISPALLOC_core__CompiledFunction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__Real_O:
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    sout << "KIND_LISPALLOC_core__Real_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    sout << "KIND_LISPALLOC_core__LexicalEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Bignum_O:
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    sout << "KIND_LISPALLOC_core__Bignum_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    sout << "KIND_LISPALLOC_core__SourceFileInfo_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    typedef core::LambdaListHandler_O type_KIND_LISPALLOC_core__LambdaListHandler_O;
    sout << "KIND_LISPALLOC_core__LambdaListHandler_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LambdaListHandler_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Pointer_O:
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    sout << "KIND_LISPALLOC_core__Pointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Rational_O:
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    sout << "KIND_LISPALLOC_core__Rational_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    sout << "KIND_LISPALLOC_core__WeakKeyHashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    sout << "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    typedef core::StackValueEnvironment_O type_KIND_LISPALLOC_core__StackValueEnvironment_O;
    sout << "KIND_LISPALLOC_core__StackValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__StackValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    gctools::GCVector_moveable<core::SymbolClassPair>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::SymbolClassPair>*>(client);
    sout << "gctools::GCVector_moveable<core::SymbolClassPair>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::SymbolClassPair> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__WeakHashTable_O:
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    sout << "KIND_LISPALLOC_core__WeakHashTable_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Symbol_O:
{
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    sout << "KIND_BOOTSTRAP_core__Symbol_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    sout << "KIND_LISPALLOC_core__FunctionContainerEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    sout << "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_BOOTSTRAP_core__Class_O:
{
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    sout << "KIND_BOOTSTRAP_core__Class_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__HashTableEq_O:
{
    typedef core::HashTableEq_O type_KIND_LISPALLOC_core__HashTableEq_O;
    sout << "KIND_LISPALLOC_core__HashTableEq_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEq_O))+global_alignup_sizeof_header) << "]" ;
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
obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    sout << "KIND_LISPALLOC_core__CompileTimeEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__Complex_O:
{
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    sout << "KIND_LISPALLOC_core__Complex_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
obj_dump_KIND_LISPALLOC_core__SourceManager_O:
{
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    sout << "KIND_LISPALLOC_core__SourceManager_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O))+global_alignup_sizeof_header) << "]" ;
}
goto BOTTOM;
#endif // defined(GC_OBJ_DUMP_MAP)
#if defined(GC_OBJ_DUMP_MAP_TABLE)
static void* OBJ_DUMP_MAP_table[] = { NULL 
  /* 1 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , &&obj_dump_KIND_LISPALLOC_core__Function_O
  /* 3 */ , &&obj_dump_KIND_LISPALLOC_core__CandoException_O
  /* 4 */ , &&obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 5 */ , &&obj_dump_KIND_LISPALLOC_core__Ratio_O
  /* 6 */ , &&obj_dump_KIND_LISPALLOC_core__Environment_O
  /* 7 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 8 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 10 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 11 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 12 */ , &&obj_dump_KIND_BOOTSTRAP_core__Metaobject_O
  /* 13 */ , &&obj_dump_KIND_LISPALLOC_core__Float_O
  /* 14 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 15 */ , &&obj_dump_KIND_LISPALLOC_core__LongFloat_O
  /* 16 */ , &&obj_dump_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_dump_KIND_LISPALLOC_core__DoubleFloat_O
  /* 18 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 19 */ , &&obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 20 */ , &&obj_dump_KIND_LISPALLOC_core__Number_O
  /* 21 */ , &&obj_dump_KIND_LISPALLOC_core__Vector_O
  /* 22 */ , &&obj_dump_KIND_LISPALLOC_core__Integer_O
  /* 23 */ , &&obj_dump_KIND_BOOTSTRAP_core__Str_O
  /* 24 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 25 */ , &&obj_dump_KIND_BOOTSTRAP_core__Specializer_O
  /* 26 */ , &&obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 27 */ , &&obj_dump_KIND_LISPALLOC_core__CxxObject_O
  /* 28 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 29 */ , &&obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 30 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 31 */ , &&obj_dump_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 32 */ , &&obj_dump_KIND_LISPALLOC_core__VectorObjects_O
  /* 33 */ , &&obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 34 */ , &&obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 35 */ , &&obj_dump_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 36 */ , &&obj_dump_KIND_LISPALLOC_core__Cons_O
  /* 37 */ , &&obj_dump_KIND_LISPALLOC_core__String_O
  /* 38 */ , &&obj_dump_KIND_LISPALLOC_core__Record_O
  /* 39 */ , &&obj_dump_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 40 */ , &&obj_dump_KIND_CLASSALLOC_core__MacroClosure
  /* 41 */ , &&obj_dump_KIND_LISPALLOC_core__ValueFrame_O
  /* 42 */ , &&obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 43 */ , &&obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 44 */ , &&obj_dump_KIND_LISPALLOC_core__ShortFloat_O
  /* 45 */ , &&obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 46 */ , &&obj_dump_KIND_LISPALLOC_core__Array_O
  /* 47 */ , &&obj_dump_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 48 */ , &&obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 49 */ , &&obj_dump_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 50 */ , &&obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 51 */ , &&obj_dump_KIND_LISPALLOC_core__CompiledFunction_O
  /* 52 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 53 */ , &&obj_dump_KIND_LISPALLOC_core__Real_O
  /* 54 */ , &&obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 55 */ , &&obj_dump_KIND_LISPALLOC_core__Bignum_O
  /* 56 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 57 */ , &&obj_dump_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 58 */ , &&obj_dump_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 59 */ , &&obj_dump_KIND_LISPALLOC_core__Pointer_O
  /* 60 */ , &&obj_dump_KIND_LISPALLOC_core__Rational_O
  /* 61 */ , &&obj_dump_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 62 */ , &&obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 63 */ , &&obj_dump_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 64 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 65 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 66 */ , &&obj_dump_KIND_LISPALLOC_core__WeakHashTable_O
  /* 67 */ , &&obj_dump_KIND_BOOTSTRAP_core__Symbol_O
  /* 68 */ , &&obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 69 */ , &&obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 70 */ , &&obj_dump_KIND_BOOTSTRAP_core__Class_O
  /* 71 */ , &&obj_dump_KIND_LISPALLOC_core__HashTableEq_O
  /* 72 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 73 */ , &&obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 74 */ , &&obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 75 */ , &&obj_dump_KIND_LISPALLOC_core__Complex_O
  /* 76 */ , &&obj_dump_KIND_LISPALLOC_core__SourceManager_O
};
#endif // defined(GC_OBJ_DUMP_MAP_TABLE)
#if defined(GC_OBJ_SKIP)
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Function_O:
{
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CandoException_O:
{
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__FunctionFrame_O:
{
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Str_O:
{
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O:
{
    typedef core::InvocationHistoryFrameIterator_O type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__VectorObjects_O:
{
    typedef core::VectorObjects_O type_KIND_LISPALLOC_core__VectorObjects_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__VectorObjects_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    typedef core::Fixnum_dummy_O type_KIND_LISPALLOC_core__Fixnum_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Cons_O:
{
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__String_O:
{
    typedef core::String_O type_KIND_LISPALLOC_core__String_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__String_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Record_O:
{
    typedef core::Record_O type_KIND_LISPALLOC_core__Record_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Record_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    typedef core::SingleFloat_dummy_O type_KIND_LISPALLOC_core__SingleFloat_dummy_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_dummy_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_CLASSALLOC_core__MacroClosure:
{
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ValueFrame_O:
{
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__ShortFloat_O:
{
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Array_O:
{
    typedef core::Array_O type_KIND_LISPALLOC_core__Array_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Array_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    typedef core::SourcePosInfo_O type_KIND_LISPALLOC_core__SourcePosInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourcePosInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__CompiledFunction_O:
{
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__Real_O:
{
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Bignum_O:
{
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    typedef core::SourceFileInfo_O type_KIND_LISPALLOC_core__SourceFileInfo_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceFileInfo_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__LambdaListHandler_O:
{
    typedef core::LambdaListHandler_O type_KIND_LISPALLOC_core__LambdaListHandler_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LambdaListHandler_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Pointer_O:
{
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Rational_O:
{
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    typedef core::WeakKeyHashTable_O type_KIND_LISPALLOC_core__WeakKeyHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakKeyHashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    typedef core::StackValueEnvironment_O type_KIND_LISPALLOC_core__StackValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__StackValueEnvironment_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__WeakHashTable_O:
{
    typedef core::WeakHashTable_O type_KIND_LISPALLOC_core__WeakHashTable_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakHashTable_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Symbol_O:
{
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_BOOTSTRAP_core__Class_O:
{
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__HashTableEq_O:
{
    typedef core::HashTableEq_O type_KIND_LISPALLOC_core__HashTableEq_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__HashTableEq_O)) + global_alignup_sizeof_header;
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
obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__Complex_O:
{
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
obj_skip_KIND_LISPALLOC_core__SourceManager_O:
{
    typedef core::SourceManager_O type_KIND_LISPALLOC_core__SourceManager_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SourceManager_O)) + global_alignup_sizeof_header;
    goto DONE; //return client;
}
#endif // defined(GC_OBJ_SKIP)
#if defined(GC_OBJ_SKIP_TABLE)
static void* OBJ_SKIP_table[] = { NULL 
  /* 1 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , &&obj_skip_KIND_LISPALLOC_core__Function_O
  /* 3 */ , &&obj_skip_KIND_LISPALLOC_core__CandoException_O
  /* 4 */ , &&obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 5 */ , &&obj_skip_KIND_LISPALLOC_core__Ratio_O
  /* 6 */ , &&obj_skip_KIND_LISPALLOC_core__Environment_O
  /* 7 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__
  /* 8 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__
  /* 10 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__
  /* 11 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_
  /* 12 */ , &&obj_skip_KIND_BOOTSTRAP_core__Metaobject_O
  /* 13 */ , &&obj_skip_KIND_LISPALLOC_core__Float_O
  /* 14 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 15 */ , &&obj_skip_KIND_LISPALLOC_core__LongFloat_O
  /* 16 */ , &&obj_skip_KIND_BOOTSTRAP_core__StandardObject_O
  /* 17 */ , &&obj_skip_KIND_LISPALLOC_core__DoubleFloat_O
  /* 18 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_
  /* 19 */ , &&obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 20 */ , &&obj_skip_KIND_LISPALLOC_core__Number_O
  /* 21 */ , &&obj_skip_KIND_LISPALLOC_core__Vector_O
  /* 22 */ , &&obj_skip_KIND_LISPALLOC_core__Integer_O
  /* 23 */ , &&obj_skip_KIND_BOOTSTRAP_core__Str_O
  /* 24 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 25 */ , &&obj_skip_KIND_BOOTSTRAP_core__Specializer_O
  /* 26 */ , &&obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O
  /* 27 */ , &&obj_skip_KIND_LISPALLOC_core__CxxObject_O
  /* 28 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__
  /* 29 */ , &&obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 30 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_
  /* 31 */ , &&obj_skip_KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O
  /* 32 */ , &&obj_skip_KIND_LISPALLOC_core__VectorObjects_O
  /* 33 */ , &&obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 34 */ , &&obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 35 */ , &&obj_skip_KIND_LISPALLOC_core__Fixnum_dummy_O
  /* 36 */ , &&obj_skip_KIND_LISPALLOC_core__Cons_O
  /* 37 */ , &&obj_skip_KIND_LISPALLOC_core__String_O
  /* 38 */ , &&obj_skip_KIND_LISPALLOC_core__Record_O
  /* 39 */ , &&obj_skip_KIND_LISPALLOC_core__SingleFloat_dummy_O
  /* 40 */ , &&obj_skip_KIND_CLASSALLOC_core__MacroClosure
  /* 41 */ , &&obj_skip_KIND_LISPALLOC_core__ValueFrame_O
  /* 42 */ , &&obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 43 */ , &&obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 44 */ , &&obj_skip_KIND_LISPALLOC_core__ShortFloat_O
  /* 45 */ , &&obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 46 */ , &&obj_skip_KIND_LISPALLOC_core__Array_O
  /* 47 */ , &&obj_skip_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 48 */ , &&obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 49 */ , &&obj_skip_KIND_LISPALLOC_core__SourcePosInfo_O
  /* 50 */ , &&obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 51 */ , &&obj_skip_KIND_LISPALLOC_core__CompiledFunction_O
  /* 52 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_
  /* 53 */ , &&obj_skip_KIND_LISPALLOC_core__Real_O
  /* 54 */ , &&obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 55 */ , &&obj_skip_KIND_LISPALLOC_core__Bignum_O
  /* 56 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__
  /* 57 */ , &&obj_skip_KIND_LISPALLOC_core__SourceFileInfo_O
  /* 58 */ , &&obj_skip_KIND_LISPALLOC_core__LambdaListHandler_O
  /* 59 */ , &&obj_skip_KIND_LISPALLOC_core__Pointer_O
  /* 60 */ , &&obj_skip_KIND_LISPALLOC_core__Rational_O
  /* 61 */ , &&obj_skip_KIND_LISPALLOC_core__WeakKeyHashTable_O
  /* 62 */ , &&obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 63 */ , &&obj_skip_KIND_LISPALLOC_core__StackValueEnvironment_O
  /* 64 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 65 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_
  /* 66 */ , &&obj_skip_KIND_LISPALLOC_core__WeakHashTable_O
  /* 67 */ , &&obj_skip_KIND_BOOTSTRAP_core__Symbol_O
  /* 68 */ , &&obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 69 */ , &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 70 */ , &&obj_skip_KIND_BOOTSTRAP_core__Class_O
  /* 71 */ , &&obj_skip_KIND_LISPALLOC_core__HashTableEq_O
  /* 72 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 73 */ , &&obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 74 */ , &&obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 75 */ , &&obj_skip_KIND_LISPALLOC_core__Complex_O
  /* 76 */ , &&obj_skip_KIND_LISPALLOC_core__SourceManager_O
};
#endif // defined(GC_OBJ_SKIP_TABLE)
#if defined(GC_OBJ_SCAN)
obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator:
{
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->closure);
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_message._Contents);
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_SymbolIndex);
    SMART_PTR_FIX(obj_gc_safe->_ActivationFrame);
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_numerator);
    SMART_PTR_FIX(obj_gc_safe->_denominator);
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Environment_O:
{
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
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
obj_scan_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    TAGGED_POINTER_FIX(obj_gc_safe->_Objects._Array._Contents);
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
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
obj_scan_KIND_BOOTSTRAP_core__Metaobject_O:
{
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Float_O:
{
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
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
obj_scan_KIND_LISPALLOC_core__LongFloat_O:
{
    typedef core::LongFloat_O type_KIND_LISPALLOC_core__LongFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LongFloat_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__StandardObject_O:
{
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__DoubleFloat_O:
{
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
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
obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Number_O:
{
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Vector_O:
{
    typedef core::Vector_O type_KIND_LISPALLOC_core__Vector_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Vector_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__Integer_O:
{
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_Contents._Contents);
    typedef core::Str_O type_KIND_BOOTSTRAP_core__Str_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Str_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_FunctionIndices);
    SMART_PTR_FIX(obj_gc_safe->_FunctionFrame);
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_BOOTSTRAP_core__Specializer_O:
{
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    TAGGED_POINTER_FIX(obj_gc_safe->_WhatTheEnumsRepresent._Contents);
    SMART_PTR_FIX(obj_gc_safe->_EnumToSymbol);
    SMART_PTR_FIX(obj_gc_safe->_ArchiveSymbolToEnum);
    SMART_PTR_FIX(obj_gc_safe->_EnumToArchiveSymbol);
    SMART_PTR_FIX(obj_gc_safe->_SymbolToEnum);
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_LISPALLOC_core__CxxObject_O:
{
    typedef core::CxxObject_O type_KIND_LISPALLOC_core__CxxObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CxxObject_O)) + global_alignup_sizeof_header;
}
goto TOP;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__tagged_pointer_core__SequenceStepper__:
{
    gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>*>(client);
    for (gctools::GCVector_moveable<gctools::tagged_pointer<core::SequenceStepper>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
