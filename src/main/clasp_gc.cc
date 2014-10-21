#ifdef DECLARE_FORWARDS
 namespace core {
    class Complex_O;
    class SymbolToEnumConverter_O;
    class Class_O;
    class FunctionContainerEnvironment_O;
    class ShortFloat_O;
    class UnwindProtectEnvironment_O;
    class BlockEnvironment_O;
    class ValueFrame_O;
    class MacroClosure;
    class Integer_O;
    class Number_O;
    class ExceptionEntry;
    class Cons_O;
    class DoubleFloat_O;
    class CatchEnvironment_O;
    class Pointer_O;
    class Float_O;
    class Metaobject_O;
    class Environment_O;
    class ValueEnvironment_O;
    class TagbodyEnvironment_O;
    class Ratio_O;
    class CandoException_O;
    class CompileTimeEnvironment_O;
    class WeakPointer_O;
    class TagbodyFrame_O;
    class SingleFloat_O;
    class FunctionValueEnvironment_O;
    class Symbol_O;
    class Specializer_O;
    class RuntimeVisibleEnvironment_O;
    class SymbolMacroletEnvironment_O;
    class StandardObject_O;
    class Rational_O;
    class GlueEnvironment_O;
    class Bignum_O;
    class MacroletEnvironment_O;
    class LexicalEnvironment_O;
    class DynamicBinding;
    class FunctionFrame_O;
    class Real_O;
    class CompiledFunction_O;
    class CacheRecord;
    class Fixnum_O;
    class Function_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_TEMPLATED_CLASSALLOC_core__Creator = 1,
KIND_LISPALLOC_core__Function_O = 2,
KIND_LISPALLOC_core__Fixnum_O = 3,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 4,
KIND_LISPALLOC_core__CompiledFunction_O = 5,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ = 6,
KIND_LISPALLOC_core__Real_O = 7,
KIND_LISPALLOC_core__FunctionFrame_O = 8,
KIND_LISPALLOC_core__LexicalEnvironment_O = 9,
KIND_LISPALLOC_core__MacroletEnvironment_O = 10,
KIND_LISPALLOC_core__Bignum_O = 11,
KIND_LISPALLOC_core__GlueEnvironment_O = 12,
KIND_LISPALLOC_core__Rational_O = 13,
KIND_BOOTSTRAP_core__StandardObject_O = 14,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 15,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 16,
KIND_BOOTSTRAP_core__Specializer_O = 17,
KIND_BOOTSTRAP_core__Symbol_O = 18,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 19,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 20,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 21,
KIND_LISPALLOC_core__SingleFloat_O = 22,
KIND_LISPALLOC_core__TagbodyFrame_O = 23,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 24,
KIND_LISPALLOC_core__WeakPointer_O = 25,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 26,
KIND_LISPALLOC_core__CandoException_O = 27,
KIND_LISPALLOC_core__Ratio_O = 28,
KIND_LISPALLOC_core__ValueEnvironment_O = 29,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 30,
KIND_LISPALLOC_core__Environment_O = 31,
KIND_BOOTSTRAP_core__Metaobject_O = 32,
KIND_LISPALLOC_core__Float_O = 33,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 34,
KIND_LISPALLOC_core__CatchEnvironment_O = 35,
KIND_LISPALLOC_core__Pointer_O = 36,
KIND_LISPALLOC_core__DoubleFloat_O = 37,
KIND_LISPALLOC_core__Cons_O = 38,
KIND_LISPALLOC_core__Number_O = 39,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 40,
KIND_CLASSALLOC_core__MacroClosure = 41,
KIND_LISPALLOC_core__ValueFrame_O = 42,
KIND_LISPALLOC_core__Integer_O = 43,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 44,
KIND_LISPALLOC_core__BlockEnvironment_O = 45,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 46,
KIND_LISPALLOC_core__ShortFloat_O = 47,
KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure = 48,
KIND_BOOTSTRAP_core__Class_O = 49,
KIND_LISPALLOC_core__Complex_O = 50,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 51,
  KIND_max = 51
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct DynamicCast<core::Creator*,FP> {
  static bool isA(FP client) {
    gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
    int kindVal = header->kind();
    // low high --> 1 1 
    return (kindVal == 1);
  };
  static core::Creator* castOrNULL(FP client) {
    if (DynamicCast<core::Creator*,FP>::isA(client)) {
      return reinterpret_cast<core::Creator*>(client);
    }
    return NULL;
  };
};
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
template <> class gctools::GCKind<core::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Function_O ;
};
template <> class gctools::GCKind<core::Fixnum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<core::Creator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__Creator ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<core::SymbolMacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolMacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<core::Specializer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Specializer_O ;
};
template <> class gctools::GCKind<core::Symbol_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Symbol_O ;
};
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<core::SingleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_O ;
};
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
template <> class gctools::GCKind<core::CompileTimeEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompileTimeEnvironment_O ;
};
template <> class gctools::GCKind<core::WeakPointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakPointer_O ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ ;
};
template <> class gctools::GCKind<core::CandoException_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CandoException_O ;
};
template <> class gctools::GCKind<core::Ratio_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Ratio_O ;
};
template <> class gctools::GCKind<core::ValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueEnvironment_O ;
};
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
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
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
};
template <> class gctools::GCKind<core::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pointer_O ;
};
template <> class gctools::GCKind<core::DoubleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DoubleFloat_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::Number_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Number_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::ExceptionEntry>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ ;
};
template <> class gctools::GCKind<core::MacroClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__MacroClosure ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<core::ShortFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ShortFloat_O ;
};
template <> class gctools::GCKind<core::BuiltinClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_KIND_NAME_MAP)
const char* kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator(){
return "KIND_TEMPLATED_CLASSALLOC_core__Creator";
}
const char* kind_name_KIND_LISPALLOC_core__Function_O(){
return "KIND_LISPALLOC_core__Function_O";
}
const char* kind_name_KIND_LISPALLOC_core__Fixnum_O(){
return "KIND_LISPALLOC_core__Fixnum_O";
}
const char* kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_(){
return "KIND_GCSTRING_gctools__GCString_moveable_char_";
}
const char* kind_name_KIND_LISPALLOC_core__CompiledFunction_O(){
return "KIND_LISPALLOC_core__CompiledFunction_O";
}
const char* kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__(){
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__";
}
const char* kind_name_KIND_LISPALLOC_core__Real_O(){
return "KIND_LISPALLOC_core__Real_O";
}
const char* kind_name_KIND_LISPALLOC_core__FunctionFrame_O(){
return "KIND_LISPALLOC_core__FunctionFrame_O";
}
const char* kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O(){
return "KIND_LISPALLOC_core__LexicalEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O(){
return "KIND_LISPALLOC_core__MacroletEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__Bignum_O(){
return "KIND_LISPALLOC_core__Bignum_O";
}
const char* kind_name_KIND_LISPALLOC_core__GlueEnvironment_O(){
return "KIND_LISPALLOC_core__GlueEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__Rational_O(){
return "KIND_LISPALLOC_core__Rational_O";
}
const char* kind_name_KIND_BOOTSTRAP_core__StandardObject_O(){
return "KIND_BOOTSTRAP_core__StandardObject_O";
}
const char* kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O(){
return "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O(){
return "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O";
}
const char* kind_name_KIND_BOOTSTRAP_core__Specializer_O(){
return "KIND_BOOTSTRAP_core__Specializer_O";
}
const char* kind_name_KIND_BOOTSTRAP_core__Symbol_O(){
return "KIND_BOOTSTRAP_core__Symbol_O";
}
const char* kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O(){
return "KIND_LISPALLOC_core__FunctionValueEnvironment_O";
}
const char* kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_(){
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_";
}
const char* kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__(){
return "KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__";
}
const char* kind_name_KIND_LISPALLOC_core__SingleFloat_O(){
return "KIND_LISPALLOC_core__SingleFloat_O";
}
const char* kind_name_KIND_LISPALLOC_core__TagbodyFrame_O(){
return "KIND_LISPALLOC_core__TagbodyFrame_O";
}
const char* kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O(){
return "KIND_LISPALLOC_core__CompileTimeEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__WeakPointer_O(){
return "KIND_LISPALLOC_core__WeakPointer_O";
}
const char* kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_(){
return "KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_";
}
const char* kind_name_KIND_LISPALLOC_core__CandoException_O(){
return "KIND_LISPALLOC_core__CandoException_O";
}
const char* kind_name_KIND_LISPALLOC_core__Ratio_O(){
return "KIND_LISPALLOC_core__Ratio_O";
}
const char* kind_name_KIND_LISPALLOC_core__ValueEnvironment_O(){
return "KIND_LISPALLOC_core__ValueEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O(){
return "KIND_LISPALLOC_core__TagbodyEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__Environment_O(){
return "KIND_LISPALLOC_core__Environment_O";
}
const char* kind_name_KIND_BOOTSTRAP_core__Metaobject_O(){
return "KIND_BOOTSTRAP_core__Metaobject_O";
}
const char* kind_name_KIND_LISPALLOC_core__Float_O(){
return "KIND_LISPALLOC_core__Float_O";
}
const char* kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_(){
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_";
}
const char* kind_name_KIND_LISPALLOC_core__CatchEnvironment_O(){
return "KIND_LISPALLOC_core__CatchEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__Pointer_O(){
return "KIND_LISPALLOC_core__Pointer_O";
}
const char* kind_name_KIND_LISPALLOC_core__DoubleFloat_O(){
return "KIND_LISPALLOC_core__DoubleFloat_O";
}
const char* kind_name_KIND_LISPALLOC_core__Cons_O(){
return "KIND_LISPALLOC_core__Cons_O";
}
const char* kind_name_KIND_LISPALLOC_core__Number_O(){
return "KIND_LISPALLOC_core__Number_O";
}
const char* kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_(){
return "KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_";
}
const char* kind_name_KIND_CLASSALLOC_core__MacroClosure(){
return "KIND_CLASSALLOC_core__MacroClosure";
}
const char* kind_name_KIND_LISPALLOC_core__ValueFrame_O(){
return "KIND_LISPALLOC_core__ValueFrame_O";
}
const char* kind_name_KIND_LISPALLOC_core__Integer_O(){
return "KIND_LISPALLOC_core__Integer_O";
}
const char* kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O(){
return "KIND_LISPALLOC_core__UnwindProtectEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__BlockEnvironment_O(){
return "KIND_LISPALLOC_core__BlockEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O(){
return "KIND_LISPALLOC_core__FunctionContainerEnvironment_O";
}
const char* kind_name_KIND_LISPALLOC_core__ShortFloat_O(){
return "KIND_LISPALLOC_core__ShortFloat_O";
}
const char* kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure(){
return "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure";
}
const char* kind_name_KIND_BOOTSTRAP_core__Class_O(){
return "KIND_BOOTSTRAP_core__Class_O";
}
const char* kind_name_KIND_LISPALLOC_core__Complex_O(){
return "KIND_LISPALLOC_core__Complex_O";
}
const char* kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O(){
return "KIND_LISPALLOC_core__SymbolToEnumConverter_O";
}
#endif // defined(GC_KIND_NAME_MAP)
#if defined(GC_KIND_NAME_MAP_TABLE)
const char* (*)() KIND_NAME_MAP_table[] = { NULL 
  /* 1 */ , kind_name_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , kind_name_KIND_LISPALLOC_core__Function_O
  /* 3 */ , kind_name_KIND_LISPALLOC_core__Fixnum_O
  /* 4 */ , kind_name_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 5 */ , kind_name_KIND_LISPALLOC_core__CompiledFunction_O
  /* 6 */ , kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 7 */ , kind_name_KIND_LISPALLOC_core__Real_O
  /* 8 */ , kind_name_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , kind_name_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 10 */ , kind_name_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 11 */ , kind_name_KIND_LISPALLOC_core__Bignum_O
  /* 12 */ , kind_name_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 13 */ , kind_name_KIND_LISPALLOC_core__Rational_O
  /* 14 */ , kind_name_KIND_BOOTSTRAP_core__StandardObject_O
  /* 15 */ , kind_name_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 16 */ , kind_name_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 17 */ , kind_name_KIND_BOOTSTRAP_core__Specializer_O
  /* 18 */ , kind_name_KIND_BOOTSTRAP_core__Symbol_O
  /* 19 */ , kind_name_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 20 */ , kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 21 */ , kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 22 */ , kind_name_KIND_LISPALLOC_core__SingleFloat_O
  /* 23 */ , kind_name_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 24 */ , kind_name_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 25 */ , kind_name_KIND_LISPALLOC_core__WeakPointer_O
  /* 26 */ , kind_name_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 27 */ , kind_name_KIND_LISPALLOC_core__CandoException_O
  /* 28 */ , kind_name_KIND_LISPALLOC_core__Ratio_O
  /* 29 */ , kind_name_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 30 */ , kind_name_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 31 */ , kind_name_KIND_LISPALLOC_core__Environment_O
  /* 32 */ , kind_name_KIND_BOOTSTRAP_core__Metaobject_O
  /* 33 */ , kind_name_KIND_LISPALLOC_core__Float_O
  /* 34 */ , kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 35 */ , kind_name_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 36 */ , kind_name_KIND_LISPALLOC_core__Pointer_O
  /* 37 */ , kind_name_KIND_LISPALLOC_core__DoubleFloat_O
  /* 38 */ , kind_name_KIND_LISPALLOC_core__Cons_O
  /* 39 */ , kind_name_KIND_LISPALLOC_core__Number_O
  /* 40 */ , kind_name_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 41 */ , kind_name_KIND_CLASSALLOC_core__MacroClosure
  /* 42 */ , kind_name_KIND_LISPALLOC_core__ValueFrame_O
  /* 43 */ , kind_name_KIND_LISPALLOC_core__Integer_O
  /* 44 */ , kind_name_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 45 */ , kind_name_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 46 */ , kind_name_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 47 */ , kind_name_KIND_LISPALLOC_core__ShortFloat_O
  /* 48 */ , kind_name_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 49 */ , kind_name_KIND_BOOTSTRAP_core__Class_O
  /* 50 */ , kind_name_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , kind_name_KIND_LISPALLOC_core__SymbolToEnumConverter_O
#endif // defined(GC_KIND_NAME_MAP_TABLE)
#if defined(GC_OBJ_DUMP_MAP)
string obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator(){
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    stringstream sout;
    sout << "KIND_TEMPLATED_CLASSALLOC_core__Creator size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Function_O(){
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Function_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Fixnum_O(){
    typedef core::Fixnum_O type_KIND_LISPALLOC_core__Fixnum_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Fixnum_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_GCSTRING_gctools__GCString_moveable_char_(){
    gctools::GCString_moveable<char>* obj_gc_safe = reinterpret_cast<gctools::GCString_moveable<char>*>(client);
    typedef typename gctools::GCString_moveable<char> type_KIND_GCSTRING_gctools__GCString_moveable_char_;
    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_KIND_GCSTRING_gctools__GCString_moveable_char_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    stringstream sout;
    sout << "KIND_GCSTRING_gctools__GCString_moveable_char_" << "bytes[" << header_and_gcstring_size << "]";
    return sout.str()
}
string obj_dump_KIND_LISPALLOC_core__CompiledFunction_O(){
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__CompiledFunction_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__(){
    gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>*>(client);
    stringstream sout;
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
     return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Real_O(){
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Real_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__FunctionFrame_O(){
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__FunctionFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O(){
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__LexicalEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O(){
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__MacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Bignum_O(){
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Bignum_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O(){
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__GlueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Rational_O(){
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Rational_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_BOOTSTRAP_core__StandardObject_O(){
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    stringstream sout;
    sout << "KIND_BOOTSTRAP_core__StandardObject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O(){
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__SymbolMacroletEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O(){
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_BOOTSTRAP_core__Specializer_O(){
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    stringstream sout;
    sout << "KIND_BOOTSTRAP_core__Specializer_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_BOOTSTRAP_core__Symbol_O(){
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    stringstream sout;
    sout << "KIND_BOOTSTRAP_core__Symbol_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O(){
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__FunctionValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_(){
    gctools::GCVector_moveable<core::CacheRecord>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::CacheRecord>*>(client);
    stringstream sout;
    sout << "gctools::GCVector_moveable<core::CacheRecord>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::CacheRecord> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
     return sout.str();
}
string obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__(){
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    stringstream sout;
    sout << "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
     return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__SingleFloat_O(){
    typedef core::SingleFloat_O type_KIND_LISPALLOC_core__SingleFloat_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__SingleFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O(){
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__TagbodyFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O(){
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__CompileTimeEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__WeakPointer_O(){
    typedef core::WeakPointer_O type_KIND_LISPALLOC_core__WeakPointer_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__WeakPointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakPointer_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_(){
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    stringstream sout;
    sout << "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
     return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__CandoException_O(){
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__CandoException_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Ratio_O(){
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Ratio_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O(){
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__ValueEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O(){
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__TagbodyEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Environment_O(){
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Environment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_BOOTSTRAP_core__Metaobject_O(){
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    stringstream sout;
    sout << "KIND_BOOTSTRAP_core__Metaobject_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Float_O(){
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Float_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_(){
    gctools::GCVector_moveable<core::DynamicBinding>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::DynamicBinding>*>(client);
    stringstream sout;
    sout << "gctools::GCVector_moveable<core::DynamicBinding>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::DynamicBinding> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
     return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O(){
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__CatchEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Pointer_O(){
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Pointer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__DoubleFloat_O(){
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__DoubleFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Cons_O(){
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Cons_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Number_O(){
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Number_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_(){
    gctools::GCVector_moveable<core::ExceptionEntry>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::ExceptionEntry>*>(client);
    stringstream sout;
    sout << "gctools::GCVector_moveable<core::ExceptionEntry>" << " size/capacity[" << obj_gc_safe->size() << "/" << obj_gc_safe->capacity();
    typedef typename gctools::GCVector_moveable<core::ExceptionEntry> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    sout << "bytes[" << header_and_gccontainer_size << "]";
     return sout.str();
}
string obj_dump_KIND_CLASSALLOC_core__MacroClosure(){
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    stringstream sout;
    sout << "KIND_CLASSALLOC_core__MacroClosure size[" << (AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__ValueFrame_O(){
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__ValueFrame_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Integer_O(){
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Integer_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O(){
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__UnwindProtectEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O(){
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__BlockEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O(){
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__FunctionContainerEnvironment_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__ShortFloat_O(){
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__ShortFloat_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure(){
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    stringstream sout;
    sout << "KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure size[" << (AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_BOOTSTRAP_core__Class_O(){
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    stringstream sout;
    sout << "KIND_BOOTSTRAP_core__Class_O size[" << (AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__Complex_O(){
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__Complex_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
string obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O(){
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    stringstream sout;
    sout << "KIND_LISPALLOC_core__SymbolToEnumConverter_O size[" << (AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O))+global_alignup_sizeof_header) << "]" ;
    return sout.str();
}
#endif // defined(GC_OBJ_DUMP_MAP)
#if defined(GC_OBJ_DUMP_MAP_TABLE)
string (*)() OBJ_DUMP_MAP_table[] = { NULL 
  /* 1 */ , obj_dump_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , obj_dump_KIND_LISPALLOC_core__Function_O
  /* 3 */ , obj_dump_KIND_LISPALLOC_core__Fixnum_O
  /* 4 */ , obj_dump_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 5 */ , obj_dump_KIND_LISPALLOC_core__CompiledFunction_O
  /* 6 */ , obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 7 */ , obj_dump_KIND_LISPALLOC_core__Real_O
  /* 8 */ , obj_dump_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , obj_dump_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 10 */ , obj_dump_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 11 */ , obj_dump_KIND_LISPALLOC_core__Bignum_O
  /* 12 */ , obj_dump_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 13 */ , obj_dump_KIND_LISPALLOC_core__Rational_O
  /* 14 */ , obj_dump_KIND_BOOTSTRAP_core__StandardObject_O
  /* 15 */ , obj_dump_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 16 */ , obj_dump_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 17 */ , obj_dump_KIND_BOOTSTRAP_core__Specializer_O
  /* 18 */ , obj_dump_KIND_BOOTSTRAP_core__Symbol_O
  /* 19 */ , obj_dump_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 20 */ , obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 21 */ , obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 22 */ , obj_dump_KIND_LISPALLOC_core__SingleFloat_O
  /* 23 */ , obj_dump_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 24 */ , obj_dump_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 25 */ , obj_dump_KIND_LISPALLOC_core__WeakPointer_O
  /* 26 */ , obj_dump_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 27 */ , obj_dump_KIND_LISPALLOC_core__CandoException_O
  /* 28 */ , obj_dump_KIND_LISPALLOC_core__Ratio_O
  /* 29 */ , obj_dump_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 30 */ , obj_dump_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 31 */ , obj_dump_KIND_LISPALLOC_core__Environment_O
  /* 32 */ , obj_dump_KIND_BOOTSTRAP_core__Metaobject_O
  /* 33 */ , obj_dump_KIND_LISPALLOC_core__Float_O
  /* 34 */ , obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 35 */ , obj_dump_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 36 */ , obj_dump_KIND_LISPALLOC_core__Pointer_O
  /* 37 */ , obj_dump_KIND_LISPALLOC_core__DoubleFloat_O
  /* 38 */ , obj_dump_KIND_LISPALLOC_core__Cons_O
  /* 39 */ , obj_dump_KIND_LISPALLOC_core__Number_O
  /* 40 */ , obj_dump_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 41 */ , obj_dump_KIND_CLASSALLOC_core__MacroClosure
  /* 42 */ , obj_dump_KIND_LISPALLOC_core__ValueFrame_O
  /* 43 */ , obj_dump_KIND_LISPALLOC_core__Integer_O
  /* 44 */ , obj_dump_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 45 */ , obj_dump_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 46 */ , obj_dump_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 47 */ , obj_dump_KIND_LISPALLOC_core__ShortFloat_O
  /* 48 */ , obj_dump_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 49 */ , obj_dump_KIND_BOOTSTRAP_core__Class_O
  /* 50 */ , obj_dump_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , obj_dump_KIND_LISPALLOC_core__SymbolToEnumConverter_O
#endif // defined(GC_OBJ_DUMP_MAP_TABLE)
#if defined(GC_OBJ_SKIP)
mps_addr_t obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator(mps_addr_t client){
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Function_O(mps_addr_t client){
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Fixnum_O(mps_addr_t client){
    typedef core::Fixnum_O type_KIND_LISPALLOC_core__Fixnum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_GCSTRING_gctools__GCString_moveable_char_(mps_addr_t client){
    gctools::GCString_moveable<char>* obj_gc_safe = reinterpret_cast<gctools::GCString_moveable<char>*>(client);
    typedef typename gctools::GCString_moveable<char> type_KIND_GCSTRING_gctools__GCString_moveable_char_;
    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_KIND_GCSTRING_gctools__GCString_moveable_char_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + Align(header_and_gcstring_size);
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__CompiledFunction_O(mps_addr_t client){
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__(mps_addr_t client){
    gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Real_O(mps_addr_t client){
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__FunctionFrame_O(mps_addr_t client){
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O(mps_addr_t client){
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O(mps_addr_t client){
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Bignum_O(mps_addr_t client){
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O(mps_addr_t client){
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Rational_O(mps_addr_t client){
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_BOOTSTRAP_core__StandardObject_O(mps_addr_t client){
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O(mps_addr_t client){
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O(mps_addr_t client){
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_BOOTSTRAP_core__Specializer_O(mps_addr_t client){
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_BOOTSTRAP_core__Symbol_O(mps_addr_t client){
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O(mps_addr_t client){
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_(mps_addr_t client){
    gctools::GCVector_moveable<core::CacheRecord>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::CacheRecord>*>(client);
    typedef typename gctools::GCVector_moveable<core::CacheRecord> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
mps_addr_t obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__(mps_addr_t client){
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__SingleFloat_O(mps_addr_t client){
    typedef core::SingleFloat_O type_KIND_LISPALLOC_core__SingleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O(mps_addr_t client){
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O(mps_addr_t client){
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__WeakPointer_O(mps_addr_t client){
    typedef core::WeakPointer_O type_KIND_LISPALLOC_core__WeakPointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakPointer_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_(mps_addr_t client){
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__CandoException_O(mps_addr_t client){
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Ratio_O(mps_addr_t client){
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O(mps_addr_t client){
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O(mps_addr_t client){
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Environment_O(mps_addr_t client){
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_BOOTSTRAP_core__Metaobject_O(mps_addr_t client){
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Float_O(mps_addr_t client){
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_(mps_addr_t client){
    gctools::GCVector_moveable<core::DynamicBinding>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::DynamicBinding>*>(client);
    typedef typename gctools::GCVector_moveable<core::DynamicBinding> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O(mps_addr_t client){
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Pointer_O(mps_addr_t client){
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__DoubleFloat_O(mps_addr_t client){
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Cons_O(mps_addr_t client){
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Number_O(mps_addr_t client){
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_(mps_addr_t client){
    gctools::GCVector_moveable<core::ExceptionEntry>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::ExceptionEntry>*>(client);
    typedef typename gctools::GCVector_moveable<core::ExceptionEntry> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
}
mps_addr_t obj_skip_KIND_CLASSALLOC_core__MacroClosure(mps_addr_t client){
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__ValueFrame_O(mps_addr_t client){
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Integer_O(mps_addr_t client){
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O(mps_addr_t client){
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O(mps_addr_t client){
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O(mps_addr_t client){
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__ShortFloat_O(mps_addr_t client){
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure(mps_addr_t client){
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_BOOTSTRAP_core__Class_O(mps_addr_t client){
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__Complex_O(mps_addr_t client){
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O)) + global_alignup_sizeof_header;
}
mps_addr_t obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O(mps_addr_t client){
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
}
#endif // defined(GC_OBJ_SKIP)
#if defined(GC_OBJ_SKIP_TABLE)
mps_addr_t (*)(mps_addr_t) OBJ_SKIP_table[] = { NULL 
  /* 1 */ , obj_skip_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , obj_skip_KIND_LISPALLOC_core__Function_O
  /* 3 */ , obj_skip_KIND_LISPALLOC_core__Fixnum_O
  /* 4 */ , obj_skip_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 5 */ , obj_skip_KIND_LISPALLOC_core__CompiledFunction_O
  /* 6 */ , obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 7 */ , obj_skip_KIND_LISPALLOC_core__Real_O
  /* 8 */ , obj_skip_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , obj_skip_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 10 */ , obj_skip_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 11 */ , obj_skip_KIND_LISPALLOC_core__Bignum_O
  /* 12 */ , obj_skip_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 13 */ , obj_skip_KIND_LISPALLOC_core__Rational_O
  /* 14 */ , obj_skip_KIND_BOOTSTRAP_core__StandardObject_O
  /* 15 */ , obj_skip_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 16 */ , obj_skip_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 17 */ , obj_skip_KIND_BOOTSTRAP_core__Specializer_O
  /* 18 */ , obj_skip_KIND_BOOTSTRAP_core__Symbol_O
  /* 19 */ , obj_skip_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 20 */ , obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 21 */ , obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 22 */ , obj_skip_KIND_LISPALLOC_core__SingleFloat_O
  /* 23 */ , obj_skip_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 24 */ , obj_skip_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 25 */ , obj_skip_KIND_LISPALLOC_core__WeakPointer_O
  /* 26 */ , obj_skip_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 27 */ , obj_skip_KIND_LISPALLOC_core__CandoException_O
  /* 28 */ , obj_skip_KIND_LISPALLOC_core__Ratio_O
  /* 29 */ , obj_skip_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 30 */ , obj_skip_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 31 */ , obj_skip_KIND_LISPALLOC_core__Environment_O
  /* 32 */ , obj_skip_KIND_BOOTSTRAP_core__Metaobject_O
  /* 33 */ , obj_skip_KIND_LISPALLOC_core__Float_O
  /* 34 */ , obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 35 */ , obj_skip_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 36 */ , obj_skip_KIND_LISPALLOC_core__Pointer_O
  /* 37 */ , obj_skip_KIND_LISPALLOC_core__DoubleFloat_O
  /* 38 */ , obj_skip_KIND_LISPALLOC_core__Cons_O
  /* 39 */ , obj_skip_KIND_LISPALLOC_core__Number_O
  /* 40 */ , obj_skip_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 41 */ , obj_skip_KIND_CLASSALLOC_core__MacroClosure
  /* 42 */ , obj_skip_KIND_LISPALLOC_core__ValueFrame_O
  /* 43 */ , obj_skip_KIND_LISPALLOC_core__Integer_O
  /* 44 */ , obj_skip_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 45 */ , obj_skip_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 46 */ , obj_skip_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 47 */ , obj_skip_KIND_LISPALLOC_core__ShortFloat_O
  /* 48 */ , obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 49 */ , obj_skip_KIND_BOOTSTRAP_core__Class_O
  /* 50 */ , obj_skip_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , obj_skip_KIND_LISPALLOC_core__SymbolToEnumConverter_O
#endif // defined(GC_OBJ_SKIP_TABLE)
#if defined(GC_OBJ_SCAN)
GC_RESULT obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Function_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    POINTER_FIX(obj_gc_safe->closure);
    typedef core::Function_O type_KIND_LISPALLOC_core__Function_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Function_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Fixnum_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Fixnum_O type_KIND_LISPALLOC_core__Fixnum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Fixnum_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
    gctools::GCString_moveable<char>* obj_gc_safe = reinterpret_cast<gctools::GCString_moveable<char>*>(client);
    typedef typename gctools::GCString_moveable<char> type_KIND_GCSTRING_gctools__GCString_moveable_char_;
    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_KIND_GCSTRING_gctools__GCString_moveable_char_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + Align(header_and_gcstring_size);
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__CompiledFunction_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    POINTER_FIX(obj_gc_safe->closure);
    typedef core::CompiledFunction_O type_KIND_LISPALLOC_core__CompiledFunction_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompiledFunction_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Real_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Real_O type_KIND_LISPALLOC_core__Real_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Real_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__FunctionFrame_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    POINTER_FIX(obj_gc_safe->_Objects._Array._Contents);
    typedef core::FunctionFrame_O type_KIND_LISPALLOC_core__FunctionFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionFrame_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__LexicalEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::LexicalEnvironment_O type_KIND_LISPALLOC_core__LexicalEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__LexicalEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__MacroletEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_Macros);
    typedef core::MacroletEnvironment_O type_KIND_LISPALLOC_core__MacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__MacroletEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Bignum_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Bignum_O type_KIND_LISPALLOC_core__Bignum_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Bignum_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__GlueEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Map);
    SMART_PTR_FIX(obj_gc_safe->_Args);
    typedef core::GlueEnvironment_O type_KIND_LISPALLOC_core__GlueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__GlueEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Rational_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Rational_O type_KIND_LISPALLOC_core__Rational_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Rational_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_BOOTSTRAP_core__StandardObject_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::StandardObject_O type_KIND_BOOTSTRAP_core__StandardObject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__StandardObject_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_Macros);
    typedef core::SymbolMacroletEnvironment_O type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    typedef core::RuntimeVisibleEnvironment_O type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_BOOTSTRAP_core__Specializer_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Specializer_O type_KIND_BOOTSTRAP_core__Specializer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Specializer_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_BOOTSTRAP_core__Symbol_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Name);
    SMART_PTR_FIX(obj_gc_safe->_HomePackage);
    SMART_PTR_FIX(obj_gc_safe->_Value);
    SMART_PTR_FIX(obj_gc_safe->_Function);
    SMART_PTR_FIX(obj_gc_safe->_PropertyList);
    typedef core::Symbol_O type_KIND_BOOTSTRAP_core__Symbol_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Symbol_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_FunctionIndices);
    SMART_PTR_FIX(obj_gc_safe->_FunctionFrame);
    typedef core::FunctionValueEnvironment_O type_KIND_LISPALLOC_core__FunctionValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionValueEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    gctools::GCVector_moveable<core::CacheRecord>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::CacheRecord>*>(client);
    for (gctools::GCVector_moveable<core::CacheRecord>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_key);
    SMART_PTR_FIX(it->_value);
    }
    typedef typename gctools::GCVector_moveable<core::CacheRecord> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>*>(client);
    for (gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>> type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__SingleFloat_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::SingleFloat_O type_KIND_LISPALLOC_core__SingleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SingleFloat_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__TagbodyFrame_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    typedef core::TagbodyFrame_O type_KIND_LISPALLOC_core__TagbodyFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyFrame_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__CompileTimeEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::CompileTimeEnvironment_O type_KIND_LISPALLOC_core__CompileTimeEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CompileTimeEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__WeakPointer_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::WeakPointer_O* obj_gc_safe = reinterpret_cast<core::WeakPointer_O*>(client);
    POINTER_FIX(obj_gc_safe->_WeakObject.pointer);
    typedef core::WeakPointer_O type_KIND_LISPALLOC_core__WeakPointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__WeakPointer_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>* obj_gc_safe = reinterpret_cast<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>*>(client);
    for (gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
          SMART_PTR_FIX(*it);
    }
    typedef typename gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0> type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__CandoException_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    POINTER_FIX(obj_gc_safe->_message._Contents);
    typedef core::CandoException_O type_KIND_LISPALLOC_core__CandoException_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CandoException_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Ratio_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_numerator);
    SMART_PTR_FIX(obj_gc_safe->_denominator);
    typedef core::Ratio_O type_KIND_LISPALLOC_core__Ratio_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Ratio_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_SymbolIndex);
    SMART_PTR_FIX(obj_gc_safe->_ActivationFrame);
    typedef core::ValueEnvironment_O type_KIND_LISPALLOC_core__ValueEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__TagbodyEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_RuntimeEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Tags);
    POINTER_FIX(obj_gc_safe->_TagCode._Vector._Contents);
    SMART_PTR_FIX(obj_gc_safe->_ActivationFrame);
    typedef core::TagbodyEnvironment_O type_KIND_LISPALLOC_core__TagbodyEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__TagbodyEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Environment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Environment_O type_KIND_LISPALLOC_core__Environment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Environment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_BOOTSTRAP_core__Metaobject_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Metaobject_O type_KIND_BOOTSTRAP_core__Metaobject_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Metaobject_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Float_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Float_O type_KIND_LISPALLOC_core__Float_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Float_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    gctools::GCVector_moveable<core::DynamicBinding>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::DynamicBinding>*>(client);
    for (gctools::GCVector_moveable<core::DynamicBinding>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_Var);
    SMART_PTR_FIX(it->_Val);
    }
    typedef typename gctools::GCVector_moveable<core::DynamicBinding> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__CatchEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::CatchEnvironment_O type_KIND_LISPALLOC_core__CatchEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__CatchEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Pointer_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Pointer_O type_KIND_LISPALLOC_core__Pointer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Pointer_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__DoubleFloat_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::DoubleFloat_O type_KIND_LISPALLOC_core__DoubleFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__DoubleFloat_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Cons_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Car);
    SMART_PTR_FIX(obj_gc_safe->_Cdr);
    typedef core::Cons_O type_KIND_LISPALLOC_core__Cons_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Cons_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Number_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Number_O type_KIND_LISPALLOC_core__Number_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Number_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    gctools::GCVector_moveable<core::ExceptionEntry>* obj_gc_safe = reinterpret_cast<gctools::GCVector_moveable<core::ExceptionEntry>*>(client);
    for (gctools::GCVector_moveable<core::ExceptionEntry>::iterator it = obj_gc_safe->begin(); it!=obj_gc_safe->end(); ++it) {
    SMART_PTR_FIX(it->_Key);
    }
    typedef typename gctools::GCVector_moveable<core::ExceptionEntry> type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_;
    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_>(obj_gc_safe->capacity()))+AlignUp(sizeof(gctools::Header_s));
    client = (char*)client + header_and_gccontainer_size;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_CLASSALLOC_core__MacroClosure(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_SourcePosInfo);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_lambdaListHandler);
    typedef core::MacroClosure type_KIND_CLASSALLOC_core__MacroClosure;
    client = (char*)client + AlignUp(sizeof(type_KIND_CLASSALLOC_core__MacroClosure)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__ValueFrame_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentFrame);
    POINTER_FIX(obj_gc_safe->_Objects._Array._Contents);
    SMART_PTR_FIX(obj_gc_safe->_DebuggingInfo);
    typedef core::ValueFrame_O type_KIND_LISPALLOC_core__ValueFrame_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ValueFrame_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Integer_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::Integer_O type_KIND_LISPALLOC_core__Integer_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Integer_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__UnwindProtectEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::UnwindProtectEnvironment_O type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__UnwindProtectEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__BlockEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    SMART_PTR_FIX(obj_gc_safe->_BlockSymbol);
    typedef core::BlockEnvironment_O type_KIND_LISPALLOC_core__BlockEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__BlockEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__FunctionContainerEnvironment_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_ParentEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_Metadata);
    typedef core::FunctionContainerEnvironment_O type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__FunctionContainerEnvironment_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__ShortFloat_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    typedef core::ShortFloat_O type_KIND_LISPALLOC_core__ShortFloat_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__ShortFloat_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    SMART_PTR_FIX(obj_gc_safe->name);
    SMART_PTR_FIX(obj_gc_safe->closedEnvironment);
    SMART_PTR_FIX(obj_gc_safe->_SourcePosInfo);
    SMART_PTR_FIX(obj_gc_safe->kind);
    SMART_PTR_FIX(obj_gc_safe->_lambdaListHandler);
    client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss); 
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_BOOTSTRAP_core__Class_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_Signature_ClassSlots);
    POINTER_FIX(obj_gc_safe->_creator);
    POINTER_FIX(obj_gc_safe->_MetaClassSlots._Vector._Contents);
    typedef core::Class_O type_KIND_BOOTSTRAP_core__Class_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_BOOTSTRAP_core__Class_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__Complex_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    SMART_PTR_FIX(obj_gc_safe->_real);
    SMART_PTR_FIX(obj_gc_safe->_imaginary);
    typedef core::Complex_O type_KIND_LISPALLOC_core__Complex_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__Complex_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
GC_RESULT obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit){
  MPS_SCAN_BEGIN(ss) {
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    POINTER_FIX(obj_gc_safe->_WhatTheEnumsRepresent._Contents);
    SMART_PTR_FIX(obj_gc_safe->_EnumToSymbol);
    SMART_PTR_FIX(obj_gc_safe->_ArchiveSymbolToEnum);
    SMART_PTR_FIX(obj_gc_safe->_EnumToArchiveSymbol);
    SMART_PTR_FIX(obj_gc_safe->_SymbolToEnum);
    typedef core::SymbolToEnumConverter_O type_KIND_LISPALLOC_core__SymbolToEnumConverter_O;
    client = (char*)client + AlignUp(sizeof(type_KIND_LISPALLOC_core__SymbolToEnumConverter_O)) + global_alignup_sizeof_header;
  } MPS_SCAN_END(ss);
    return GC_RESULT_OK;
}
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_TABLE)
GC_RESULT (*)(mps_ss_t ss, mps_addr_t& client, mps_addr_t limit) OBJ_SCAN_table[] = { NULL 
  /* 1 */ , obj_scan_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , obj_scan_KIND_LISPALLOC_core__Function_O
  /* 3 */ , obj_scan_KIND_LISPALLOC_core__Fixnum_O
  /* 4 */ , obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 5 */ , obj_scan_KIND_LISPALLOC_core__CompiledFunction_O
  /* 6 */ , obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 7 */ , obj_scan_KIND_LISPALLOC_core__Real_O
  /* 8 */ , obj_scan_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , obj_scan_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 10 */ , obj_scan_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 11 */ , obj_scan_KIND_LISPALLOC_core__Bignum_O
  /* 12 */ , obj_scan_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 13 */ , obj_scan_KIND_LISPALLOC_core__Rational_O
  /* 14 */ , obj_scan_KIND_BOOTSTRAP_core__StandardObject_O
  /* 15 */ , obj_scan_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 16 */ , obj_scan_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 17 */ , obj_scan_KIND_BOOTSTRAP_core__Specializer_O
  /* 18 */ , obj_scan_KIND_BOOTSTRAP_core__Symbol_O
  /* 19 */ , obj_scan_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 20 */ , obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 21 */ , obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 22 */ , obj_scan_KIND_LISPALLOC_core__SingleFloat_O
  /* 23 */ , obj_scan_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 24 */ , obj_scan_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 25 */ , obj_scan_KIND_LISPALLOC_core__WeakPointer_O
  /* 26 */ , obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 27 */ , obj_scan_KIND_LISPALLOC_core__CandoException_O
  /* 28 */ , obj_scan_KIND_LISPALLOC_core__Ratio_O
  /* 29 */ , obj_scan_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 30 */ , obj_scan_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 31 */ , obj_scan_KIND_LISPALLOC_core__Environment_O
  /* 32 */ , obj_scan_KIND_BOOTSTRAP_core__Metaobject_O
  /* 33 */ , obj_scan_KIND_LISPALLOC_core__Float_O
  /* 34 */ , obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 35 */ , obj_scan_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 36 */ , obj_scan_KIND_LISPALLOC_core__Pointer_O
  /* 37 */ , obj_scan_KIND_LISPALLOC_core__DoubleFloat_O
  /* 38 */ , obj_scan_KIND_LISPALLOC_core__Cons_O
  /* 39 */ , obj_scan_KIND_LISPALLOC_core__Number_O
  /* 40 */ , obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 41 */ , obj_scan_KIND_CLASSALLOC_core__MacroClosure
  /* 42 */ , obj_scan_KIND_LISPALLOC_core__ValueFrame_O
  /* 43 */ , obj_scan_KIND_LISPALLOC_core__Integer_O
  /* 44 */ , obj_scan_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 45 */ , obj_scan_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 46 */ , obj_scan_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 47 */ , obj_scan_KIND_LISPALLOC_core__ShortFloat_O
  /* 48 */ , obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 49 */ , obj_scan_KIND_BOOTSTRAP_core__Class_O
  /* 50 */ , obj_scan_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , obj_scan_KIND_LISPALLOC_core__SymbolToEnumConverter_O
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
void obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__Creator(mps_addr_t client){
    core::Creator* obj_gc_safe = reinterpret_cast<core::Creator*>(client);
    obj_gc_safe->~Creator();
}
void obj_finalize_KIND_LISPALLOC_core__Function_O(mps_addr_t client){
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Fixnum_O(mps_addr_t client){
    core::Fixnum_O* obj_gc_safe = reinterpret_cast<core::Fixnum_O*>(client);
    obj_gc_safe->~Fixnum_O();
    return;
}
void obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
void obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O(mps_addr_t client){
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    obj_gc_safe->~CompiledFunction_O();
    return;
}
void obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>"));}
void obj_finalize_KIND_LISPALLOC_core__Real_O(mps_addr_t client){
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    obj_gc_safe->~Real_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O(mps_addr_t client){
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    obj_gc_safe->~FunctionFrame_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O(mps_addr_t client){
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    obj_gc_safe->~LexicalEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O(mps_addr_t client){
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    obj_gc_safe->~MacroletEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Bignum_O(mps_addr_t client){
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    obj_gc_safe->~Bignum_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O(mps_addr_t client){
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    obj_gc_safe->~GlueEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Rational_O(mps_addr_t client){
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    obj_gc_safe->~Rational_O();
    return;
}
void obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O(mps_addr_t client){
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    obj_gc_safe->~StandardObject_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O(mps_addr_t client){
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    obj_gc_safe->~SymbolMacroletEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O(mps_addr_t client){
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    obj_gc_safe->~RuntimeVisibleEnvironment_O();
    return;
}
void obj_finalize_KIND_BOOTSTRAP_core__Specializer_O(mps_addr_t client){
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    obj_gc_safe->~Specializer_O();
    return;
}
void obj_finalize_KIND_BOOTSTRAP_core__Symbol_O(mps_addr_t client){
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    obj_gc_safe->~Symbol_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O(mps_addr_t client){
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    obj_gc_safe->~FunctionValueEnvironment_O();
    return;
}
void obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
void obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
void obj_finalize_KIND_LISPALLOC_core__SingleFloat_O(mps_addr_t client){
    core::SingleFloat_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_O*>(client);
    obj_gc_safe->~SingleFloat_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O(mps_addr_t client){
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    obj_gc_safe->~TagbodyFrame_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O(mps_addr_t client){
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    obj_gc_safe->~CompileTimeEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__WeakPointer_O(mps_addr_t client){
    core::WeakPointer_O* obj_gc_safe = reinterpret_cast<core::WeakPointer_O*>(client);
    obj_gc_safe->~WeakPointer_O();
    return;
}
void obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
void obj_finalize_KIND_LISPALLOC_core__CandoException_O(mps_addr_t client){
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    obj_gc_safe->~CandoException_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Ratio_O(mps_addr_t client){
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    obj_gc_safe->~Ratio_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O(mps_addr_t client){
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    obj_gc_safe->~ValueEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O(mps_addr_t client){
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    obj_gc_safe->~TagbodyEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Environment_O(mps_addr_t client){
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    obj_gc_safe->~Environment_O();
    return;
}
void obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O(mps_addr_t client){
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    obj_gc_safe->~Metaobject_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Float_O(mps_addr_t client){
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    obj_gc_safe->~Float_O();
    return;
}
void obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
void obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O(mps_addr_t client){
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    obj_gc_safe->~CatchEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Pointer_O(mps_addr_t client){
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O(mps_addr_t client){
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    obj_gc_safe->~DoubleFloat_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Cons_O(mps_addr_t client){
    core::Cons_O* obj_gc_safe = reinterpret_cast<core::Cons_O*>(client);
    obj_gc_safe->~Cons_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Number_O(mps_addr_t client){
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    obj_gc_safe->~Number_O();
    return;
}
void obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_(mps_addr_t client){
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
void obj_finalize_KIND_CLASSALLOC_core__MacroClosure(mps_addr_t client){
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    obj_gc_safe->~MacroClosure();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__ValueFrame_O(mps_addr_t client){
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    obj_gc_safe->~ValueFrame_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Integer_O(mps_addr_t client){
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    obj_gc_safe->~Integer_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O(mps_addr_t client){
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    obj_gc_safe->~UnwindProtectEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O(mps_addr_t client){
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    obj_gc_safe->~BlockEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O(mps_addr_t client){
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    obj_gc_safe->~FunctionContainerEnvironment_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__ShortFloat_O(mps_addr_t client){
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    obj_gc_safe->~ShortFloat_O();
    return;
}
void obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure(mps_addr_t client){
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    obj_gc_safe->~BuiltinClosure();
}
void obj_finalize_KIND_BOOTSTRAP_core__Class_O(mps_addr_t client){
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    obj_gc_safe->~Class_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__Complex_O(mps_addr_t client){
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    obj_gc_safe->~Complex_O();
    return;
}
void obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O(mps_addr_t client){
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    obj_gc_safe->~SymbolToEnumConverter_O();
    return;
}
#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_TABLE)
void (*)(mps_addr_t client) OBJ_FINALIZE_table[] = { NULL 
  /* 1 */ , obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__Creator
  /* 2 */ , obj_finalize_KIND_LISPALLOC_core__Function_O
  /* 3 */ , obj_finalize_KIND_LISPALLOC_core__Fixnum_O
  /* 4 */ , obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_
  /* 5 */ , obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O
  /* 6 */ , obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__
  /* 7 */ , obj_finalize_KIND_LISPALLOC_core__Real_O
  /* 8 */ , obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O
  /* 9 */ , obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O
  /* 10 */ , obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O
  /* 11 */ , obj_finalize_KIND_LISPALLOC_core__Bignum_O
  /* 12 */ , obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O
  /* 13 */ , obj_finalize_KIND_LISPALLOC_core__Rational_O
  /* 14 */ , obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O
  /* 15 */ , obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O
  /* 16 */ , obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O
  /* 17 */ , obj_finalize_KIND_BOOTSTRAP_core__Specializer_O
  /* 18 */ , obj_finalize_KIND_BOOTSTRAP_core__Symbol_O
  /* 19 */ , obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O
  /* 20 */ , obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_
  /* 21 */ , obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__
  /* 22 */ , obj_finalize_KIND_LISPALLOC_core__SingleFloat_O
  /* 23 */ , obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O
  /* 24 */ , obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O
  /* 25 */ , obj_finalize_KIND_LISPALLOC_core__WeakPointer_O
  /* 26 */ , obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_
  /* 27 */ , obj_finalize_KIND_LISPALLOC_core__CandoException_O
  /* 28 */ , obj_finalize_KIND_LISPALLOC_core__Ratio_O
  /* 29 */ , obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O
  /* 30 */ , obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O
  /* 31 */ , obj_finalize_KIND_LISPALLOC_core__Environment_O
  /* 32 */ , obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O
  /* 33 */ , obj_finalize_KIND_LISPALLOC_core__Float_O
  /* 34 */ , obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_
  /* 35 */ , obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O
  /* 36 */ , obj_finalize_KIND_LISPALLOC_core__Pointer_O
  /* 37 */ , obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O
  /* 38 */ , obj_finalize_KIND_LISPALLOC_core__Cons_O
  /* 39 */ , obj_finalize_KIND_LISPALLOC_core__Number_O
  /* 40 */ , obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_
  /* 41 */ , obj_finalize_KIND_CLASSALLOC_core__MacroClosure
  /* 42 */ , obj_finalize_KIND_LISPALLOC_core__ValueFrame_O
  /* 43 */ , obj_finalize_KIND_LISPALLOC_core__Integer_O
  /* 44 */ , obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O
  /* 45 */ , obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O
  /* 46 */ , obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O
  /* 47 */ , obj_finalize_KIND_LISPALLOC_core__ShortFloat_O
  /* 48 */ , obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure
  /* 49 */ , obj_finalize_KIND_BOOTSTRAP_core__Class_O
  /* 50 */ , obj_finalize_KIND_LISPALLOC_core__Complex_O
  /* 51 */ , obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_GLOBALS)
 SMART_PTR_FIX(core::WeakPointer_O::___staticClass);
 POINTER_FIX(core::WeakPointer_O::static_creator);
 SMART_PTR_FIX(core::WeakPointer_O::___staticClassSymbol);
#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
