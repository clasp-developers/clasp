#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/evaluator.h> // af_interpreter_lookup_macro
#include <clasp/core/sysprop.h> // core__get_sysprop

namespace core {

T_sp BytecodeCmpEnv_O::variableInfo(T_sp varname) {
  T_sp vars = this->vars();
  if (vars.nilp()) return vars;
  else {
    T_sp pair = gc::As<Cons_sp>(vars)->assoc(varname, nil<T_O>(),
                                             cl::_sym_eq, nil<T_O>());
    if (pair.nilp()) return pair;
    else return oCdr(pair);
  }
}

T_sp BytecodeCmpEnv_O::lookupSymbolMacro(T_sp sname) {
  T_sp info = this->variableInfo(sname);
  if (gc::IsA<BytecodeCmpSymbolMacroVarInfo_sp>(info))
    return gc::As_unsafe<BytecodeCmpSymbolMacroVarInfo_sp>(info)->expander();
  else if (info.notnilp()) { // global?
    T_mv result = core__get_sysprop(sname, ext::_sym_symbolMacro);
    if (gc::As<T_sp>(result.valueGet_(1)).notnilp()) {
      return result;
    } else return nil<T_O>();
  } else return nil<T_O>();
}

T_sp BytecodeCmpEnv_O::functionInfo(T_sp funname) {
  T_sp funs = this->funs();
  if (funs.nilp()) return funs;
  else {
    T_sp pair = gc::As<Cons_sp>(funs)->assoc(funname, nil<T_O>(),
                                             cl::_sym_equal, nil<T_O>());
    if (pair.nilp()) return pair;
    else return oCdr(pair);
  }
}

T_sp BytecodeCmpEnv_O::lookupMacro(T_sp macroname) {
  T_sp info = this->functionInfo(macroname);
  if (gc::IsA<BytecodeCmpGlobalMacroInfo_sp>(info))
    return gc::As_unsafe<BytecodeCmpGlobalMacroInfo_sp>(info)->expander();
  else if (gc::IsA<BytecodeCmpLocalMacroInfo_sp>(info))
    return gc::As_unsafe<BytecodeCmpLocalMacroInfo_sp>(info)->expander();
  else if (info.nilp()) // could be global
    return af_interpreter_lookup_macro(macroname, nil<T_O>());
  else return nil<T_O>();
}

CL_LAMBDA(context &key (receiving (bytecode-cmp-context/receiving context)) (cfunction (bytecode-cmp-context/cfunction context)))
CL_DEFUN BytecodeCmpContext_sp new_context(BytecodeCmpContext_sp parent,
                                           T_sp receiving,
                                           T_sp cfunction) {
  return BytecodeCmpContext_O::make(receiving, cfunction);
}

}; //namespace core
