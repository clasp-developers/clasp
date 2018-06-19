#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/arguments.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/predicates.h>
//#include <clasp/core/clc.h>
#include <clasp/core/clcenv.h>
#include <clasp/core/wrappers.h>


namespace clcenv {

core::T_sp GlobalEnvironment_O::compilation_speed() const
{
  IMPLEMENT_ME();
}
core::T_sp GlobalEnvironment_O::debug() const
{
  IMPLEMENT_ME();
}

core::T_sp GlobalEnvironment_O::space() const
{
  IMPLEMENT_ME();
}

core::T_sp GlobalEnvironment_O::speed() const
{
  IMPLEMENT_ME();
}

core::T_sp GlobalEnvironment_O::safety() const
{
  IMPLEMENT_ME();
}
 
};

namespace clcenv {

CL_DEFUN core::T_sp defining_variable_info_global(core::T_sp tsymbol)
{
  core::Symbol_sp symbol = gc::As<core::Symbol_sp>(tsymbol);
  if ( core::cl__constantp(symbol) ) {
    ConstantVariableInfo_sp info = ConstantVariableInfo_O::create();
    info->_Name = symbol;
    info->_Value = symbol->symbolValue();
    return info;
  } else if (symbol != core::cl__macroexpand_1(symbol,_Nil<core::T_O>()) ) {
    SymbolMacroInfo_sp info = SymbolMacroInfo_O::create();
    info->_Name = symbol;
    info->_Expansion = core::cl__macroexpand_1(symbol,_Nil<core::T_O>());
    return info;
  } else if (symbol->boundP()) {
    SpecialVariableInfo_sp info = SpecialVariableInfo_O::create();
    info->_Name = symbol;
    return info;
  } else if (symbol->specialP()) {
    SpecialVariableInfo_sp info = SpecialVariableInfo_O::create();
    info->_Name = symbol;
    return info;
  }
  return _Nil<core::T_O>();
}

CL_DEFUN bool clcenv__treat_as_special_operator_p(core::Symbol_sp symbol)
{
  if ( symbol == cl::_sym_unwind_protect ) return false;
  if ( symbol == cl::_sym_catch ) return false;
  if ( symbol == cl::_sym_throw ) return false;
  if ( symbol == core::_sym_debug_message ) return true;
  if ( symbol == core::_sym_multiple_value_foreign_call ) return true;
  if ( symbol == core::_sym_foreign_call ) return true;
  if ( symbol == core::_sym_foreign_call_pointer ) return true;
  return core::cl__special_operator_p(symbol);
}
CL_DEFUN core::T_sp defining_function_info_global(core::T_sp function_name)
{
  if ( core::Symbol_sp s = function_name.asOrNull<core::Symbol_O>() ) {
    if ( clcenv__treat_as_special_operator_p(s) ) {
      auto info = SpecialOperatorInfo_O::create();
      info->_Name = function_name;
      return info;
    }
    if ( core::cl__macro_function(function_name,_Nil<core::T_O>()).notnilp() ) {
      // It's a global macro
      auto info = GlobalMacroInfo_O::create();
      info->_Name = function_name;
      info->_Expander = core::cl__macro_function(function_name,_Nil<core::T_O>());
      info->_CompilerMacro = core::cl__compiler_macro_function(function_name,_Nil<core::T_O>());
      return info;
    }
  }
  if ( core::cl__fboundp(function_name) ) {
    auto info = GlobalFunctionInfo_O::create();
    info->_Name = function_name;
    info->_CompilerMacro = core::cl__compiler_macro_function(function_name,_Nil<core::T_O>());
    info->_Inline = core::core__get_global_inline_status(function_name,_Nil<core::T_O>());
    core::T_sp fdef = core::cl__fdefinition(function_name);
    info->_Ast = _Nil<core::T_O>(); // FIXME: stored in environments now
    return info;
  }
  return _Nil<core::T_O>();
}
      
 
CL_DEFUN core::T_sp defining_variable_info(core::T_sp environment, core::T_sp symbol)
{
  if ( clcenv::LexicalVariable_sp lv = environment.asOrNull<clcenv::LexicalVariable_O>() ) {
    if ( symbol == lv->_Name ) {
      LexicalVariableInfo_sp info = LexicalVariableInfo_O::create();
      info->_Name = symbol;
      info->_Identity = lv->_Next;
      return info;
    }
  } else if ( clcenv::SpecialVariable_sp sv = environment.asOrNull<clcenv::SpecialVariable_O>() ) {
    if ( symbol == lv->_Name ) {
      SpecialVariableInfo_sp info = SpecialVariableInfo_O::create();
      info->_Name = symbol;
      info->_GlobalP = _Nil<core::T_O>();
      return info;
    }
  } else if ( clcenv::SymbolMacro_sp sm = environment.asOrNull<clcenv::SymbolMacro_O>() ) { 
    if ( symbol == lv->_Name ) {
      SymbolMacroInfo_sp info = SymbolMacroInfo_O::create();
      info->_Name = symbol;
      info->_Expansion  = sm->_Expansion;
      return info;
    }
  } else if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return defining_variable_info_global(symbol);
  }
  // At this point it's guaranteed to be an entry
  Entry_sp entry = gc::As<Entry_sp>(environment);
  return defining_variable_info(entry->_Next,symbol);
}

CL_DEFUN core::T_sp variable_type(core::T_sp environment, Info_sp defining_info)
{
  // The global environment
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return core::Cons_O::createList(defining_info->type());
  } else if ( LexicalVariable_sp lv = environment.asOrNull<LexicalVariable_O>() ) {
    if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
      if ( lv->_Name == lvi->_Name ) {
        return core::Cons_O::createList(lvi->_Type);
      }
    }
    return variable_type(lv->_Next,defining_info);
  } else if ( SpecialVariable_sp sv = environment.asOrNull<SpecialVariable_O>() ) {
    if ( SpecialVariableInfo_sp svi = defining_info.asOrNull<SpecialVariableInfo_O>() ) {
      if ( sv->_Name == svi->_Name ) {
        return core::Cons_O::createList(svi->_Type);
      }
    }
    return variable_type(sv->_Next,defining_info);
  } else if ( SymbolMacro_sp sm = environment.asOrNull<SymbolMacro_O>() ) {
    if ( SymbolMacroInfo_sp smi = defining_info.asOrNull<SymbolMacroInfo_O>() ) {
      if ( sm->_Name == smi->_Name ) {
        return core::Cons_O::createList(smi->_Type);
      }
    }
    return variable_type(lv->_Next,defining_info);
  } else if ( VariableType_sp vt = environment.asOrNull<VariableType_O>() ) {
    if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
      if ( vt->_Name == lvi->_Name ) {
        return core::Cons_O::createList(vt->_Type,variable_type(vt->_Next,defining_info));
      } else {
        return variable_type(vt->_Next,defining_info);
      }
    } else if ( SpecialVariableInfo_sp svi = defining_info.asOrNull<SpecialVariableInfo_O>() ) {
      if ( vt->_Name == svi->_Name ) {
        return core::Cons_O::createList(vt->_Type,variable_type(vt->_Next,defining_info));
      } else {
        return variable_type(vt->_Next,defining_info);
      }
    } else if ( SymbolMacroInfo_sp smi = defining_info.asOrNull<SymbolMacroInfo_O>() ) {
      if ( vt->_Name == smi->_Name ) {
        return core::Cons_O::createList(vt->_Type,variable_type(vt->_Next,defining_info));
      } else {
        return variable_type(vt->_Next,defining_info);
      }
    }
  }
  // The default case
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return variable_type(ee->_Next,defining_info);
}

CL_DEFUN core::T_sp variable_ignore(core::T_sp environment, Info_sp defining_info)
{
  // The global environment
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return _Nil<core::T_O>();
  } else if ( LexicalVariable_sp lv = environment.asOrNull<LexicalVariable_O>() ) {
    if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
      if ( lv->_Name == lvi->_Name ) {
        return _Nil<core::T_O>();
      } else {
        return variable_ignore(lv->_Next,defining_info);
      }
    }
  } else if ( SpecialVariable_sp sv = environment.asOrNull<SpecialVariable_O>() ) {
    if ( SpecialVariableInfo_sp svi = defining_info.asOrNull<SpecialVariableInfo_O>() ) {
      if ( sv->_Name == svi->_Name ) {
        return _Nil<core::T_O>();
      } else {
        return variable_ignore(sv->_Next,defining_info);
      }
    }
  } else if ( VariableIgnore_sp vi = environment.asOrNull<VariableIgnore_O>() ) {
    if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
      if ( vi->_Name == lvi->_Name ) {
        return environment;
      } else {
        return variable_ignore(vi->_Next,defining_info);
      }
    } else if ( SpecialVariableInfo_sp svi = defining_info.asOrNull<SpecialVariableInfo_O>() ) {
      if ( vi->_Name == svi->_Name ) {
        return environment;
      } else {
        return variable_ignore(vi->_Next,defining_info);
      }
    }
  }
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return variable_ignore(ee->_Next,defining_info);
}

CL_DEFUN core::T_sp variable_dynamic_extent(core::T_sp environment, Info_sp defining_info)
{
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return _Nil<core::T_O>();
  } else if ( LexicalVariable_sp lv = environment.asOrNull<LexicalVariable_O>() ) {
    if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
      if ( lv->_Name == lvi->_Name ) {
        return environment;
      } else {
        return variable_dynamic_extent(lv->_Next,defining_info);
      }
    }
  } else if ( VariableDynamicExtent_sp vde = environment.asOrNull<VariableDynamicExtent_O>() ) {
    if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
      if ( vde->_Name == lvi->_Name ) {
        return environment;
      } else {
        return variable_dynamic_extent(lv->_Next,defining_info);
      }
    }
  }
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return variable_dynamic_extent(ee->_Next,defining_info);
}

CL_DEFUN core::T_sp make_info_variable(core::T_sp environment, Info_sp defining_info)
{
  if ( LexicalVariableInfo_sp lvi = defining_info.asOrNull<LexicalVariableInfo_O>() ) {
    LexicalVariableInfo_sp info = LexicalVariableInfo_O::create();
    info->_Name = lvi->_Name;
    info->_Identity = lvi->_Identity;
    info->_Type = core::Cons_O::create(cl::_sym_and, variable_type(environment,defining_info));
    {
      Entry_sp entry = variable_ignore(environment,defining_info);
      if ( entry.nilp() ) info->_Ignore = _Nil<core::T_O>();
      else info->_Ignore = lvi->_Ignore;
    }
    {
      Entry_sp entry = variable_dynamic_extent(environment,defining_info);
      if ( entry.nilp() ) info->_DynamicExtent = lvi->_DynamicExtent;
      else info->_DynamicExtent = _lisp->_true();
    }
    return info;
  } else if ( SpecialVariableInfo_sp svi = defining_info.asOrNull<SpecialVariableInfo_O>() ) {
    auto info = SpecialVariableInfo_O::create();
    info->_Name = svi->_Name;
    info->_Type = core::Cons_O::create(cl::_sym_and,variable_type(environment,defining_info));
    info->_GlobalP = svi->_GlobalP;
    {
      Entry_sp entry = variable_ignore(environment,defining_info);
      if ( entry.nilp() ) info->_Ignore = _Nil<core::T_O>();
      else info->_Ignore = entry->ignore();
    }
    return info;
  } else if ( ConstantVariableInfo_sp cvi = defining_info.asOrNull<ConstantVariableInfo_O>() ) {
    return defining_info;
  } else if ( SymbolMacroInfo_sp smi = defining_info.asOrNull<SymbolMacroInfo_O>() ) {
    auto info = SymbolMacroInfo_O::create();
    info->_Name = smi->_Name;
    info->_Expansion = smi->_Expansion;
    info->_Type = core::Cons_O::create(cl::_sym_and,variable_type(environment,defining_info));
    return info;
  }
  SIMPLE_ERROR(BF("What do I do if we get here?"));
}


CL_DEFUN core::T_sp variable_info(core::T_sp environment, core::T_sp symbol)
{
  Info_sp defining_info = defining_variable_info(environment,symbol);
  if ( defining_info.nilp() ) {
    return _Nil<core::T_O>();
  } else {
    return make_info_variable(environment,defining_info);
  }
}

CL_DEFUN core::T_sp defining_function_info(core::T_sp environment, core::T_sp function_name)
{
  if ( Function_sp f = environment.asOrNull<Function_O>() ) {
    if ( core::cl__equal(f->_Name,function_name) ) {
      auto info = LocalFunctionInfo_O::create();
      info->_Name = function_name;
      info->_Identity = f->_Identity;
      return info;
    } else {
      return defining_function_info(f->_Next,function_name);
    }
  } else if ( Macro_sp m = environment.asOrNull<Macro_O>() ) {
    if ( core::cl__eq(m->_Name,function_name) ) {
      auto info = LocalMacroInfo_O::create();
      info->_Name = function_name;
      info->_Expander = m->_Expander;
      return info;
    } else {
      return defining_function_info(m->_Next,function_name);
    }
  } else if ( Entry_sp e = environment.asOrNull<Entry_O>() ) {
    return defining_function_info(e->_Next,function_name);
  }
  // Now check the global environment
  return defining_function_info_global(function_name);
}

CL_DEFUN core::T_sp function_type(core::T_sp environment, Info_sp defining_info )
{
  // Global environment
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return defining_info->type();
  } else if ( Function_sp fe = environment.asOrNull<Function_O>() ) {
    if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
      if ( core::cl__equal(fe->_Name,lfi->_Name) ) {
        return core::Cons_O::create(lfi->_Type);
      } else {
        return function_type(fe->_Next,defining_info);
      }
    }
  } else if ( Macro_sp me = environment.asOrNull<Macro_O>() ) {
    if ( LocalMacroInfo_sp lmi = defining_info.asOrNull<LocalMacroInfo_O>() ) {
      if ( core::cl__eq(me->_Name,lmi->_Name) ) {
        return _Nil<core::T_O>();
      } else {
        return function_type(me->_Next,defining_info);
      }
    }
  } else if ( FunctionType_sp fte = environment.asOrNull<FunctionType_O>() ) {
    if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
      if ( core::cl__equal(fte->_Name,lfi->_Name) ) {
        return core::Cons_O::create(fte->_Type,function_type(fte->_Next,defining_info));
      } else {
        return function_type(fte->_Next,defining_info);
      }
    } else if ( GlobalFunctionInfo_sp gfi = environment.asOrNull<GlobalFunctionInfo_O>() ) {
      if ( core::cl__equal(fte->_Name,gfi->_Name) ) {
        return core::Cons_O::create(fte->_Type,function_type(fte->_Next,defining_info));
      } else {
        return function_type(fte->_Next,defining_info);
      }
    }
  }
  // All other situations
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return function_type(ee->_Next,defining_info);
}

CL_DEFUN core::T_sp function_ignore(core::T_sp environment, Info_sp defining_info)
{
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return _Nil<core::T_O>();
  } else if ( Function_sp fe = environment.asOrNull<Function_O>() ) {
    if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
      if ( core::cl__equal(fe->_Name,lfi->_Name) ) {
        return _Nil<core::T_O>();
      } else {
        return function_ignore(fe->_Next,defining_info);
      }
    }
  } else if ( Macro_sp me = environment.asOrNull<Macro_O>() ) {
    if ( LocalMacroInfo_sp lmi = defining_info.asOrNull<LocalMacroInfo_O>() ) {
      if ( core::cl__equal(me->_Name,lmi->_Name) ) {
        return _Nil<core::T_O>();
      } else {
        return function_ignore(me->_Next,defining_info);
      }
    }
  } else if ( FunctionIgnore_sp fie = environment.asOrNull<FunctionIgnore_O>() ) {
    if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
      if ( core::cl__equal(fie->_Name,lfi->_Name) ) {
        return environment;
      } else {
        return function_ignore(fie->_Next,defining_info);
      }
    } else if ( GlobalFunctionInfo_sp gfi = defining_info.asOrNull<GlobalFunctionInfo_O>() ) {
      if ( core::cl__equal(fie->_Name,gfi->_Name) ) {
        return environment;
      } else {
        return function_ignore(fie->_Next,defining_info);
      }
    } else if ( LocalMacroInfo_sp lmi = defining_info.asOrNull<LocalMacroInfo_O>() ) {
      if ( core::cl__equal(fie->_Name,lmi->_Name) ) {
        return environment;
      } else {
        return function_ignore(fie->_Next,defining_info);
      }
    } else if ( GlobalMacroInfo_sp gmi = defining_info.asOrNull<GlobalMacroInfo_O>() ) {
      if ( core::cl__equal(fie->_Name,gmi->_Name) ) {
        return environment;
      } else {
        return function_ignore(fie->_Next,defining_info);
      }
    }
  }
  // All other situations
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return function_ignore(ee->_Next,defining_info);
}


CL_DEFUN core::T_sp function_dynamic_extent(core::T_sp environment, Info_sp defining_info )
{
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return _Nil<core::T_O>();
  } else if ( Function_sp fe = environment.asOrNull<Function_O>() ) {
    if ( core::cl__eq(fe->_Name, defining_info->name() ) ) {
      return _Nil<core::T_O>();
    } else {
      return function_dynamic_extent(fe->_Next,defining_info);
    }
  } else if ( FunctionDynamicExtent_sp fde = environment.asOrNull<FunctionDynamicExtent_O>() ) {
    if ( core::cl__equal(fde->_Name,defining_info->name()) ) {
      return environment;
    } else {
      return function_dynamic_extent(fde->_Next,defining_info);
    }
  }
  // All other situations
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return function_dynamic_extent(ee->_Next,defining_info);
}

CL_DEFUN core::T_sp function_inline(core::T_sp environment, Info_sp defining_info )
{
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return defining_info->inline_();
  } else if ( Function_sp fe = environment.asOrNull<Function_O>() ) {
    if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
      if ( core::cl__equal(fe->_Name, lfi->_Name ) ) {
        return _Nil<core::T_O>();
      } else {
        return function_inline(fe->_Next,defining_info);
      }
    } else if ( Inline_sp ie = environment.asOrNull<Inline_O>() ) {
      if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
        if ( core::cl__equal(ie->_Name, lfi->_Name ) ) {
          return environment;
        } else {
          return function_inline(ie->_Next,defining_info);
        }
      } else if ( GlobalFunctionInfo_sp gfi = defining_info.asOrNull<GlobalFunctionInfo_O>() ) {
        return environment;
      } else {
        return function_inline(ie->_Next,defining_info);
      }
    }
  }
  // All other situations
  Entry_sp ee = gc::As<Entry_sp>(environment);
  return function_inline(ee->_Next,defining_info);
}

CL_DEFUN core::T_sp make_info_function(core::T_sp environment, Info_sp defining_info )
{
  if ( LocalFunctionInfo_sp lfi = defining_info.asOrNull<LocalFunctionInfo_O>() ) {
    auto info = LocalFunctionInfo_O::create();
    info->_Name = lfi->_Name;
    info->_Identity = lfi->_Identity;
    info->_Type = core::Cons_O::create(cl::_sym_and,function_type(environment,defining_info));
    /* Cleavir doesn't set _Inline or _Ast - why not? */
    info->_Inline = function_inline(environment,defining_info);
    {
      core::T_sp entry = function_ignore(environment,defining_info);
      if ( entry.nilp() ) info->_Ignore = _Nil<core::T_O>();
      else info->_Ignore = gc::As<Entry_sp>(entry)->ignore();
    }
    info->_Ast = lfi->_Ast;
    {
      info->_DynamicExtent = _Nil<core::T_O>();
      core::T_sp entry = function_dynamic_extent(environment,defining_info);
      if ( entry.nilp() ) info->_DynamicExtent = lfi->_DynamicExtent;
    }
    return info;
  } else if ( GlobalFunctionInfo_sp gfi = defining_info.asOrNull<GlobalFunctionInfo_O>() ) {
    auto info = GlobalFunctionInfo_O::create();
    info->_Name = gfi->_Name;
    info->_Type = core::Cons_O::create(cl::_sym_and,function_type(environment,defining_info));
    {
      Entry_sp entry = function_ignore(environment,defining_info);
      if ( entry.nilp() ) info->_Ignore = _Nil<core::T_O>();
      else info->_Ignore = entry->ignore();
    }
    info->_Inline = function_inline(environment,defining_info);
    info->_Ast = gfi->_Ast;
    info->_CompilerMacro = gfi->_CompilerMacro;
    {
      Entry_sp entry = function_dynamic_extent(environment,defining_info);
      if ( entry.nilp() ) info->_DynamicExtent = gfi->_DynamicExtent;
      else info->_DynamicExtent = _lisp->_true();
    }
    return info;
  } else if ( LocalMacroInfo_sp lmi = defining_info.asOrNull<LocalMacroInfo_O>() ) {
    return defining_info;
  } else if ( GlobalMacroInfo_sp gmi = defining_info.asOrNull<GlobalMacroInfo_O>() ) {
    GlobalMacroInfo_sp info = GlobalMacroInfo_O::create();
    info->_Name = gmi->_Name;
    // Cleavir docs say TODO: Add compiler-macro info here
    info->_Expander = gmi->_Expander;
    return info;
  } else if ( SpecialOperatorInfo_sp soi = defining_info.asOrNull<SpecialOperatorInfo_O>() ) {
    return soi;
  }
//  SIMPLE_ERROR(BF("What do we do now?"));
  return _Nil<core::T_O>();
}

core::T_sp function_info(core::T_sp environment, core::T_sp form)
{
  Info_sp defining_info = defining_function_info(environment,form);
  if ( defining_info.nilp() ) {
    return _Nil<core::T_O>();
  } else {
    return make_info_function(environment,defining_info);
  }
//  SIMPLE_ERROR(BF("What do we do now?"));
  return _Nil<core::T_O>();
}

core::T_sp block_info_impl(core::T_sp environment, core::T_sp symbol, bool& crossesFunction)
{
  if ( Block_sp be = environment.asOrNull<Block_O>() ) {
    if ( core::cl__eq(symbol,be->_Name) ) {
      BlockInfo_sp info = BlockInfo_O::create();
      info->_Name = symbol;
      info->_Identity = be->_Identity;
      return info;
    } else {
      return block_info_impl(be->_Next,symbol,crossesFunction);
    }
  } else if ( Closure_sp fe = environment.asOrNull<Closure_O>()) {
    crossesFunction = true;
    return block_info_impl(fe->_Next,symbol,crossesFunction);
  } else if ( Entry_sp ee = environment.asOrNull<Entry_O>() ) {
    return block_info_impl(ee->_Next,symbol,crossesFunction);
  }
  return _Nil<core::T_O>();
}

/*! *Arguments
- environment : Pass the environment in.
- symbol : The symbol of the block to return from.
* Description
Returns two values, the block info given symbol and the whether or not
the block needs a non-local exit to return to it.
*/
CL_DEFUN core::T_mv block_info(core::T_sp environment, core::T_sp symbol)
{
  bool crossesFunction = false;
  core::T_sp info = block_info_impl(environment,symbol,crossesFunction);
  return Values(info,_lisp->_boolean(crossesFunction));
}


CL_DEFUN core::T_sp tag_info_impl(core::T_sp environment, core::T_sp symbol, bool &nonlocal)
{
  if ( Tag_sp te = environment.asOrNull<Entry_O>() ) {
    if ( core::cl__eq(symbol,te->_Name) ) {
      Tag_sp info = Tag_O::create();
      info->_Name = symbol;
      info->_Identity = te->_Identity;
      return info;
    } else {
      return tag_info_impl(te->_Next,symbol,nonlocal);
    }
  } else if ( Entry_sp ee = environment.asOrNull<Entry_O>() ) {
    return tag_info_impl(ee->_Next,symbol,nonlocal);
  } else if ( Closure_sp ce = environment.asOrNull<Closure_O>() ) {
    nonlocal = true;
    return tag_info_impl(ee->_Next,symbol,nonlocal);
  }
  return _Nil<core::T_O>();
}

CL_DEFUN core::T_mv tag_info(core::T_sp environment, core::T_sp symbol)
{
  bool nonlocal = false;
  core::T_sp info = tag_info_impl(environment,symbol,nonlocal);
  return Values(info,_lisp->_boolean(nonlocal));
}


CL_DEFUN core::T_sp quality_value(core::T_sp environment, core::T_sp name)
{
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    if ( name == cl::_sym_speed ) return ge->speed();
    else if ( name == cl::_sym_debug ) return ge->debug();
    else if ( name == cl::_sym_space ) return ge->space();
    else if ( name == cl::_sym_compilation_speed ) return ge->compilation_speed();
    else if ( name == cl::_sym_safety ) return ge->safety();
    SIMPLE_ERROR(BF("Unknown quality %s") % _rep_(name));
  } else if ( Optimize_sp oe = environment.asOrNull<Optimize_O>() ) {
    if ( core::cl__eq(name, oe->_Quality) ) {
      // It's the quality we want
      return oe->_Value;
    } else {
      // It's not the quality we want - keep looking
      return quality_value(oe->_Next,name);
    }
  } else if ( Entry_sp ee = environment.asOrNull<Entry_O>() ) {
    return quality_value(ee->_Next,name);
  }
  SIMPLE_ERROR(BF("What do we do if we get here?"));
}

CL_DEFUN core::T_sp optimize_info(core::T_sp environment)
{
  OptimizeInfo_sp info = OptimizeInfo_O::create();
  info->_Speed = quality_value(environment,cl::_sym_speed);
  info->_Debug = quality_value(environment,cl::_sym_debug);
  info->_Space = quality_value(environment,cl::_sym_space);
  info->_CompilationSpeed = quality_value(environment,cl::_sym_compilation_speed);
  info->_Safety = quality_value(environment,cl::_sym_safety);
  return info;
}

CL_DEFUN core::T_sp global_environment(core::T_sp environment)
{
  if ( GlobalEnvironment_sp ge = environment.asOrNull<GlobalEnvironment_O>() ) {
    return ge;
  } if ( Entry_sp ee = environment.asOrNull<Entry_O>() ) {
    return global_environment(ee->_Next);
  }
  SIMPLE_ERROR(BF("Illegal environment"));
}


#ifdef PROVIDE_CONVERT_FORM
clc::Ast_sp Info_O::convert_form(ARGS_form_env_rest)
{
  IMPLEMENT_MEF("Implement convert_form for " + lisp_classNameAsString(core::instance_class(this->asSmartPtr())) + " with name " + _rep_(this->name()) );
}
#endif

#ifdef PROVIDE_CONVERT_FORM
clc::Ast_sp SpecialOperatorInfo_O::convert_form(ARGS_form_env_rest)
{
  return clc::convert_special(oCar(form),PASS_form_env_rest);
}
#endif

};


namespace clcenv {

CL_DEFUN Entry_sp clcenv__augment_environment_with_declaration(core::List_sp canonicalized_declaration_specifier,
                                              Entry_sp environment )
{
  core::T_sp head = oCar(canonicalized_declaration_specifier);
  core::List_sp rest = oCdr(canonicalized_declaration_specifier);
  if ( head == cl::_sym_dynamic_extent ) {
    core::T_sp var_or_function = oCar(rest);
    if ( var_or_function.consp() ) {
      return add_function_dynamic_extent(environment,oCadr(var_or_function));
    } else {
      return add_variable_dynamic_extent(environment,oCar(var_or_function));
    }
  } else if ( head == cl::_sym_ftype ) {
    return add_function_type(environment,oCadr(rest),oCar(rest));
  } else if ( head == cl::_sym_ignore || head == cl::_sym_ignorable ) {
    if ( oCar(rest).consp() ) {
      return add_function_ignore(environment,oCadr(oCar(rest)),head);
    } else {
      return add_variable_ignore(environment,oCar(rest),head);
    }
  } else if ( head == cl::_sym_inline || head == cl::_sym_notinline ) {
    return add_inline(environment,oCar(rest),head);
  } else if ( head == cl::_sym_optimize ) {
    core::T_sp quality = oCar(oCar(rest));
    core::T_sp value = oCadr(oCar(rest));
    return add_optimize(environment,quality,value);
  } else if ( head == cl::_sym_special ) {
    // This case is a bit tricky, because if the
    // variable is globally special, nothing should
    // be added to the environment.
    Info_sp info = variable_info(environment,oCar(rest));
    if ( SpecialVariableInfo_sp spinfo = info.asOrNull<SpecialVariableInfo_O>() ) {
      if ( spinfo->_GlobalP.notnilp() ) {
        return environment;
      }
      return add_function_type(environment,oCadr(rest),oCar(rest));
    }
  }
  printf("%s:%d Unable to handle declarations specifier: %s\n", __FILE__, __LINE__, _rep_(canonicalized_declaration_specifier).c_str());
  return environment;
}

// Augment the environment with a list of canonicalized declartion
// specifiers.
CL_DEFUN Entry_sp clcenv__augment_environment_with_declarations(Entry_sp environment, core::List_sp canonicalized_dspecs)
{
  Entry_sp new_env = environment;
  for ( auto spec : canonicalized_dspecs ) {
    new_env = clcenv__augment_environment_with_declaration(spec,new_env);
  }
  return new_env;
}


CL_DEFUN bool clcenv__special_declaration_p(core::List_sp declarations, core::T_sp variable)
{
  for ( auto cur : declarations ) {
    core::List_sp one = oCar(cur);
    core::T_sp head = oCar(one);
    if ( head == cl::_sym_special ) {
      core::T_sp var = oCadr(one);
      ASSERT(oCddr(one).nilp()); // assert that declarations are canonicalized
      if ( var == variable ) return true;
    }
  }
  return false;
}

CL_DEFUN core::T_mv clcenv__variable_is_special_p(core::T_sp variable, core::List_sp declarations, Entry_sp env )
{
  VariableInfo_sp existing_var_info = variable_info(env,variable);
  if ( SpecialVariableInfo_sp svi = existing_var_info.asOrNull<SpecialVariableInfo_O>() ) {
    return Values(_lisp->_true(), svi->_GlobalP );
  } else if ( clcenv__special_declaration_p(declarations,variable) ) {
    return Values(_lisp->_true(), _Nil<core::T_O>() );
  } else {
    return Values(_Nil<core::T_O>(),_Nil<core::T_O>());
  }
}


CL_DEFUN core::T_sp clcenv__declared_type(core::List_sp declarations)
{
  ql::list result;
  result << cl::_sym_and;
  for ( auto cur : declarations ) {
    core::T_sp declaration = oCar(cur);
    if ( oCar(declaration) == cl::_sym_type ) {
      result << oCadr(declaration);
    }
  }
  return result.cons();
}

SYMBOL_EXPORT_SC_(CompPkg,make_lexical_ast);
SYMBOL_EXPORT_SC_(KeywordPkg,name);
    
CL_DEFUN Entry_sp clcenv__augment_environment_with_variable(core::T_sp variable, core::List_sp declarations, Entry_sp env, Entry_sp orig_env )
{
  Entry_sp new_env = env;
  core::T_mv special_p__globally_p = clcenv__variable_is_special_p(variable,declarations,orig_env);
  bool special_p = special_p__globally_p.notnilp();
  bool globally_p = special_p__globally_p.second().notnilp();
  if ( special_p ) {
    if (!globally_p) {
      new_env = add_special_variable(new_env,variable);
    }
  } else {
    core::T_sp var_ast = core::eval::funcall(comp::_sym_make_lexical_ast, kw::_sym_name, variable); // make_LexicalAst(variable);
    new_env = add_lexical_variable(new_env,variable,var_ast);
  }
  core::T_sp type = clcenv__declared_type(declarations);
  if ( !(core::cl__equal(type,core::Cons_O::create(cl::_sym_and))) ) {
    new_env = add_variable_type(new_env,variable,type);
  }
  if ( core::cl__member(cl::_sym_dynamic_extent,declarations,cl::_sym_car->symbolFunction(),cl::_sym_eq->symbolFunction(),_Nil<core::T_O>()).notnilp()) {
    new_env = add_variable_dynamic_extent(new_env,core::Cons_O::createList(cl::_sym_quote,cl::_sym_variable));
  }
  return new_env;
};


}
