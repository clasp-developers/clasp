#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/arguments.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/predicates.h>
#include <clasp/core/clc.h>
#include <clasp/core/clc-ast.h>
#include <clasp/core/clc-env-info.h>
#include <clasp/core/clc-env.h>
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


CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
CL_DEFUN Entry_sp add_lexical_variable(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(LexicalVariable_O, entry, env, symbol, identity );
  return entry;
}

CL_DEFUN Entry_sp add_special_variable(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(SpecialVariable_O, entry, env, symbol );
  return entry;
}

CL_DEFUN Entry_sp add_local_symbol_macro(core::T_sp env, core::T_sp symbol, core::T_sp expansion )
{
  GC_ALLOCATE_VARIADIC(SymbolMacro_O, entry, env, symbol, expansion );
  return entry;
}

CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
CL_DEFUN Entry_sp add_local_function(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(Function_O, entry, env, symbol, identity );
  return entry;
}

CL_DEFUN Entry_sp add_local_macro(core::T_sp env, core::T_sp name, core::T_sp expander )
{
  GC_ALLOCATE_VARIADIC(Macro_O, entry, env, name, expander );
  return entry;
}

CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
CL_DEFUN Entry_sp add_block(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(Block_O, entry, env, symbol, identity );
  return entry;
}

CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
CL_DEFUN Entry_sp add_tag(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(Tag_O, entry, env, symbol, identity );
  return entry;
}

CL_DEFUN Entry_sp add_variable_type(core::T_sp env, core::T_sp symbol, core::T_sp type )
{
  GC_ALLOCATE_VARIADIC(VariableType_O, entry, env, symbol, type );
  return entry;
}

CL_DEFUN Entry_sp add_function_ignore(core::T_sp env, core::T_sp name, core::T_sp ignore )
{
  GC_ALLOCATE_VARIADIC(FunctionIgnore_O, entry, env, name, ignore );
  return entry;
}

CL_DEFUN Entry_sp add_variable_dynamic_extent(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(VariableDynamicExtent_O, entry, env, symbol );
  return entry;
}

CL_DEFUN Entry_sp add_function_dynamic_extent(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(FunctionDynamicExtent_O, entry, env, symbol );
  return entry;
}


CL_DEFUN Entry_sp add_optimize(core::T_sp env, core::T_sp quality, core::T_sp value )
{
  GC_ALLOCATE_VARIADIC(Optimize_O, entry, env, quality, value );
  return entry;
}

CL_DEFUN Entry_sp add_inline(core::T_sp env, core::T_sp name, core::T_sp inlineit )
{
  GC_ALLOCATE_VARIADIC(Inline_O, entry, env, name, inlineit );
  return entry;
}

  
};
