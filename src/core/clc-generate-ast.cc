#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/arguments.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/predicates.h>
#include <clasp/core/clc.h>
#include <clasp/core/clcenv.h>

namespace clc {

SYMBOL_EXPORT_SC_(ClcPkg,STARcurrent_form_is_top_level_pSTAR);
SYMBOL_EXPORT_SC_(ClcPkg,STARsubforms_are_top_level_pSTAR);
SYMBOL_EXPORT_SC_(ClcPkg,STARcompile_time_tooSTAR);

core::T_sp eval(core::T_sp form, core::T_sp environment)
{
  STUB();
  return _Nil<core::T_O>();
}

core::T_sp convert_lambda_call(core::T_sp form, core::T_sp environment )
{
  STUB();
  return _Nil<core::T_O>();
};

core::T_sp convert_form(core::T_sp form, core::T_sp info, core::T_sp environment)
{
  STUB();
  return _Nil<core::T_O>();
};

core::T_sp convert_constant_to_immediate_clasp(core::T_sp constant, core::T_sp environment)
{
  if (constant.fixnump() ||
      constant.characterp() ||
      constant.single_floatp() ) {
    return core::core__create_tagged_immediate_value_or_nil(constant);
  }
  return _Nil<core::T_O>();
}


core::T_sp convert_constant(core::T_sp constant, core::T_sp environment)
{
  core::T_sp global_env = _Nil<core::T_O>();
  core::T_sp immediate = convert_constant_to_immediate_clasp(constant,global_env);
  if (immediate.nilp()) {
    return LoadTimeValueAst_O::make(core::Cons_O::createList(cl::_sym_quote,constant), _lisp->_true() /*read-only-p*/ );
  } else {
    return ImmediateAst_O::make(immediate);
  }
}


core::T_sp convert_cleavir(core::T_sp form, core::T_sp environment)
{
  if ( !(form).consp() && !core::cl__symbolp(form) ) {
    return convert_constant(form, environment );
  } else if ( core::cl__symbolp(form) && core::cl__constantp(form,_Nil<core::T_O>()) ) {
    return convert_constant(gc::As<core::Symbol_sp>(form)->symbolValue(), environment);
  } else if ( core::cl__symbolp(form) ) {
    core::T_sp info = clcenv::variable_info(environment,form);
    return convert_form(form,info,environment);
  } else if ( core::cl__symbolp(core::oCar(gc::As<core::List_sp>(form))) ) {
    core::T_sp info = clcenv::function_info(environment,form);
    return convert_form(form,info,environment);
  } else {
    if ( _sym_STARcurrent_form_is_top_level_pSTAR->symbolValue().isTrue() &&
         _sym_STARcompile_time_tooSTAR->symbolValue().isTrue() ) {
      clc::eval(form,environment);
      return convert_lambda_call(form,environment);
    }
  }
}

core::T_sp convert_around_cleavir(core::T_sp form, core::T_sp environment)
{
      // This is translated from the original cleavir convert method
  core::DynamicScopeManager scope(_sym_STARcurrent_form_is_top_level_pSTAR,_sym_STARsubforms_are_top_level_pSTAR->symbolValue());
  scope.pushSpecialVariableAndSet(_sym_STARsubforms_are_top_level_pSTAR,_Nil<core::T_O>());
  return convert_cleavir(form,environment);
}

SYMBOL_EXPORT_SC_(ClaspCleavirPkg,STARsimple_environmentSTAR);
SYMBOL_EXPORT_SC_(ClaspCleavirPkg,STARcode_walkerSTAR);

core::T_sp convert_around_clasp(core::T_sp form, core::T_sp environment)
{
  // Set up for code walking
  // This is translated from:
  // (defmethod cleavir-generate-ast:convert :around (form environment (system clasp-64bit))
  core::DynamicScopeManager outer_scope(clasp_cleavir::_sym_STARsimple_environmentSTAR,
                                  clasp_cleavir::_sym_STARsimple_environmentSTAR->symbolValue());
  if ( clasp_cleavir::_sym_STARcode_walkerSTAR->symbolValue().isTrue() ) {
    if ( ext::local_function_form_p(form) ) {
      // (setq *simple-environment* (cons 'core:function-boundary *simple-environment*))
      clasp_cleavir::_sym_STARsimple_environmentSTAR->setf_symbolValue(core::Cons_O::create(core::_sym_function_boundary,clasp_cleavir::_sym_STARsimple_environmentSTAR->symbolValue()));
    }
    core::eval::funcall(clasp_cleavir::_sym_STARcode_walkerSTAR->symbolValue(),
                        form, clasp_cleavir::_sym_STARsimple_environmentSTAR->symbolValue());
  }
  return convert_around_cleavir(form,environment);
}


core::T_sp convert(core::T_sp form, core::T_sp environment)
{
  // This dispatches to convert_around_clasp
  //   which calls convert_around_cleavir
  //   which calls convert_cleavir
  return convert_around_clasp(form,environment);
}

CL_DEFUN core::T_sp generate_ast(core::T_sp form, core::T_sp environment)
{
  core::DynamicScopeManager scope(_sym_STARsubforms_are_top_level_pSTAR, _lisp->_true() );
  scope.pushSpecialVariableAndSet(_sym_STARcompile_time_tooSTAR,_Nil<core::T_O>());
  return convert(form,environment);
}
  

  
};
