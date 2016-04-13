#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/clc-ast.h>

namespace clc {


// Typical arguments for convert functions
#define PASS_rest dummy_
#define PASS_env_rest env, PASS_rest
#define PASS_form_env_rest form, PASS_env_rest
#define ARGS_form_env_rest core::T_sp form, core::T_sp env, int dummy_

Ast_sp convert(ARGS_form_env_rest);

bool treat_as_special_operator_p(core::T_sp form)
{
  if ( form == cl::_sym_unwind_protect ) return false;
  if ( form == cl::_sym_catch ) return false;
  if ( form == cl::_sym_throw ) return false;
  if ( form == core::_sym_debug_message ) return true;
  return cl__special_operator_p(form);
}


Ast_sp convert_constant( core::T_sp constant, ARGS_form_env_rest )
{
  core::T_sp global_env = clcenv::global_environment(env);
  core::T_sp immediate = convert_constant_to_immediate(constant,global_env,PASS_rest);
  if ( immediate.nilp() ) {
    return clcenv::make_load_time_value_ast(quote(constant),_lisp->_true());
  } else {
    return clcenv::make_immediate_ast(immediate);
  }
}


Ast_sp convert_if(core::T_sp head, ARGS_form_env_rest ) {
  core::T_sp test = oSecond(form);
  core::T_sp then = oThird(form);
  core::T_sp else_ = oFourth(form);
  Ast_sp test_ast = convert(test,PASS_env_rest);
  Ast_sp then_ast = convert(then,PASS_env_rest);
  Ast_sp else_ast = convert(else_,PASS_env_rest);
  return IfAst_O::make_if_ast(test_ast,then_ast,else_ast);
}

Ast_sp convert_symbol_value(core::T_sp form,core::T_sp env)
{
  ql::list l;
  l << cl::_sym_symbolValue;
  l << form;
  return l.cons();
}


Ast_sp convert_atom(core::T_sp form, core::T_sp env) {
  return form;
}

Ast_sp convert_special_operator(core::T_sp head, core::T_sp rest, core::T_sp env) {
  if ( head == cl::_sym_if ) {
    return convert_if(head,rest,env);
  }
  IMPLEMENT_ME();
}

Ast_sp convert_application(core::T_sp form, core::T_sp env) {
  printf("%s:%d   convert_application\n", __FILE__, __LINE__ );
  return form;
};




Ast_sp convert(ARGS_form_env_rest) {
  if ( !form.consp() && !cl__symbolp(form) ) {
    return convert_constant(PASS_form_env_rest);
  } else if (cl__symbolp(form) && cl__constantp(form)) {
    return convert_constant(cl__symbolValue(form),env);
  } else if (cl__symbolp(form)) {
    core::T_sp info = variable_info(env,form);
    return convert_form(form,info,env,PASS_extra);
  } else if (cl__symbolp(oCar(form))) {
    	 // From Cleavir 
         // Even if we are in COMPILE-TIME-TOO mode, at this point, we
	 // do not know whether to evaluate the form at compile time,
	 // simply because it might be a special form that is handled
	 // specially.  So we must wait until we have more
	 // information.
    core::T_sp info = function_info(env,oCar(form));
    return convert_form(form,info,env,PASS_extra);
  }
	 // The form must be a compound form where the CAR is a lambda
	 // expression.  Evaluating such a form might have some
	 // compile-time side effects, so we must check whether we are
	 // in COMPILE-TIME-TOO mode, in which case we must evaluate
	 // the form as well.
  if (comp::_sym_STARcurrent_form_is_top_level_pSTAR->symbolValue().isTrue()
      && comp::_sym_STARcompile_time_tooSTAR->symbolValue().isTrue() ) {
    return eval(form,env,env);
  } else {
    return convert_lambda_call(PASS_form_env_rest);
  }
  SIMPLE_ERROR(BF("Cannot convert code for %s") % _rep_(form));
}



CL_DEFUN Ast_sp comp__generate_ast(ARGS_form_env_rest) {
  core::DynamicScopeManager scope(comp::_sym_STARsubforms_are_top_level_pSTAR,_lisp->_true());
  scope.pushSpecialVariableAndSet(comp::_sym_STARcompile_time_tooSTAR, _Nil<core::T_O>() );
  return convert(PASS_form_env_rest);
}

};
