
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/generate.h>
#include <clasp/core/primitives.h>
#include <clasp/core/ql.h>

namespace core {

T_sp generate(T_sp form, T_sp environment );

bool treat_as_special_operator_p(T_sp form)
{
  if ( form == cl::_sym_unwind_protect ) return false;
  if ( form == cl::_sym_catch ) return false;
  if ( form == cl::_sym_throw ) return false;
  if ( form == core::_sym_debug_message ) return true;
  if ( form == core::_sym_intrinsic_call ) return true;
  return cl__special_operator_p(form);
}

T_sp generate_if(T_sp head, T_sp args, T_sp environment) {
  T_sp expr = oFirst(args);
  T_sp then = oSecond(args);
  T_sp else_ = oThird(args);
  ql::list result = ql::list()
    << cl::_sym_if
    << generate(expr,environment)
    << generate(then,environment)
    << generate(else_,environment);
  return result.cons();
}

T_sp generate_symbol_value(T_sp form,T_sp env)
{
  ql::list l;
  l << cl::_sym_symbolValue;
  l << form;
  return l.cons();
}


T_sp generate_atom(T_sp form, T_sp env) {
  return form;
}

T_sp generate_special_operator(T_sp head, T_sp rest, T_sp env) {
  if ( head == cl::_sym_if ) {
    return generate_if(head,rest,env);
  }
  IMPLEMENT_ME();
}

T_sp generate_application(T_sp form, T_sp env) {
  printf("%s:%d   generate_application\n", __FILE__, __LINE__ );
  return form;
};

T_sp generate(T_sp form, T_sp env ) {
  if ( cl__atom(form) ) {
    if ( cl__symbolp(form) ) {
      return generate_symbol_value(form,env);
    } else {
      return generate_atom(form,env);
    }
  }
  List_sp form_list = gctools::reinterpret_cast_smart_ptr<List_V>(form);
  T_sp head = oCar(form_list);
  T_sp rest = oCdr(form_list);
  if ( treat_as_special_operator_p(head) ) {
    return generate_special_operator(head,rest,env);
  } else if (head.consp() && oCar(head) == cl::_sym_lambda ) {
    ql::list l;
    l << cl::_sym_funcall
      << head;
    l.dot(rest);
    List_sp temp = l.cons();
    return generate(temp,env);
  } else if ( head.notnilp() && cl__symbolp(head) ) {
    return generate_application(form,env);
  }
  SIMPLE_ERROR(BF("Cannot generate code for %s") % _rep_(form));
}



CL_DEFUN T_sp core__generate(T_sp form, T_sp env) {
  return generate(form,env);
}

};

