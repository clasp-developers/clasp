#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashtable.h>
#include <clasp/core/hashtableEq.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/clc.h>
#include <clasp/core/ql.h>
#include <clasp/core/clcenv.h>


/*! Translating Cleavir into C++

(typep ast 'cleavir-ast:boolean-ast-mixin)
 --> ast->boolean_ast_mixin_p()
*/
namespace clc {


void with_preserved_toplevel_ness()
{
  core::T_sp is = _sym_STARcurrent_form_is_top_level_pSTAR->symbolValue();
  _sym_STARsubforms_are_top_level_pSTAR->setf_symbolValue(is);
}

bool treat_as_special_operator_p(core::T_sp form)
{
  if ( form == cl::_sym_unwind_protect ) return false;
  if ( form == cl::_sym_catch ) return false;
  if ( form == cl::_sym_throw ) return false;
  if ( form == core::_sym_debug_message ) return true;
  if ( form == core::_sym_multiple_value_foreign_call ) return true;
  if ( form == core::_sym_foreign_call ) return true;
  if ( form == core::_sym_foreign_call_pointer ) return true;
  return cl__special_operator_p(form);
}

core::T_mv separate_ordinary_body(core::List_sp body)
{
  core::List_sp declares;
  gc::Nilable<core::String_sp> dummy_doc;
  core::List_sp code;
  core::List_sp specials;
  core::eval::extract_declares_docstring_code_specials(body,declares,false,dummy_doc,code,specials);
  return Values(declares,code);
}

core::List_sp canonicalize_declaration_specifiers(core::List_sp decls)
{
  // cutting a corner here
  printf("%s:%d canonicalize_declaration_specifiers: %s\n",__FILE__, __LINE__, _rep_(decls).c_str());
  return decls;
}





//------------------------------------------------------------
//
// Destructuring forms
//
//

SYMBOL_EXPORT_SC_(ClcPkg,expected_tree_but_found);
SYMBOL_EXPORT_SC_(KeywordPkg,found);


core::T_sp destructure_car(core::T_sp form,bool list_only=false)
{
  core::T_sp val = oCar(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}

core::T_sp destructure_cadr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCadr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}
core::T_sp destructure_caddr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCaddr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}
core::T_sp destructure_cadddr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCadddr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}

core::T_sp destructure_cdr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCdr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}

core::T_sp destructure_cddr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCddr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}

core::T_sp destructure_cdddr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCdddr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}

core::T_sp destructure_cddddr(core::T_sp form, bool list_only = false )
{
  core::T_sp val = oCddddr(form);
  if (!list_only || core::cl__listp(val) ) return val;
  ERROR(clc::_sym_expected_tree_but_found,
        core::Cons_O::createList(kw::_sym_found,val));
}


//------------------------------------------------------------
//
// Handle sequence and progn
//

core::List_sp convert_sequence(core::List_sp forms, ARGS_env_rest)
{
  ql::list l;
  for ( auto cur : forms ) {
    core::T_sp form = oCar(cur);
    l << convert(form,PASS_env_rest);
  }
  return l.cons();
}

Ast_sp process_progn(core::List_sp asts)
{
  if ( asts.nilp() ) {
    return make_LoadTimeValueAst(_Nil<core::T_O>(),_lisp->_true());
  } else {
    return make_PrognAst(asts);
  }
}
};
//------------------------------------------------------------
//
// AST functions

namespace clc {
core::List_sp FunctionAst_O::children() {
  core::List_sp rest = _Nil<core::T_O>();
  core::List_sp lambda_list = gc::As<core::List_sp>(this->lambda_list());
  ql::list entries;
  for ( auto cur : lambda_list ) {
    core::T_sp entry = oCar(cur);
    core::T_sp val;
    if ( core::cl__symbolp(entry) ) {
      entries << _Nil<core::T_O>();
    } else if ( entry.consp() ) {
      if ( core::cl__length(entry) == 2 ) {
        entries << entry;
      } else {
        entries << oCdr(entry);
      }
    } else {
      entries << core::Cons_O::createList(entry);
    }
  }
  return core::Cons_O::create(this->body_ast(),entries.cons());
}


core::T_sp TypeqAst_O::type_specifier_ast() {
  core::T_sp value = this->type_specifier_ast_inner();
  if ( value.nilp() ) {
    value = make_LoadTimeValueAst(core::Cons_O::createList(cl::_sym_quote,this->type_specifier()),_Nil<core::T_O>());
    this->reinitialize(this->type_specifier(),value,this->form_ast());
  }
  return value;
}
                                     

};


//------------------------------------------------------------
//
// Convert specials
//

namespace clc {

Ast_sp convert_special_quote(core::T_sp head, ARGS_form_env_rest ) {
  core::T_sp const_ = destructure_cadr(form);
  return convert_constant(const_,PASS_env_rest);
}

Ast_sp convert_special_block( core::T_sp head, ARGS_form_env_rest ) {
  SETUP_SOURCE_INFO(s);
  core::T_sp name = destructure_cadr(form);
  core::T_sp body = destructure_cddr(form);
  BlockAst_sp ast = make_BlockAst(_Nil<core::T_O>());
  clcenv::Block_sp new_env = clcenv::add_block(env,name,ast);
  ast->setf_body_ast(process_progn(convert_sequence(body,new_env,PASS_rest)));
  return ast;
}

Ast_sp convert_special_if(core::T_sp head, ARGS_form_env_rest ) {
  SETUP_SOURCE_INFO(s);
  core::T_sp test = destructure_cadr(form);
  core::T_sp then = destructure_caddr(form);
  core::T_sp tail = destructure_cdddr(form);
  Ast_sp test_ast = convert(test,PASS_env_rest);
  Ast_sp true_ast = convert(then,PASS_env_rest);
  Ast_sp false_ast;
  if ( tail.nilp() ) {
    false_ast = convert_constant(_Nil<core::T_O>(),PASS_env_rest);
  } else {
    SETUP_SOURCE_INFO(s);
    core::T_sp else_ = destructure_car(tail);
    false_ast = convert(else_,PASS_env_rest);
  }
  if ( test_ast->boolean_ast_mixin_p() ) {
    return make_IfAst(test_ast,true_ast,false_ast);
  }
  return make_IfAst( make_EqAst(test_ast,
                                  convert_constant(_Nil<core::T_O>(),
                                                   PASS_env_rest)),
                      false_ast,
                      true_ast);
}

//------------------------------------------------------------
//
// Convert LET and LET*
//

SYMBOL_EXPORT_SC_(ClcPkg,binding_init_form);
CL_DEFUN core::T_sp clc__binding_init_form(core::T_sp binding)
{
  if ( core::cl__symbolp(binding) || oCdr(binding).nilp() ) {
    return _Nil<core::T_O>();
  } else {
    return oCadr(binding);
  }
}

core::List_sp binding_init_forms(core::List_sp bindings)
{
  return core::cl__mapcar(clc::_sym_binding_init_form, core::Cons_O::create(bindings));
}

SYMBOL_EXPORT_SC_(ClcPkg,binding_var);
CL_DEFUN core::T_sp clc__binding_var(core::T_sp binding)
{
  if ( core::cl__symbolp(binding) ) return binding;
  return oFirst(binding);
}

core::T_sp binding_vars(core::List_sp bindings)
{
  return core::cl__mapcar(clc::_sym_binding_var,core::Cons_O::create(bindings));
}

Ast_sp process_remaining_letSTAR_bindings(core::List_sp bindings,
                                          core::List_sp idspecs,
                                          core::List_sp rdspecs,
                                          core::List_sp forms,
                                          ARGS_env_rest )
{
  if ( bindings.nilp() ) {
    clcenv::Entry_sp new_env = clcenv::augment_environment_with_declarations(env,rdspecs);
    return process_progn(convert_sequence(forms,new_env,PASS_rest));
  } else {
    core::T_sp var = core::oCar(core::oFirst(bindings));
    core::T_sp init_form = core::oCdr(core::oFirst(bindings));
    clcenv::Entry_sp new_env = augment_environment_with_variable(var,core::oFirst(idspecs),env,env);
    Ast_sp value_ast = convert(init_form,PASS_env_rest);
    Ast_sp next_ast = process_remaining_letSTAR_bindings(core::oCdr(bindings),
                                                         core::oCdr(idspecs),
                                                         rdspecs,
                                                         forms,
                                                         new_env,
                                                         PASS_rest);
    return set_or_bind_variable(var,value_ast,next_ast,new_env,PASS_rest);
  }
}
    

Ast_sp convert_special_letSTAR( core::T_sp head, ARGS_form_env_rest) {
  core::T_sp bindings = destructure_cadr(form);
  core::T_sp body = destructure_cddr(form);
  core::T_mv mv = separate_ordinary_body(body);
  core::List_sp declarations = gc::As<core::List_sp>(mv);
  core::List_sp forms = mv.second();
  core::List_sp canonical_dspecs = canonicalize_declaration_specifiers(declarations);
  core::List_sp variables = clc__binding_vars(bindings);
  core::List_sp init_forms = clc__binding_init_forms(bindings);
  core::List_sp variable_lists = core::cl__mapcar(cl::_sym_list, core::Cons_O::create(variables));
  core::T_mv idspecs_rdspecs = itemize_declaration_specifiers(variable_lists,canonical_dspecs);
  core::T_sp idspecs = idspecs_rdspecs;
  core::T_sp rdspecs = idspecs_rdspecs.second();
  core::List_sp cons_variables = core::cl__mapcar(cl::_sym_cons,core::Cons_O::createList(variables,init_forms));
  return process_remaining_letSTAR_bindings(cons_variables,
                                            idspecs,
                                            rdspecs,
                                            forms,
                                            PASS_env_rest);
}

Ast_sp convert_special_progn( core::T_sp head, ARGS_form_env_rest ) {
  with_preserved_toplevel_ness();
  SETUP_SOURCE_INFO(s);
  core::T_sp forms = destructure_cdr(form);
  return process_progn(convert_sequence(forms,PASS_env_rest));
}

Ast_sp convert_special(core::T_sp head, ARGS_form_env_rest ) {
  if ( head == cl::_sym_quote ) {
    return convert_special_quote( head, PASS_form_env_rest);
  } else if ( head == cl::_sym_block ) {
    return convert_special_block( head, PASS_form_env_rest );
  } else if ( head == cl::_sym_progn ) {
    return convert_special_progn( head, PASS_form_env_rest);
  } else if ( head == cl::_sym_if ) {
    return convert_special_if( head, PASS_form_env_rest);
  }
  IMPLEMENT_MEF("Implement convert_special for a new head");
}

};
//------------------------------------------------------------
//
// 
/*! If the constant is an immediate value then create numerical
representation of it.  Otherwise return NIL */

namespace clc {
core::T_sp convert_constant_to_immediate(core::T_sp constant, ARGS_env_rest )
{
  return core::core__create_tagged_immediate_value_or_nil(constant);
}



Ast_sp convert_constant( ARGS_form_env_rest )
{
  core::T_sp global_env = clcenv::global_environment(env);
  core::T_sp immediate = convert_constant_to_immediate(form,global_env,PASS_rest);
  if ( immediate.nilp() ) {
    return make_LoadTimeValueAst(core::Cons_O::createList(cl::_sym_quote,form),_lisp->_true());
  } else {
    return make_ImmediateAst(immediate);
  }
}



Ast_sp convert_symbol_value(core::T_sp form,core::T_sp env)
{
  IMPLEMENT_ME();
}


Ast_sp convert_atom(ARGS_form_env_rest) {
  return form;
}


Ast_sp convert_application(core::T_sp form, core::T_sp env) {
  printf("%s:%d   convert_application\n", __FILE__, __LINE__ );
  return form;
};


Ast_sp eval(core::T_sp form, core::T_sp env, core::T_sp env2 )
{
  IMPLEMENT_ME();
}

Ast_sp convert_lambda_call(ARGS_form_env_rest)
{
  IMPLEMENT_ME();
};


Ast_sp convert_inner(ARGS_form_env_rest) {
  if ( !form.consp() && !cl__symbolp(form) ) {
    return convert_constant(PASS_form_env_rest);
  } else if (cl__symbolp(form) && cl__constantp(form)) {
    return convert_constant(cl__symbol_value(form),PASS_env_rest);
  } else if (cl__symbolp(form)) {
    clcenv::Info_sp info = clcenv::variable_info(env,form);
    return info->convert_form(form,PASS_env_rest);
  } else if (cl__symbolp(oCar(form))) {
    	 // From Cleavir 
         // Even if we are in COMPILE-TIME-TOO mode, at this point, we
	 // do not know whether to evaluate the form at compile time,
	 // simply because it might be a special form that is handled
	 // specially.  So we must wait until we have more
	 // information.
    clcenv::Info_sp info = clcenv::function_info(env,oCar(form));
    return info->convert_form(form,PASS_env_rest);
  }
	 // The form must be a compound form where the CAR is a lambda
	 // expression.  Evaluating such a form might have some
	 // compile-time side effects, so we must check whether we are
	 // in COMPILE-TIME-TOO mode, in which case we must evaluate
	 // the form as well.
  if (_sym_STARcurrent_form_is_top_level_pSTAR->symbolValue().isTrue()
      && _sym_STARcompile_time_tooSTAR->symbolValue().isTrue() ) {
    return eval(form,env,env);
  } else {
    return convert_lambda_call(PASS_form_env_rest);
  }
  SIMPLE_ERROR(BF("Cannot convert code for %s") % _rep_(form));
}

Ast_sp convert(ARGS_form_env_rest) {
  core::DynamicScopeManager scope(_sym_STARcurrent_form_is_top_level_pSTAR,
                            _sym_STARsubforms_are_top_level_pSTAR->symbolValue());
  scope.bind(_sym_STARsubforms_are_top_level_pSTAR,_Nil<core::T_O>());
  return convert_inner(PASS_form_env_rest);
}

SYMBOL_EXPORT_SC_(ClcPkg,STARcurrent_form_is_top_level_pSTAR);
SYMBOL_EXPORT_SC_(ClcPkg,STARcompile_time_tooSTAR);
SYMBOL_EXPORT_SC_(ClcPkg,STARsubforms_are_top_level_pSTAR);

Ast_sp generate_ast(ARGS_form_env_rest) {
  core::DynamicScopeManager scope(_sym_STARsubforms_are_top_level_pSTAR,_lisp->_true());
  scope.pushSpecialVariableAndSet(_sym_STARcompile_time_tooSTAR, _Nil<core::T_O>() );
  return convert(PASS_form_env_rest);
}

CL_DEFUN Ast_sp clc__generate_ast(core::T_sp form, core::T_sp env)
{
  int dummy_ = 0;
  return generate_ast(form,env,PASS_rest);
};

};


//----------------------------------------------------------------------
//
// Graphviz functions
namespace clc {
SYMBOL_EXPORT_SC_(ClcPkg,STARgraphviz_tableSTAR);

core::T_sp Ast_O::id() {
  core::T_sp id = gc::As<core::HashTable_sp>(_sym_STARgraphviz_tableSTAR->symbolValue())->gethash(this->asSmartPtr());
  return id;
}

std::string Ast_O::id_string() {
  core::Symbol_sp id = gc::As<core::Symbol_sp>(this->id());
  return _rep_(id->symbolName());
}

void Ast_O::draw_ast(core::T_sp stream)
{
  core::write_bf_stream(BF("  %s [label = \"%s\"];\n")
                        % this->id_string() % this->label(), stream );
}

void stream_draw_ast(Ast_sp ast, core::T_sp stream)
{
  core::HashTable_sp table = gc::As<core::HashTable_sp>(_sym_STARgraphviz_tableSTAR->symbolValue());
  core::T_sp id = table->gethash(ast);
  if (id.nilp()) {
    core::T_sp idsym = core::cl__gensym(_Nil<core::T_O>());
    table->setf_gethash(ast,idsym);
    core::write_bf_stream(BF("  %s [shape = box];\n") % ast->id_string(), stream);
    ast->draw_ast(stream);
    int i=0;
    for ( auto cur : ast->children() ) {
      Ast_sp child = gc::As<Ast_sp>(oCar(cur));
      stream_draw_ast(child,stream);
      core::write_bf_stream(BF("  %s -> %s [label = \"%d\"];\n")
                            % ast->id_string() % child->id_string() % i, stream);
      ++i;
    }
  }
}
CL_DEFUN void draw_ast(Ast_sp ast, core::T_sp filename )
{
  core::T_sp stream = clasp_openWrite(filename);
  if (stream.nilp()) {
    FILE_ERROR(filename);
  }
  try {
    core::write_bf_stream(BF("digraph G {\n  ordering = out;\n"), stream);
    core::DynamicScopeManager scope(_sym_STARgraphviz_tableSTAR,core::HashTableEq_O::create_default());
    stream_draw_ast(ast,stream);
    core::write_bf_stream(BF("}\n"),stream);
    core::cl__close(stream);
  } catch (...) {
    core::write_bf_stream(BF("}\n"),stream);
    core::cl__close(stream);
  }
}


};
