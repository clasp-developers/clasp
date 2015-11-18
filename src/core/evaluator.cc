/*
    File: evaluator.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#define DEBUG_LEVEL_FULL
//#include "core/foundation.h"
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/iterator.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/specialForm.h>
//#i n c l u d e "setfExpander.h"
#include <clasp/core/environment.h>
#include <clasp/core/executables.h>
#include <clasp/core/designators.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/predicates.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/predicates.h>
#include <clasp/core/lisp.h>
#include <clasp/core/backquote.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/conditions.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/primitives.h>
//#include "debugger.h"
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

namespace core {
namespace eval {
T_mv t1Evaluate(T_sp exp, T_sp environment);
};
};

namespace core {

//void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<Str_sp> &docstring, List_sp &code);

List_sp separateTopLevelForms(List_sp accumulated, T_sp possibleForms) {
  if (Cons_sp cpf = possibleForms.asOrNull<Cons_O>()) {
    if (gc::As<Symbol_sp>(oCar(cpf)) == cl::_sym_progn) {
      for (auto cur : (List_sp)oCdr(cpf)) {
        accumulated = separateTopLevelForms(accumulated, oCar(cur));
      }
      return accumulated;
    }
  }
  accumulated = Cons_O::create(possibleForms, accumulated);
  return accumulated;
}

#define ARGS_core_compileFormAndEvalWithEnv "(form &optional env stepping compiler-env-p (execute t))"
#define DECL_core_compileFormAndEvalWithEnv ""
#define DOCS_core_compileFormAndEvalWithEnv "compileFormAndEvalWithEnv"
T_mv core_compileFormAndEvalWithEnv(T_sp form, T_sp env, T_sp stepping, T_sp compiler_env_p, T_sp execute) {
  T_mv result = eval::funcall(comp::_sym_STARimplicit_compile_hookSTAR->symbolValue(), form, env);
  return result;
};

#define ARGS_af_interpreter_lookup_variable "(symbol env)"
#define DECL_af_interpreter_lookup_variable ""
#define DOCS_af_interpreter_lookup_variable "environment_lookup_variable"
T_sp af_interpreter_lookup_variable(Symbol_sp sym, T_sp env) {
  if (env.notnilp()) {
    int depth, index;
    Environment_O::ValueKind valueKind;
    T_sp value;
    bool found = Environment_O::clasp_findValue(env, sym, depth, index, valueKind, value);
    if (found) {
      switch (valueKind) {
      case Environment_O::lexicalValue:
        return value;
      case Environment_O::specialValue:
        return sym->symbolValue();
      default:
        // do nothing;
        break;
      }
    }
  }
  if (sym->specialP() || sym->boundP()) {
    return sym->symbolValue();
  }
  SIMPLE_ERROR(BF("Could not find variable %s in lexical/global environment") % _rep_(sym));
};

#define ARGS_af_interpreter_lookup_function "(symbol env)"
#define DECL_af_interpreter_lookup_function ""
#define DOCS_af_interpreter_lookup_function "environment_lookup_function return the function or UNBOUND"
T_sp af_interpreter_lookup_function(Symbol_sp name, T_sp env) {
  _G();
  if (env.notnilp()) {
    Function_sp fn;
    int depth;
    int index;
    if (Environment_O::clasp_findFunction(env, name, depth, index, fn)) {
      return fn;
    }
  }
  if (name->fboundp())
    return name->symbolFunction();
  return _Nil<T_O>(); // never let unbound propagate
};

#define ARGS_af_interpreter_lookup_setf_function "(symbol env)"
#define DECL_af_interpreter_lookup_setf_function ""
#define DOCS_af_interpreter_lookup_setf_function "environment_lookup_setf_function"
T_sp af_interpreter_lookup_setf_function(List_sp setf_name, T_sp env) {
  _G();
  Symbol_sp name = gc::As<Symbol_sp>(oCadr(setf_name));
  if (env.notnilp()) {
    Function_sp fn;
    int depth;
    int index;
    // TODO: This may not work properly - it looks like it will find regular functions
    if (Environment_O::clasp_findFunction(env, name, depth, index, fn))
      return fn;
  }
  if (name->setf_fboundp())
    return name->getSetfFdefinition();
  return _Nil<T_O>();
};

#define ARGS_core_lookup_symbol_macro "(symbol &optional env)"
#define DECL_core_lookup_symbol_macro ""
#define DOCS_core_lookup_symbol_macro "environment_lookup_symbol_macro_definition"
T_sp core_lookup_symbol_macro(Symbol_sp sym, T_sp env) {
  _G();
  if (sym.nilp())
    return _Nil<T_O>();
  if (env.notnilp()) {
    int depth = 0;
    int level = 0;
    bool shadowed = false;
    Function_sp macro;
    bool found = Environment_O::clasp_findSymbolMacro(env, sym, depth, level, shadowed, macro);
    if (found)
      return macro;
  }
  SYMBOL_SC_(CorePkg, symbolMacro);
  T_sp fn = _Nil<T_O>();
  T_mv result = af_get_sysprop(sym, core::_sym_symbolMacro);
  if (gc::As<T_sp>(result.valueGet(1)).notnilp()) {
    fn = gc::As<Function_sp>(result);
  }
  return fn;
};

#define ARGS_af_interpreter_lookup_macro "(symbol env)"
#define DECL_af_interpreter_lookup_macro ""
#define DOCS_af_interpreter_lookup_macro "environment_lookup_macro_definition"
T_sp af_interpreter_lookup_macro(Symbol_sp sym, T_sp env) {
  if (sym.nilp())
    return _Nil<T_O>();
  if (core_lexicalFunction(sym, env).notnilp())
    return _Nil<T_O>();
  int depth = 0;
  int level = 0;
  Function_sp macro;
  bool found = Environment_O::clasp_findMacro(env, sym, depth, level, macro);
  if (found)
    return macro;
  if (sym->fboundp()) {
    if (Function_sp fn = sym->symbolFunction().asOrNull<Function_O>()) {
      if (fn->macroP())
        return fn;
    }
  }
  return _Nil<T_O>();
};

namespace interpret {
T_mv interpreter_cond(List_sp args, T_sp environment) {
  _G();
  for (auto cur : args) {
    T_sp cond;
    List_sp condProgn = oCar(cur);
    cond = eval::evaluate(oCar(condProgn), environment);
    if (cond.isTrue()) {
      List_sp code = oCdr(condProgn);
      if (code.notnilp()) {
        return eval::sp_progn(code, environment);
      }
      return (Values(cond));
    }
  }
  return Values(_Nil<T_O>());
}

SYMBOL_EXPORT_SC_(ClPkg, case);
T_mv interpreter_case(List_sp args, T_sp environment) {
  _G();
  T_sp keyform = oCar(args);
  List_sp clauses = oCdr(args);
  T_sp test_key = eval::evaluate(keyform, environment);
  LOG(BF("Evaluated test_key = %s\n") % _rep_(test_key));
  for (auto cur : clauses) {
    T_sp oclause = oCar(cur);
    if (cl_consp(oclause)) {
      List_sp clause = oclause;
      T_sp keys = oCar(clause);
      List_sp forms = oCdr(clause);
      SYMBOL_EXPORT_SC_(ClPkg, otherwise);
      if (keys == cl::_sym_otherwise || keys == _lisp->_true()) {
        if (oCdr(cur).notnilp()) {
          SIMPLE_ERROR(BF("otherwise-clause must be the last clause of case - it is not"));
        }
        return eval::sp_progn(forms, environment);
      } else if (cl_atom(keys)) {
        if (cl_eql(keys, test_key)) {
          return eval::sp_progn(forms, environment);
        }
      } else if (keys.consp()) {
        List_sp lkeys = keys;
        for (auto kcur : lkeys) {
          if (cl_eql(oCar(kcur), test_key)) {
            return eval::sp_progn(forms, environment);
          }
        }
      }
    } else {
      SIMPLE_ERROR(BF("Bad case clause: %s") % _rep_(oclause));
    }
  }
  return (Values(_Nil<T_O>()));
}

void setq_symbol_value(Symbol_sp symbol, T_sp value, T_sp environment) {
  if (symbol->specialP() || Environment_O::clasp_lexicalSpecialP(environment, symbol)) {
    symbol->setf_symbolValue(value);
    return;
  } else {
    bool updated = af_updateValue(environment, symbol, value);
    if (!updated) {
      symbol->setf_symbolValue(value);
    }
  }
}

SYMBOL_EXPORT_SC_(ClPkg, multipleValueSetq);
T_sp interpreter_multipleValueSetq(List_sp args, T_sp environment) {
  _G();
  List_sp lcur = oCar(args);
  T_sp form = oCadr(args);
  VectorObjects_sp values(VectorObjects_O::create());
  T_mv result = eval::evaluate(form, environment);
  multipleValuesSaveToVector(result, values);
  Cons_sp skipFirst = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  Cons_sp add = skipFirst;
  // Assemble a Cons for sp_setq
  size_t valuesLength = cl_length(values);
  int i = 0;
  for (auto cur : lcur) {
    Symbol_sp symbol = gc::As<Symbol_sp>(oCar(cur));
    T_sp value = i < valuesLength ? values->operator[](i) : _Nil<T_O>();
    Cons_sp one = Cons_O::create(symbol, _Nil<T_O>());
    add->setCdr(one);
    add = one;
    Cons_sp quotedValue = Cons_O::createList(cl::_sym_quote, value);
    Cons_sp two = Cons_O::create(quotedValue, _Nil<T_O>());
    add->setCdr(two);
    add = two;
    ++i;
  }
  eval::sp_setq(oCdr(skipFirst), environment);
  return (values->operator[](0));
}

SYMBOL_EXPORT_SC_(ClPkg, prog1);
T_mv interpreter_prog1(List_sp args, T_sp environment) {
  _G();
  T_sp firstForm = oCar(args);
  List_sp forms = oCdr(args);
  T_sp result = eval::evaluate(firstForm, environment);
  eval::sp_progn(forms, environment);
  return (Values(result));
}
};

namespace eval {
/*! LAMBDA expression processing.
	  Process ( [[declaration* | documentation]] form* ) and
	  aggregate the declarations into a list ( (decl-1) (decl-2) ... )
	  and identify the last documentation string seen.
	  Return the list of declarations, the documentation string and the rest
	  of the forms in CODE.  Identify the local special declarations and
	return them in SPECIALS but leave them in the DECLARES list */
void extract_declares_docstring_code_specials(List_sp inputBody, List_sp &declares, bool expectDocString, gc::Nilable<Str_sp> &documentation, List_sp &code, List_sp &specials) {
  _G();
  List_sp body = inputBody;
  declares = _Nil<T_O>();
  specials = _Nil<T_O>();
  for (; body.notnilp(); body = oCdr(body)) {
    if (!cl_listp(body)) {
      SIMPLE_ERROR(BF("Bad input to processDeclares: %s") % _rep_(inputBody));
    }
    T_sp form = oCar(body);
    // If we are expecting docstring and we hit a string, then we hit a possible docstring
    if (expectDocString && af_stringP(form)) {
      // If there is something following the current element then treat it as a docstring
      if (oCdr(body).notnilp()) {
        // Here we are in undefined behavior CLHS 3.4.11
        // we may be replacing previous docstrings
        documentation = gc::As<Str_sp>(form);
        continue;
      } else {
        // Nothing follows so the current form is a form
        // and stop looking for docstrings or declares
        break;
      }
    }
    if (cl_atom(form) || oCar(form) != cl::_sym_declare) {
      break;
    }
    List_sp cform = form;
    for (cform = oCdr(form); cform.notnilp();) {
      List_sp sentence = oCar(cform);
      cform = oCdr(cform);
      declares = Cons_O::create(sentence, declares);
      T_sp sentenceHead = oCar(sentence);
      sentence = oCdr(sentence);
      if (sentenceHead == cl::_sym_special) {
        while (sentence.notnilp()) {
          T_sp v = oCar(sentence);
          sentence = oCdr(sentence);
          if (!cl_symbolp(v)) {
            SIMPLE_ERROR(BF("Illegal object[%s] in declare special") % _rep_(v));
          }
          specials = Cons_O::create(v, specials);
        }
      }
    }
  }
  code = body;
  declares = cl_nreverse(declares);
}

#if 0
#define ARGS_af_extractDeclaresDocstringCode "(body &key (expect-docstring t))"
#define DECL_af_extractDeclaresDocstringCode ""
#define DOCS_af_extractDeclaresDocstringCode "extractDeclaresDocstringCode"
	T_mv af_extractDeclaresDocstringCode(List_sp body, T_sp expectDocStr)
	{_G();
	    IMPLEMENT_MEF(BF("Switch to process-declarations"));
	    List_sp declares;
	    Str_sp docstr;
	    List_sp code;
	    List_sp specials;
	    extract_declares_docstring_code_specials(body,declares,expectDocStr.isTrue(),docstr,code,specials);
	    return Values(declares,docstr,code,specials);
	};
#endif

void extract_declares_code(List_sp args, List_sp &declares, List_sp &code) {
  _G();
  gc::Nilable<Str_sp> dummy_docstring;
  List_sp specials;
  IMPLEMENT_MEF(BF("Check who is using this and why they aren't calling extract_declares_docstring_code_specials directly"));
  extract_declares_docstring_code_specials(args, declares, false, dummy_docstring, code, specials);
}

void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<Str_sp> &docstring, List_sp &code) {
  LOG(BF("Parsing lambda body: %s") % body->__repr__());
  List_sp specials;
  extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
}

#define ARGS_core_coerce_to_function "(arg)"
#define DECL_core_coerce_to_function ""
#define DOCS_core_coerce_to_function "coerce_to_function"
Function_sp core_coerce_to_function(T_sp arg) {
  if (Function_sp fnobj = arg.asOrNull<Function_O>()) {
    return fnobj;
  } else if (Symbol_sp sym = arg.asOrNull<Symbol_O>()) {
    if (!sym->fboundp())
      SIMPLE_ERROR(BF("Function value for %s is unbound") % _rep_(sym));
    return sym->symbolFunction();
  } else if (Cons_sp carg = arg.asOrNull<Cons_O>()) {
    T_sp head = oCar(carg);
    if (head == cl::_sym_setf) {
      Symbol_sp sym = oCadr(carg).as<Symbol_O>();
      if (!sym->setf_fboundp()) {
        SIMPLE_ERROR(BF("SETF function value for %s is unbound") % _rep_(sym));
      }
      return sym->getSetfFdefinition();
    } else if (head == cl::_sym_lambda) {
      T_sp olambdaList = oCadr(arg);
      List_sp body = oCdr(oCdr(arg));
      List_sp declares;
      gc::Nilable<Str_sp> docstring;
      List_sp code;
      parse_lambda_body(body, declares, docstring, code);
      LambdaListHandler_sp llh = LambdaListHandler_O::create(olambdaList, declares, cl::_sym_function);
      gctools::tagged_pointer<InterpretedClosure> ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(cl::_sym_lambda, kw::_sym_function, llh, declares, docstring, _Nil<T_O>(), code, SOURCE_POS_INFO_FIELDS(_Nil<T_O>()));
      Function_sp proc = Function_O::make(ic);
      return proc;
#if 0
      T_sp fn;
      if ( comp::_sym_compileInEnv->fboundp() ) {
        fn = eval::funcall(comp::_sym_compileInEnv
                           , _Nil<T_O>()
                           , carg
                           , _Nil<T_O>() ).as<Function_O>();
      } else {
        SIMPLE_ERROR(BF("You cannot coerce-to-function a lambda until the compiler is in place"));
      }
#endif
    }
  }
  SIMPLE_ERROR(BF("Illegal function designator %s") % _rep_(arg));
};

/*
  __BEGIN_DOC(candoScript.specialForm.block,block)
  \scriptCmdRet{block}{command1 command2 ...}{lastObject}

  Evaluates each command and returns the value \scriptArg{lastObject} from evaluating the last command. This is what you use to write blocks of code.
  __END_DOC
*/
T_mv sp_progn(List_sp args, T_sp environment) {
  _G();
  return eval::evaluateListReturnLast(args, environment);
}

T_mv sp_loadTimeValue(List_sp args, T_sp environment) {
  _G();
  T_sp form = oCar(args);
  return eval::evaluate(form, _Nil<T_O>());
}

T_mv sp_progv(List_sp args, T_sp environment) {
  _G();
  List_sp symbols = eval::evaluate(oCar(args), environment);
  List_sp values = eval::evaluate(oCadr(args), environment);
  DynamicScopeManager manager;
  for (; symbols.notnilp(); symbols = oCdr(symbols), values = oCdr(values)) {
    Symbol_sp symbol = gc::As<Symbol_sp>(oCar(symbols));
    T_sp value = oCar(values);
    manager.pushSpecialVariableAndSet(symbol, value);
  }
  List_sp forms = oCddr(args);
  return sp_progn(forms, environment);
}

T_mv sp_debug_message(List_sp args, T_sp env) {
  _G();
  Str_sp msg = gc::As<Str_sp>(oCar(args));
  printf("+++DEBUG-MESSAGE[%s]\n", msg->c_str());
  return (Values(_Nil<T_O>()));
}

T_mv sp_evalWhen(List_sp args, T_sp environment) {
  _G();
  SYMBOL_SC_(KeywordPkg, execute);
  SYMBOL_SC_(KeywordPkg, load_toplevel);
  List_sp situations = oCar(args);
  List_sp body = oCdr(args);
  bool execute = false;
  if (cl_member(kw::_sym_execute, situations, _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>()).isTrue()) {
    execute = true;
  }
  if (execute) {
    return sp_progn(body, environment);
  }
  return (Values(_Nil<T_O>()));
}

T_mv sp_the(List_sp args, T_sp env) {
  _G();
  T_mv val = eval::evaluate(oCadr(args), env);
  return (val);
}

T_mv sp_specialVar(List_sp args, T_sp env) {
  _G();
  Symbol_sp sym = gc::As<Symbol_sp>(oCar(args));
  return Values(sym->symbolValue());
}

T_mv sp_lexicalVar(List_sp args, T_sp env) {
  _G();
  int depth = unbox_fixnum(gc::As<Fixnum_sp>(oCadr(args)));
  int index = unbox_fixnum(gc::As<Fixnum_sp>(oCddr(args)));
  return Values(Environment_O::clasp_lookupValue(env, depth, index));
}

T_mv sp_locally(List_sp args, T_sp env) {
  _G();
  List_sp declares;
  gc::Nilable<Str_sp> docstring;
  List_sp code;
  List_sp specials;
  extract_declares_docstring_code_specials(args, declares, false, docstring, code, specials);
  ValueEnvironment_sp le = ValueEnvironment_O::createForLocallySpecialEntries(specials, env);
  // ignore everything else for now
  return eval::sp_progn(code, le);
}

#define DOCS_cl_eval "eval"
#define LOCK_cl_eval 1
#define ARGS_cl_eval "(form)"
#define DECL_cl_eval ""
T_mv cl_eval(T_sp form) {
  if (core::_sym_STARuseInterpreterForEvalSTAR->symbolValue().isTrue()) {
    return eval::evaluate(form, _Nil<T_O>());
  } else {
    return eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), form, _Nil<T_O>());
  }
};

#define when_load_p(s) ((s)&FLAG_LOAD)
#define when_compile_p(s) ((s)&FLAG_COMPILE)
#define when_execute_p(s) ((s)&FLAG_EXECUTE)

#define DOCS_sp_eval_when "eval_when"
#define LOCK_sp_eval_when 1
#define ARGS_sp_eval_when "(situation &rest body)"
#define DECL_sp_eval_when ""
T_mv sp_eval_when(List_sp args, T_sp env) {
  _G();
  List_sp situation_list = oCar(args);
  List_sp body = oCdr(args);
  uint situation = 0;
  for (auto cursit : situation_list) {
    Symbol_sp s = gc::As<Symbol_sp>(oCar(cursit));
    if (s == kw::_sym_compile_toplevel)
      situation |= FLAG_COMPILE;
    else if (s == cl::_sym_compile)
      situation |= FLAG_COMPILE;
    else if (s == kw::_sym_load_toplevel)
      situation |= FLAG_LOAD;
    else if (s == cl::_sym_load)
      situation |= FLAG_LOAD;
    else if (s == kw::_sym_execute)
      situation |= FLAG_EXECUTE;
    else if (s == cl::_sym_eval)
      situation |= FLAG_EXECUTE;
    else {
      SIMPLE_ERROR(BF("Illegal situation[%s] for eval-when - only :compile-toplevel, :load-toplevel, :execute, compile, load or eval allowed") % _rep_(s));
    }
  }
  uint mode = _lisp->mode();
  if (mode == FLAG_EXECUTE) {
    if (!when_execute_p(situation))
      body = _Nil<T_O>();
#if 0
	    } else if (c_env->lexical_level) {
                if (!when_execute_p(situation))
		    body = _Nil<T_O>();
#endif
  } else if (mode == FLAG_LOAD) {
    if (!when_load_p(situation)) {
      body = _Nil<T_O>();
    }
#if 0
                if (when_compile_p(situation)) {
		    env->c_env->mode = FLAG_COMPILE;
		    execute_each_form(env, body);
		    env->c_env->mode = FLAG_LOAD;
		    if (!when_load_p(situation))
			body = _Nil<T_O>();
                } else
		    if (when_load_p(situation)) {
			env->c_env->mode = FLAG_ONLY_LOAD;
			flags = compile_toplevel_body(env, body, flags);
			env->c_env->mode = FLAG_LOAD;
			return flags;
		    } else {
			body = _Nil<T_O>();
		    }
#endif
  } else if (mode == FLAG_ONLY_LOAD) {
    if (!when_load_p(situation))
      body = _Nil<T_O>();
  } else { /* FLAG_COMPILE */
    SIMPLE_ERROR(BF("I don't have a compiler yet"));
#if 0
                if (when_execute_p(situation) || when_compile_p(situation)) {
		    execute_each_form(env, body);
                }
		body = _Nil<T_O>();
#endif
  }
  return eval::sp_progn(body, env);
//	    return eval::evaluateListReturnLast(body,env,_lisp);
#if 0
	    return compile_toplevel_body(env, body, flags);
#endif
};

#define DOCS_sp_step "step is implemented as a special"
#define ARGS_sp_step "(form)"
#define DECL_sp_step ""
T_mv sp_step(List_sp args, T_sp env) {
  _G();
  IMPLEMENT_ME();
};

#define DOCS_sp_tagbody "tagbody special form - see CLHS"
T_mv sp_tagbody(List_sp args, T_sp env) {
  _G();
  TagbodyEnvironment_sp tagbodyEnv = TagbodyEnvironment_O::make(env);
  //
  // Find all the tags and tell the TagbodyEnvironment where they are in the list of forms.
  //
  for (auto cur : args) {
    T_sp tagOrForm = oCar(cur);
    if (cl_symbolp(tagOrForm)) {
      Symbol_sp tag = gc::As<Symbol_sp>(tagOrForm);
      // The tag is associated with its position in list of forms
      tagbodyEnv->addTag(tag, cur);
    }
  }
  LOG(BF("sp_tagbody has extended the environment to: %s") % tagbodyEnv->__repr__());
  T_sp tagbodyId = gc::As<TagbodyFrame_sp>(Environment_O::clasp_getActivationFrame(tagbodyEnv));
  int frame = _lisp->exceptionStack().push(TagbodyFrame, tagbodyId);
  // Start to evaluate the tagbody
  List_sp ip = args;
  while (ip.notnilp()) {
    T_sp tagOrForm = oCar(ip);
    if (cl_consp(tagOrForm)) {
      try {
        eval::evaluate(tagOrForm, tagbodyEnv);
      } catch (LexicalGo &go) {
        if (go.getFrame() != frame) {
          throw go;
        }
        int index = go.index();
        ip = tagbodyEnv->codePos(index);
      } catch (DynamicGo &dgo) {
        if (dgo.getFrame() != frame) {
          throw dgo;
        }
        int index = dgo.index();
        ip = tagbodyEnv->codePos(index);
      }
    }
    ip = oCdr(ip);
  }
  LOG(BF("Leaving sp_tagbody"));
  _lisp->exceptionStack().unwind(frame);
  return Values0<T_O>();
};

#define DOCS_sp_go "go special form - see CLHS"
T_mv sp_go(List_sp args, T_sp env) {
  _G();
  Symbol_sp tag = gc::As<Symbol_sp>(oCar(args));
  int depth = 0;
  int index = 0;
  bool interFunction;
  T_sp tagbodyEnv;
  bool foundTag = Environment_O::clasp_findTag(env, tag, depth, index, interFunction, tagbodyEnv);
  if (!foundTag) {
    SIMPLE_ERROR(BF("Could not find tag[%s] in the lexical environment: %s") % _rep_(tag) % _rep_(env));
  }
  T_sp tagbodyId = Environment_O::clasp_lookupTagbodyId(Environment_O::clasp_getActivationFrame(env), depth, index);
  int frame = _lisp->exceptionStack().findKey(TagbodyFrame, tagbodyId);
  if (frame < 0) {
    SIMPLE_ERROR(BF("Could not find tagbody frame for tag %s") % _rep_(tag));
  }
  DynamicGo go(frame, index);
  throw go;
}

#define ARGS_af_classifyLetVariablesAndDeclares "(variables declared-specials)"
#define DECL_af_classifyLetVariablesAndDeclares ""
#define DOCS_af_classifyLetVariablesAndDeclares "classifyLetVariablesAndDeclares - return (values classified-variables num-lexicals) - For each variable name in variables and declared-specials classify each as special-var, lexical-var or declared-special using the declared-specials list"

T_mv af_classifyLetVariablesAndDeclares(List_sp variables, List_sp declaredSpecials) {
  _G();
  HashTableEq_sp specialsSet = HashTableEq_O::create_default();
  for (auto cur : declaredSpecials)
    specialsSet->insert(oCar(cur)); //make(declaredSpecials);
  HashTableEq_sp specialInVariables(HashTableEq_O::create_default());
  HashTable_sp indices = cl_make_hash_table(cl::_sym_eq, make_fixnum(8),
                                            DoubleFloat_O::create(1.5),
                                            DoubleFloat_O::create(1.0));
  ql::list classified(_lisp);
  size_t indicesSize = 0;
  for (auto cur : variables) {
    Symbol_sp sym = gc::As<Symbol_sp>(oCar(cur));
    if (specialsSet->contains(sym)) {
      classified << Cons_O::create(ext::_sym_specialVar, sym);
      specialInVariables->insert(sym);
    } else if (sym->specialP()) {
      classified << Cons_O::create(ext::_sym_specialVar, sym);
      specialInVariables->insert(sym);
    } else {
      int idx;
      T_sp fi = indices->gethash(sym, _Unbound<T_O>());
      if (!fi.unboundp()) {
        idx = unbox_fixnum(gc::As<Fixnum_sp>(fi));
      } else {
        idx = indicesSize;
        indices->hash_table_setf_gethash(sym, make_fixnum(idx));
        ++indicesSize;
      }
      classified << Cons_O::create(ext::_sym_lexicalVar,
                                   Cons_O::create(sym, make_fixnum(idx)));
    }
  }
  specialsSet->maphash([&classified, &specialInVariables](T_sp s, T_sp val) {
                    if ( !specialInVariables->contains(s) ) {
                        classified << Cons_O::create(core::_sym_declaredSpecial,s);
                    }
  });
  T_sp tclassified = classified.cons();
  return Values(tclassified, make_fixnum((int)indicesSize));
}

/*
  __BEGIN_DOC(candoScript.specialForm.let,let)
  \scriptCmd{let}{assignments code}

  Assign lexical variables and then evaluate code in that context.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.specialForm.let,let)
  \scriptCmd{let\*}{assignments code}

  Assign lexical variables and then evaluate code in that context.
  __END_DOC
*/
T_mv sp_let(List_sp args, T_sp parentEnvironment) {
  List_sp assignments = oCar(args);
  T_mv pairOfLists = af_separatePairList(assignments);
  List_sp variables = coerce_to_list(pairOfLists);
  List_sp expressions = pairOfLists.valueGet(1);
  List_sp body = oCdr(args);
  //    LOG(BF("Extended the environment - result -->\n%s") % newEnvironment->__repr__() );
  //    LOG(BF("Evaluating code in this new lexical environment: %s") % body->__repr__() );
  List_sp declares;
  gc::Nilable<Str_sp> docstring;
  List_sp code;
  List_sp declaredSpecials;
  extract_declares_docstring_code_specials(body, declares, false, docstring, code, declaredSpecials);
  LOG(BF("Assignment part=%s") % assignments->__repr__());
  T_mv classifiedAndCount = af_classifyLetVariablesAndDeclares(variables, declaredSpecials);
  List_sp classified = coerce_to_list(classifiedAndCount);
  int numberOfLexicalVariables = unbox_fixnum(gc::As<Fixnum_sp>(classifiedAndCount.valueGet(1)));
  ValueEnvironment_sp newEnvironment =
      ValueEnvironment_O::createForNumberOfEntries(numberOfLexicalVariables, parentEnvironment);
  ValueEnvironmentDynamicScopeManager scope(newEnvironment);
  // Set up the debugging info - it's empty to begin with
  ValueFrame_sp valueFrame = gc::As<ValueFrame_sp>(newEnvironment->getActivationFrame());
  VectorObjects_sp debuggingInfo = VectorObjects_O::create(_Nil<T_O>(),
                                                           cl_length(valueFrame), _Nil<T_O>());
  //  valueFrame->attachDebuggingInfo(debuggingInfo);

  // Figure out which environment to evaluate in
  List_sp curExp = expressions;
  Environment_sp evaluateEnvironment;
  // SPECIFIC TO LET FROM HERE ON DOWN
  evaluateEnvironment = parentEnvironment;
  int debugInfoIndex = 0;
  //		printf("%s:%d In LET\n", __FILE__, __LINE__);

  size_t numTemps = cl_length(classified);
  core::T_O **tempValues = (core::T_O **)__builtin_alloca(sizeof(core::T_O *) * numTemps);
  size_t valueIndex = 0;
  for (auto curClassified : classified) {
    List_sp classified = oCar(curClassified);
    Symbol_sp shead = gc::As<Symbol_sp>(oCar(classified));
    if (shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar) {
      T_sp expr = oCar(curExp);
      T_sp result = eval::evaluate(expr, evaluateEnvironment);
      //			printf("%s:%d Evaluated %s --> %s\n", __FILE__, __LINE__, _rep_(expr).c_str(), _rep_(result).c_str());
      if (valueIndex >= numTemps) {
        SIMPLE_ERROR(BF("Overflow in LET temporary variables only %d available") % numTemps);
      }
      tempValues[valueIndex] = result.raw_();
      ++valueIndex;
      curExp = oCdr(curExp);
    }
  }
  valueIndex = 0;
  for (auto curClassified : classified) {
    List_sp classified = oCar(curClassified);
    Symbol_sp shead = gc::As<Symbol_sp>(oCar(classified));
    if (shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar) {
      if (valueIndex >= numTemps) {
        SIMPLE_ERROR(BF("Overflow in LET temporary variables only %d available") % numTemps);
      }
      T_sp result = gctools::smart_ptr<T_O>((gc::Tagged)tempValues[valueIndex]);
      ++valueIndex;
      scope.new_variable(classified, result);
    } else if (shead == _sym_declaredSpecial) {
      scope.new_special(classified);
    }
    if (shead == ext::_sym_lexicalVar) {
      debuggingInfo->setf_elt(debugInfoIndex, oCadr(classified));
      debugInfoIndex++;
    }
  }
  return eval::sp_progn(code, newEnvironment);
}

T_mv sp_letSTAR(List_sp args, T_sp parentEnvironment) {
  List_sp assignments = oCar(args);
  T_mv pairOfLists = af_separatePairList(assignments);
  List_sp variables = coerce_to_list(pairOfLists);
  List_sp expressions = pairOfLists.valueGet(1);
  List_sp body = oCdr(args);
  //    LOG(BF("Extended the environment - result -->\n%s") % newEnvironment->__repr__() );
  //    LOG(BF("Evaluating code in this new lexical environment: %s") % body->__repr__() );
  List_sp declares;
  gc::Nilable<Str_sp> docstring;
  List_sp code;
  List_sp declaredSpecials;
  extract_declares_docstring_code_specials(body, declares, false, docstring, code, declaredSpecials);
  LOG(BF("Assignment part=%s") % assignments->__repr__());
  T_mv classifiedAndCount = af_classifyLetVariablesAndDeclares(variables, declaredSpecials);
  List_sp classified = coerce_to_list(classifiedAndCount);
  int numberOfLexicalVariables = unbox_fixnum(gc::As<Fixnum_sp>(classifiedAndCount.valueGet(1)));
  ValueEnvironment_sp newEnvironment =
      ValueEnvironment_O::createForNumberOfEntries(numberOfLexicalVariables, parentEnvironment);
  ValueEnvironmentDynamicScopeManager scope(newEnvironment);

  // Set up the debugging info - it's empty to begin with
  ValueFrame_sp valueFrame = gc::As<ValueFrame_sp>(newEnvironment->getActivationFrame());
  VectorObjects_sp debuggingInfo = VectorObjects_O::create(_Nil<T_O>(),
                                                           cl_length(valueFrame), _Nil<T_O>());
  //  valueFrame->attachDebuggingInfo(debuggingInfo);

  // Figure out which environment to evaluate in
  List_sp curExp = expressions;
  Environment_sp evaluateEnvironment;

  // SPECIFIC TO LET* FROM HERE ON DOWN
  evaluateEnvironment = newEnvironment; // SPECIFIC TO LET*
  int debugInfoIndex = 0;
  for (auto curClassified : classified) {
    List_sp classified = oCar(curClassified);
    Symbol_sp shead = gc::As<Symbol_sp>(oCar(classified));
    if (shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar) {
      T_sp expr = oCar(curExp);
      T_sp result = eval::evaluate(expr, evaluateEnvironment);
      scope.new_variable(classified, result);
      curExp = oCdr(curExp);
    } else if (shead == _sym_declaredSpecial) {
      scope.new_special(classified);
    }
    if (shead == ext::_sym_lexicalVar) {
      debuggingInfo->setf_elt(debugInfoIndex, oCadr(classified));
      debugInfoIndex++;
    }
  }
  return eval::sp_progn(code, newEnvironment);
}

/*
  __BEGIN_DOC(candoScript.specialForm.if,if)
  \scriptCmd{if}{condition thenCode elseCode}\par
  \scriptCmd{if}{condition thenCode }

  If/then/else control statement.
  __END_DOC
*/
T_mv sp_if(List_sp args, T_sp environment) {
  _G();
  T_sp res;
  {
    res = eval::evaluate(oCar(args), environment);
  }
  if (oCdddr(args).notnilp()) {
    SIMPLE_ERROR(BF("Illegal if has too many expressions: %s") % _rep_(args));
  }
  if (res.isTrue()) {
    return eval::evaluate(oCadr(args), environment);
  } else {
    if (oCdr(oCdr(args)).notnilp()) {
      return eval::evaluate(oCaddr(args), environment);
    }
  }
  return (Values(_Nil<T_O>()));
}

/*
  __BEGIN_DOC(candoScript.specialForm.cond,cond)
  \scriptCmd{cond}{[ [cond1 code1 ...] [cond2 code2 ... ] ...] }\par

  Works just like lisp "cond" control structure. Evaluates each condition and for the first one that evaluates as true its associated block is evaluated.
  __END_DOC
*/
#if 1
T_mv sp_cond(List_sp args, T_sp environment) {
  _G();
  for (auto cur : args) {
    T_sp cond;
    List_sp condProgn;
    {
      condProgn = oCar(cur);
      cond = eval::evaluate(oCar(condProgn), environment);
    }
    if (cond.isTrue()) {
      List_sp code = oCdr(condProgn);
      if (code.notnilp()) {
        return eval::sp_progn(code, environment);
      }
      return (Values(cond));
    }
  }
  return (Values(_Nil<T_O>()));
}
#endif

T_mv sp_block(List_sp args, T_sp environment) {
  _G();
  Symbol_sp blockSymbol = gc::As<Symbol_sp>(oCar(args));
  BlockEnvironment_sp newEnvironment = BlockEnvironment_O::make(blockSymbol, environment);
  int frame = _lisp->exceptionStack().push(BlockFrame, blockSymbol);
  LOG(BF("sp_block has extended the environment to: %s") % newEnvironment->__repr__());
  T_mv result;
  try {
    result = eval::sp_progn(oCdr(args), newEnvironment);
  } catch (ReturnFrom &returnFrom) {
    LOG(BF("Caught ReturnFrom with returnFrom.getBlockDepth() ==> %d") % returnFrom.getBlockDepth());
    if (returnFrom.getFrame() != frame) // Symbol() != newEnvironment->getBlockSymbol() )
    {
      throw returnFrom;
    }
    result = gctools::multiple_values<T_O>::createFromValues(); // returnFrom.getReturnedObject();
  }
  LOG(BF("Leaving sp_block"));
  _lisp->exceptionStack().unwind(frame);
  return result;
}

T_mv sp_returnFrom(List_sp args, T_sp environment) {
  _G();
  Symbol_sp blockSymbol = gc::As<Symbol_sp>(oCar(args));
  int frame = _lisp->exceptionStack().findKey(BlockFrame, blockSymbol);
  if (frame < 0) {
    SIMPLE_ERROR(BF("Could not find block named %s in lexical environment: %s") % _rep_(blockSymbol) % _rep_(environment));
  }
  T_mv result = Values(_Nil<T_O>());
  if (oCdr(args).notnilp()) {
    result = eval::evaluate(oCadr(args), environment);
  }
  result.saveToMultipleValue0();
  ReturnFrom returnFrom(frame);
  throw returnFrom;
}

#if 1
T_mv sp_unwindProtect(List_sp args, T_sp environment) {
  T_mv result = Values(_Nil<T_O>());
  gc::Vec0<core::T_sp> save;
  try {
    // Evaluate the protected form
    result = eval::evaluate(oCar(args), environment);
    // Save the return values
    result.saveToVec0(save);
    // Evaluate the unwind forms --
    // THIS IS REALLY, REALLY WRONG - it shouldn't be protected here
    eval::sp_progn(oCdr(args), environment);
  } catch (...) {
    eval::sp_progn(oCdr(args), environment);
    throw;
  }
  result.loadFromVec0(save);
  return result;
}
#else
T_mv sp_unwindProtect(List_sp args, T_sp environment) {
  T_mv result = Values(_Nil<T_O>());
  VectorObjects_sp save(VectorObjects_O::create());
  try {
    // Evaluate the protected form
    result = eval::evaluate(oCar(args), environment);
    // Save the return values
    multipleValuesSaveToVector(result, save);
    // Evaluate the unwind forms --
    // THIS IS REALLY, REALLY WRONG - it shouldn't be protected here
    eval::sp_progn(oCdr(args), environment);
  } catch (...) {
    eval::sp_progn(oCdr(args), environment);
    throw;
  }
  return multipleValuesLoadFromVector(save);
}
#endif

T_mv sp_catch(List_sp args, T_sp environment) {
  _G();
  T_sp mytag = eval::evaluate(oCar(args), environment);
  int frame = _lisp->exceptionStack().push(CatchFrame, mytag);
  T_mv result;
  try {
    result = eval::sp_progn(oCdr(args), environment);
  } catch (CatchThrow &catchThrow) {
    if (catchThrow.getFrame() != frame) {
      throw catchThrow;
    }
    result = gctools::multiple_values<T_O>::createFromValues();
  }
  _lisp->exceptionStack().unwind(frame);
  return result;
}

T_mv sp_throw(List_sp args, T_sp environment) {
  _G();
  T_sp throwTag = eval::evaluate(oCar(args), environment);
  T_mv result = Values(_Nil<T_O>());
  int frame = _lisp->exceptionStack().findKey(CatchFrame, throwTag);
  if (frame < 0) {
    CONTROL_ERROR();
  }
  if (oCdr(args).notnilp()) {
    result = eval::evaluate(oCadr(args), environment);
  }
  // The first return value needs to be saved in MultipleValues
  result.saveToMultipleValue0();
  // I should search for the Catch frame for throwTag and
  // invoke an error if it doesn't exist
  CatchThrow catchThrow(frame);
  printf("%s:%d Throwing core::CatchThrow exception@%p tag[%s] frame: %d\n", __FILE__, __LINE__, &catchThrow, (throwTag)->__repr__().c_str(), frame);
  throw catchThrow;
}

T_mv sp_multipleValueProg1(List_sp args, T_sp environment) {
  _G();
  VectorObjects_sp save(VectorObjects_O::create());
  T_mv val0 = eval::evaluate(oCar(args), environment);
  multipleValuesSaveToVector(val0, save);
  eval::evaluateListReturnLast(oCdr(args), environment);
  return multipleValuesLoadFromVector(save);
}

T_mv sp_multipleValueCall(List_sp args, T_sp env) {
  Function_sp func;
  func = gc::As<Function_sp>(eval::evaluate(oCar(args), env));
  List_sp resultList = _Nil<T_O>();
  Cons_sp *cur = reinterpret_cast<Cons_sp *>(&resultList);
  for (auto forms : (List_sp)oCdr(args)) {
    T_sp oneForm = oCar(forms);
    T_mv retval = eval::evaluate(oneForm, env);
    *cur = Cons_O::create(retval, _Nil<T_O>());
    cur = reinterpret_cast<Cons_sp *>(&(*cur)->_Cdr);
    for (int i(1); i < retval.number_of_values(); i++) {
      *cur = Cons_O::create(retval.valueGet(i), _Nil<T_O>());
      cur = reinterpret_cast<Cons_sp *>(&(*cur)->_Cdr);
    }
  }
  size_t sz = cl_length(resultList);
  STACK_FRAME(buff, fargs, sz);
  size_t i(0);
  for (auto c : resultList) {
    fargs[i] = oCar(c).raw_();
    ++i;
  }
  VaList_S valist_struct(fargs);
  VaList_sp valist(&valist_struct); // = valist_struct.fargs.setupVaList(valist_struct);
  return eval::apply_consume_VaList(func, valist);
}

#define ARGS_af_processDeclarations "(body expectDocString)"
#define DECL_af_processDeclarations ""
#define DOCS_af_processDeclarations "Handle special declarations and remove declarations from body. Return MultipleValues: declarations body documentation specials"
T_mv af_processDeclarations(List_sp inputBody, T_sp expectDocString) {
  _G();
  bool b_expect_doc = expectDocString.isTrue();
  List_sp declares = _Nil<T_O>();
  gc::Nilable<Str_sp> docstring;
  List_sp code;
  List_sp specials;
  extract_declares_docstring_code_specials(inputBody, declares,
                                           b_expect_doc, docstring, code, specials);
  T_sp tdeclares = declares;
  return (Values(tdeclares, code, (T_sp)docstring, specials));
};

#define ARGS_core_extractLambdaNameFromDeclares "(declare-list &optional default)"
#define DECL_core_extractLambdaNameFromDeclares ""
#define DOCS_core_extractLambdaNameFromDeclares "If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX"
T_sp core_extractLambdaNameFromDeclares(List_sp declares, T_sp defaultValue) {
  // First check for a (declare (core:function-name XXX))
  for (; declares.consp(); declares = oCdr(declares)) {
    List_sp decl = oCar(declares);
    if (oCar(decl) == core::_sym_lambdaName) {
      return oCadr(decl);
    }
  }
  return defaultValue;
}

#define ARGS_core_extractLambdaName "(form &optional default)"
#define DECL_core_extractLambdaName ""
#define DOCS_core_extractLambdaName "If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX"
T_sp core_extractLambdaName(List_sp lambdaExpression, T_sp defaultValue) {
  List_sp body = oCddr(lambdaExpression);
  List_sp declares;
  gc::Nilable<Str_sp> docstring;
  List_sp form;
  parse_lambda_body(body, declares, docstring, form);
  // First check for a (declare (core:function-name XXX))
  T_sp name = core_extractLambdaNameFromDeclares(declares, _Nil<T_O>());
  if (name.notnilp())
    return name;
  // Next check if there is a (lambda (...) (block XXX ...))
  if (cl_consp(form) && cl_length(form) == 1) {
    T_sp first = oCar(form);
    if (cl_consp(first)) {
      if (oCar(first) == cl::_sym_block) {
        T_sp second = oCadr(first);
        Symbol_sp name = gc::As<Symbol_sp>(oCadr(first));
        if (name.notnilp()) {
          // Only return block name if not nil
          return name;
        }
      }
    }
  }
  // Fallback return LAMBDA as the name
  return defaultValue;
}

/*! Parse a lambda expression of the form ([declare*] ["docstring"] body...) */
Function_sp lambda(T_sp name, bool wrap_block, T_sp lambda_list, List_sp body, T_sp env) {
  _G();
  List_sp declares;
  gc::Nilable<Str_sp> docstring;
  List_sp form;
  parse_lambda_body(body, declares, docstring, form);
  LOG(BF("lambda is closing over environment\n%s") % env->__repr__());
  LambdaListHandler_sp llh;
  if (lambda_list.nilp()) {
    llh = lisp_function_lambda_list_handler(_Nil<List_V>(), declares);
  } else if (cl_consp(lambda_list)) {
    llh = lisp_function_lambda_list_handler(lambda_list, declares);
    LOG(BF("Passed lambdaList: %s") % lambda_list->__repr__());
  } else if (af_lambda_list_handler_p(lambda_list)) {
    llh = gc::As<LambdaListHandler_sp>(lambda_list);
  } else {
    SIMPLE_ERROR(BF("Illegal object for lambda-list you can "
                    "only pass a Cons or LambdaListHandler"));
  }
  // If the name is NIL then check if the form has the form (BLOCK XXX ...)
  // if it does then use XXX as the name
  if (name.nilp()) {
    name = core_extractLambdaName(form, cl::_sym_lambda);
  }

  List_sp code(form);
  if (wrap_block) {
    code = Cons_O::create(Cons_O::create(cl::_sym_block,
                                         Cons_O::create(
                                             af_functionBlockName(name),
                                             code)));
    if (_lisp->sourceDatabase().notnilp()) {
      gc::As<SourceManager_sp>(_lisp->sourceDatabase())->duplicateSourcePosInfo(body, code);
    }
  }
  //            printf("%s:%d Creating InterpretedClosure with no source information - fix this\n", __FILE__, __LINE__ );
  T_sp spi(_Nil<T_O>());
  if (_lisp->sourceDatabase().notnilp()) {
    spi = gc::As<SourceManager_sp>(_lisp->sourceDatabase())->lookupSourcePosInfo(code);
  }
  if (!spi || spi.nilp()) {
    spi = gc::As<SourcePosInfo_sp>(_sym_STARcurrentSourcePosInfoSTAR->symbolValue());
    if (spi.nilp()) {
      printf("%s:%d   Could not find source info for lambda\n", __FILE__, __LINE__);
    }
  }
  gctools::tagged_pointer<InterpretedClosure> ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(name, kw::_sym_function, llh, declares, docstring, env, code, SOURCE_POS_INFO_FIELDS(spi));
  Function_sp proc = Function_O::make(ic);
  return proc;
}

/*
  __BEGIN_DOC(candoScript.specialForm.function,function)
  \scriptCmd{function}{object}

  Returns function associated with the name.
  (name) is either a symbol or a lambda or lambda-block expression.
  (lambda (args...) body...) or (lambda-block name (args...) body...)
  __END_DOC
*/
T_mv sp_function(List_sp args, T_sp environment) {
  _G();
  ASSERTP(oCdr(args).nilp(), "You can provide only one argument - a symbol that has a function bound to it or a lambda");
  T_sp arg = oCar(args);
  if (arg.nilp()) {
    WRONG_TYPE_ARG(arg, Cons_O::createList(cl::_sym_or, cl::_sym_Symbol_O, cl::_sym_Cons_O));
  } else if (Symbol_sp fnSymbol = arg.asOrNull<Symbol_O>()) {
    LOG(BF("In sp_function - looking up for for[%s]") % fnSymbol->__repr__());
    T_sp fn = af_interpreter_lookup_function(fnSymbol, environment);
    if (fn.nilp()) {
      SIMPLE_ERROR(BF("Could not find function %s args: %s") % _rep_(fnSymbol) % _rep_(args));
    }
    LOG(BF("     Found form: %s") % fn->__repr__());
    return (Values(fn));
  } else if (Cons_sp cconsArg = arg.asOrNull<Cons_O>()) {
    List_sp consArg = cconsArg;
    T_sp head = oCar(consArg);
    if (head == cl::_sym_setf) {
      T_sp fn = af_interpreter_lookup_setf_function(consArg, environment);
      if (fn.nilp()) {
        SIMPLE_ERROR(BF("Could not find function %s args: %s") % _rep_(consArg) % _rep_(args));
      }
      return (Values(fn));
    } else if (head == cl::_sym_lambda || head == ext::_sym_lambda_block) {
      T_sp name;
      List_sp lambdaList;
      List_sp body;
      bool wrapBlock = false;
      if (head == cl::_sym_lambda) {
        name = core_extractLambdaName(consArg, cl::_sym_lambda);
        lambdaList = oCadr(consArg);
        body = oCddr(consArg);
        wrapBlock = false;
      } else // head==cl::_sym_lambda_block
      {
        name = af_functionBlockName(oCadr(consArg));
        lambdaList = oCaddr(consArg);
        body = oCdddr(consArg);
        wrapBlock = true;
      }
      //		    HALT(BF("Check name/lambdaList/body and if ok remove me"));
      // Create an anonymous function and close it over the current environment
      Function_sp lambdaFunction = lambda(name,
                                          wrapBlock,
                                          lambdaList,
                                          body,
                                          environment);
      return (Values(lambdaFunction));
    }
  }
  SIMPLE_ERROR(BF("Illegal argument[%s] for function") % _rep_(arg));
}

#if 0
#define DOCS_sp_lambda_block "Like lambda but the first argument is a symbol that defines the name of the lambda"
	T_mv sp_lambda_block( List_sp args, T_sp env)
	{_G();
	    ASSERTNOTNULL(args);
	    Symbol_sp name = args->ocar().as<Symbol_O>();
	    return lambda(name,true,args->ocadr(),args->cddr(),env);
	}
#endif

#if 0
#define DOCS_sp_lambda_with_handler "Like lambda but the first argument is a symbol that defines the name of the lambda and the second argument is a lambda-list-handler rather than a lambda-list"
	T_mv sp_lambda_with_handler( List_sp args, T_sp env)
	{_G();
	    ASSERTNOTNULL(args);
	    Symbol_sp name = args->ocar().as<Symbol_O>();
	    LambdaListHandler_sp llh = eval::evaluate(args->ocadr(),env).as<LambdaListHandler_O>();
	    return lambda(name,false,llh,args->cddr(),env,_lisp);
	}
#endif

/*
  __BEGIN_DOC(candoScript.specialForm.quote,quote)
  \scriptCmdRet{quote}{object}{unevaluatedObject}

  Returns the \scriptArg{object} without evaluating it.
  __END_DOC
*/
T_mv sp_quote(List_sp args, T_sp environment) {
  _G();
  ASSERTF(cl_length(args) == 1, BF("Only one argument allowed for QUOTE"));
  return (Values(oCar(args)));
}

/*
  __BEGIN_DOC(candoScript.general.let,let)
  \scriptCmd{let}{symbol object}\par
  \scriptInfix{symbol}{=}{object}

  Evaluate the arguments and put it into the local variable \scriptArg{symbol}.
  __END_DOC
*/
T_mv sp_setq(List_sp args, T_sp environment) {
  _G();
  ASSERTP(oCdr(args).notnilp(), "You must provide at least 2 arguments");
  List_sp pairs = args;
  T_sp result = _Nil<T_O>();
  while (pairs.notnilp()) {
    T_sp target = oCar(pairs);
    if (Symbol_sp symbol = target.asOrNull<Symbol_O>()) {
      if (oCdr(pairs).nilp()) {
        SIMPLE_ERROR(BF("Missing value for setq of target[%s] - body of setq: %s") % _rep_(target) % _rep_(args));
      }
      T_sp expr = oCadr(pairs);
      T_sp texpr = cl_macroexpand(symbol, environment);
      if (texpr != symbol) {
        // The target symbol was a symbol-macro so we
        // switch from SETQ to a SETF to define it
        eval::evaluate(Cons_O::createList(cl::_sym_setf, texpr, expr), environment);
      } else {
        result = eval::evaluate(expr, environment);
        interpret::setq_symbol_value(symbol, result, environment);
      }
      pairs = oCddr(pairs);
    } else {
      SIMPLE_ERROR(BF("Illegal target[%s] for setq - body of setq: %s") % _rep_(target) % _rep_(args));
    }
  }
  return (Values(result));
}

T_mv sp_flet(List_sp args, T_sp environment) {
  _G();
  // TODO: handle trace
  T_sp functionName;
  List_sp functions = oCar(args);
  FunctionValueEnvironment_sp newEnvironment = FunctionValueEnvironment_O::createForEntries(cl_length(functions), environment);
  List_sp body = oCdr(args);
  List_sp cur = functions;
  LOG(BF("functions part=%s") % functions->__repr__());
  while (cur.notnilp()) {
    List_sp oneDef = oCar(cur);
    functionName = oCar(oneDef);
    Function_sp func = lambda(functionName, true, oCadr(oneDef), oCddr(oneDef), environment);
    newEnvironment->bind_function(functionName, func);
    cur = oCdr(cur);
  }
  List_sp declares;
  List_sp code;
  gc::Nilable<Str_sp> docstring;
  List_sp specials;
  extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  return eval::sp_progn(code, newEnvironment);
}

/*
  __BEGIN_DOC(candoScript.macros.labels,labels)
  \scriptCmd{labels}{(function bindings) code...}

  Define functions recursively in new lexical environments.
  __END_DOC
*/
T_mv sp_labels(List_sp args, T_sp environment) {
  _G();
  // TODO: handle trace
  T_sp name;
  List_sp functions = oCar(args);
  List_sp body = oCdr(args);
  List_sp cur = functions;
  LOG(BF("functions part=%s") % functions->__repr__());
  FunctionValueEnvironment_sp newEnvironment = FunctionValueEnvironment_O::createForEntries(cl_length(functions), environment);
  while (cur.notnilp()) {
    List_sp oneDef = oCar(cur);
    name = oCar(oneDef);
    Function_sp func = lambda(name, true, oCadr(oneDef) /*lambda-list*/, oCddr(oneDef) /*body with decls/docstring*/, newEnvironment);
    LOG(BF("func = %s") % func->__repr__());
    newEnvironment->bind_function(name, func);
    cur = oCdr(cur);
  }
  List_sp declares;
  List_sp code;
  gc::Nilable<Str_sp> docstring;
  List_sp specials;
  extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  return eval::sp_progn(code, newEnvironment);
}

T_mv t1Progn(List_sp args, T_sp environment);

T_mv doMacrolet(List_sp args, T_sp env, bool toplevel) {
  // TODO: handle trace
  List_sp macros = oCar(args);
  MacroletEnvironment_sp newEnv(MacroletEnvironment_O::make(env));
  List_sp body = oCdr(args);
  List_sp cur = macros;
  LOG(BF("macros part=%s") % macros->__repr__());
  while (cur.notnilp()) {
    List_sp oneDef = oCar(cur);
    //		printf( "%s:%d  oneDef = %s\n", __FILE__, __LINE__, _rep_(oneDef).c_str());
    Symbol_sp name = gc::As<Symbol_sp>(oCar(oneDef));
    T_sp olambdaList = oCadr(oneDef);
    List_sp inner_body = oCdr(oCdr(oneDef));
    List_sp inner_declares;
    gc::Nilable<Str_sp> inner_docstring;
    List_sp inner_code;
    parse_lambda_body(inner_body, inner_declares, inner_docstring, inner_code);
    // printf("   name = %s\n", _rep_(name).c_str());
    // printf("   olambdaList = %s\n", _rep_(olambdaList).c_str());
    // printf("   inner_body = %s\n", _rep_(inner_body).c_str());
    // printf("   inner_declares = %s\n", _rep_(inner_declares).c_str());
    // printf("   inner_docstring = %s\n", _rep_(inner_docstring).c_str());
    // printf("   inner_code = %s\n", _rep_(inner_code).c_str());
    List_sp outer_func_cons = eval::funcall(core::_sym_parse_macro, name, olambdaList, inner_body);
    //		printf("%s:%d sp_macrolet outer_func_cons = %s\n", __FILE__, __LINE__, _rep_(outer_func_cons).c_str());
    Function_sp outer_func;
    if (comp::_sym_compileInEnv->fboundp()) {
      // If the compiler is set up then compile the outer func
      outer_func = gc::As<Function_sp>(eval::funcall(comp::_sym_compileInEnv, _Nil<T_O>(), outer_func_cons, newEnv));
      outer_func->setKind(kw::_sym_macro);
    } else {
      List_sp outer_ll = oCadr(outer_func_cons);
      //		printf("%s:%d sp_macrolet outer_ll = %s\n", __FILE__, __LINE__, _rep_(outer_ll).c_str());
      List_sp outer_body = oCddr(outer_func_cons);
      //		printf("%s:%d sp_macrolet outer_body = %s\n", __FILE__, __LINE__, _rep_(outer_body).c_str());
      List_sp declares;
      gc::Nilable<Str_sp> docstring;
      List_sp code;
      parse_lambda_body(outer_body, declares, docstring, code);
      LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll, declares, cl::_sym_function);
      printf("%s:%d Creating InterpretedClosure with no source information - fix this\n", __FILE__, __LINE__);
      gctools::tagged_pointer<InterpretedClosure> ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(name, kw::_sym_macro, outer_llh, declares, docstring, newEnv, code, SOURCE_POS_INFO_FIELDS(_Nil<T_O>()));
      outer_func = Function_O::make(ic);
    }
    LOG(BF("func = %s") % outer_func_cons->__repr__());
    newEnv->addMacro(name, outer_func);
    //		newEnv->bind_function(name,outer_func);
    cur = oCdr(cur);
  }
  List_sp declares;
  List_sp code;
  gc::Nilable<Str_sp> docstring;
  List_sp specials;
  extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  if (toplevel) {
    return t1Progn(code, newEnv);
  } else {
    return eval::sp_progn(code, newEnv);
  }
}

/*
  __BEGIN_DOC(candoScript.macros.macroLet,macroLet)
  \scriptCmd{macroLet}{(function bindings) code...}

  Define macros recursively in new lexical environments.
  __END_DOC
*/
T_mv sp_macrolet(List_sp args, T_sp env) {
  _G();
  return doMacrolet(args, env, false /* toplevel */);
}

extern T_mv t1Locally(List_sp args, T_sp env);

T_mv do_symbolMacrolet(List_sp args, T_sp env, bool topLevelForm) {
  _G();
  List_sp macros = oCar(args);
  SymbolMacroletEnvironment_sp newEnv(SymbolMacroletEnvironment_O::make(env));
  List_sp body = oCdr(args);
  List_sp cur = macros;
  LOG(BF("macros part=%s") % macros->__repr__());
  SYMBOL_SC_(CorePkg, whole);
  SYMBOL_SC_(CorePkg, env);
  List_sp outer_ll = Cons_O::createList(_sym_whole, _sym_env);
  SYMBOL_EXPORT_SC_(ClPkg, ignore);
  List_sp declares = Cons_O::createList(cl::_sym_declare, Cons_O::createList(cl::_sym_ignore, _sym_whole, _sym_env));
  while (cur.notnilp()) {
    List_sp oneDef = oCar(cur);
    Symbol_sp name = gc::As<Symbol_sp>(oCar(oneDef));
    List_sp expansion = Cons_O::create(Cons_O::createList(cl::_sym_quote, oCadr(oneDef)), _Nil<T_O>());
    LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,
                                                                 oCadr(declares),
                                                                 cl::_sym_function);
    printf("%s:%d Creating InterpretedClosure with no source information and empty name- fix this\n", __FILE__, __LINE__);
    gctools::tagged_pointer<InterpretedClosure> ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(_sym_symbolMacroletLambda, kw::_sym_macro, outer_llh, declares, _Nil<T_O>(), newEnv, expansion, SOURCE_POS_INFO_FIELDS(_Nil<T_O>()));
    Function_sp outer_func = Function_O::make(ic);
    newEnv->addSymbolMacro(name, outer_func);
    cur = oCdr(cur);
  }
  if (topLevelForm) {
    return t1Locally(body, newEnv);
  } else {
    return eval::sp_locally(body, newEnv);
  }
}

T_mv sp_symbolMacrolet(List_sp args, T_sp env) {
  _G();
  return do_symbolMacrolet(args, env, false);
#if 0
	    List_sp macros = oCar(args);
	    SymbolMacroletEnvironment_sp newEnv(SymbolMacroletEnvironment_O::make(env));
	    List_sp body = oCdr(args);
	    List_sp cur = macros;
	    LOG(BF("macros part=%s") % macros->__repr__() );
	    gc::Nilable<Str_sp> docString = _Nil<T_O>();
	    SYMBOL_SC_(CorePkg,whole);
	    SYMBOL_SC_(CorePkg,env);
	    List_sp outer_ll = Cons_O::createList(_sym_whole, _sym_env);
	    SYMBOL_EXPORT_SC_(ClPkg,ignore);
	    List_sp declares = Cons_O::createList(cl::_sym_declare,Cons_O::createList(cl::_sym_ignore,_sym_whole,_sym_env));
	    while ( cur.notnilp() )
	    {
		List_sp oneDef = oCar(cur);
		Symbol_sp name = oCar(oneDef).as<Symbol_O>();
		List_sp expansion = Cons_O::create(Cons_O::createList(cl::_sym_quote,oCadr(oneDef)),_Nil<T_O>());
		LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,
									     oCadr(declares),
									     cl::_sym_function);
                printf("%s:%d Creating InterpretedClosure with no source information and empty name- fix this\n", __FILE__, __LINE__ );
                InterpretedClosure* ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass( _sym_symbolMacroletLambda
                                                                                                    , _Nil<SourcePosInfo_O>()
                                                                                                    , kw::_sym_macro
                                                                                                    , outer_llh
                                                                                                    , declares
                                                                                                    , _Nil<T_O>()
                                                                                                    , newEnv
                                                                                                    , expansion );
                Function_sp outer_func = Function_O::make(ic);
		newEnv->addSymbolMacro(name,outer_func);
		cur = oCdr(cur);
	    }
	    return eval::sp_locally(body,newEnv);
#endif
}

T_mv handleConditionInEvaluate(T_sp environment) {
  _G();
  T_mv result;
  try {
    throw;
  } catch (Condition &cond) {
    THROW_HARD_ERROR(BF("Figure out what should happen here"));
#if 0
		try
		{
		    THROW(_lisp->error(cond.conditionObject()/*,environment */));
		}
		catch (DebuggerSaysContinue& debuggerSaysResume)
		{
		    T_mv resumeResult = debuggerSaysResume.returnObject();
		    LOG(BF("Execution will resume with return value[%s]") % resumeResult->__repr__() );
		    return resumeResult;
		}
		catch (HardError& err)
		{
		    // Convert the HardError into a Condition
		    SIMPLE_ERROR(BF("HARD_ERROR: %s") % err.message() );
		}
//		catch (...) { throw;}
		;
#endif
  } catch (HardError &err) {
    // Convert the HardError into a Condition
    SIMPLE_ERROR(BF("HARD_ERROR: %s") % err.message());
  }
#if 0
	    catch (DebuggerSaysContinue& debuggerSaysResume)
	    {
		T_mv resumeResult = debuggerSaysResume.returnObject();
		LOG(BF("Execution will resume with return value[%s]") % resumeResult->__repr__() );
		return resumeResult;
	    }
#endif
  catch (const std::exception &exc) {
    SIMPLE_ERROR(BF("std::exception--> %s") % exc.what());
  }
  SIMPLE_ERROR(BF("Failed to handle exception"));
}

/*! Returns NIL if no function is found */
T_sp lookupFunction(T_sp functionDesignator, T_sp env) {
  _G();
  ASSERTF(functionDesignator, BF("In apply, the head function designator is UNDEFINED"));
  if (Function_sp exec = functionDesignator.asOrNull<Function_O>())
    return exec;
  Symbol_sp shead = gc::As<Symbol_sp>(functionDesignator);
  T_sp exec = af_interpreter_lookup_function(shead, env);
  return exec;
}

T_mv applyClosureToActivationFrame(gctools::tagged_pointer<Closure> func, ActivationFrame_sp args) {
  size_t nargs = args->length();
  T_sp *frame = args->argArray();
  switch (nargs) {
#define APPLY_TO_ACTIVATION_FRAME
#include <clasp/core/generated/applyToActivationFrame.h>
#undef APPLY_TO_ACTIVATION_FRAME
  default:
    SIMPLE_ERROR(BF("Illegal number of arguments in call: %s") % nargs);
  };
}

#if 0
T_mv applyClosureToStackFrame(gctools::tagged_pointer<Closure> func, T_sp stackFrame) {
IMPLEMENT_MEF(BF("Handle new valist"));
#if 0
T_mv result;
  ASSERT(stackFrame.valistp());
  core::T_O **frameImpl(stackFrame.unsafe_frame());
  size_t nargs = frame::ValuesArraySize(frameImpl);
  T_O **frame = frame::ValuesArray(frameImpl);
  switch (nargs) {
#define APPLY_TO_TAGGED_FRAME
#include <clasp/core/generated/applyToActivationFrame.h>
#undef APPLY_TO_TAGGED_FRAME
  default:
      SIMPLE_ERROR(BF("Illegal number of arguments: %d") % nargs);
  };
#endif
}
#endif

#if 0
T_mv applyToStackFrame(T_sp head, T_sp stackFrame) {
  _G();
  ASSERT(stackFrame.valistp());
  Function_sp fn = lookupFunction(head, stackFrame);
  ASSERT(fn.notnilp());
  gctools::tagged_pointer<Closure> closureP = fn->closure;
  ASSERTF((closureP), BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
  return applyClosureToStackFrame(closureP, stackFrame);
}
#endif

T_mv applyToActivationFrame(T_sp head, ActivationFrame_sp args) {
  _G();
  T_sp tfn = lookupFunction(head, args);
  if (tfn.nilp()) {
    if (head == cl::_sym_findClass) {
      // When booting, cl::_sym_findClass may be apply'd but not
      // defined yet
      return (cl_findClass(gc::As<Symbol_sp>(args->entry(0)), true, _Nil<T_O>()));
    }
    SIMPLE_ERROR(BF("Could not find function %s args: %s") % _rep_(head) % _rep_(args));
  }
  Function_sp ffn = gc::As<Function_sp>(tfn);
  gctools::tagged_pointer<Closure> closureP = ffn->closure;
  if (closureP) {
    return applyClosureToActivationFrame(closureP, args);
  }
  SIMPLE_ERROR(BF("In applyToActivationFrame the closure for %s is NULL and is being applied to arguments: %s") % _rep_(ffn) % _rep_(args));
}

#if 0
void apply_fill_frame_from_list(core::T_O** frameImpl, int offset, int nargs, List_sp elements)
{
IMPLEMENT_MEF(BF("Handle new valist"));
#if 0
  for (int i(0); i < nargs; ++i) {
    frame::ValuesArray(frameImpl)[i+offset] = oCar(elements).raw_();
    elements = oCdr(elements);
  }
#endif
}
#endif

void errorApplyZeroArguments() {
  SIMPLE_ERROR(BF("Illegal to have zero arguments for APPLY"));
}

void errorApplyLastArgumentNotList() {
  SIMPLE_ERROR(BF("Last argument of APPLY is not a list/frame/activation-frame"));
}

#define ARGS_cl_apply "(head &va-rest args)"
#define DECL_cl_apply ""
#define DOCS_cl_apply "apply"
T_mv cl_apply(T_sp head, VaList_sp args) {
  Function_sp func = coerce::functionDesignator(head);
  int lenArgs = args->nargs();
  if (lenArgs == 0) {
    errorApplyZeroArguments();
  }
  T_sp last = T_sp((gc::Tagged)args->indexed_arg(lenArgs - 1));
  if (last.nilp()) {
    // Nil as last argument
    LCC_VA_LIST_SET_NUMBER_OF_ARGUMENTS(args, lenArgs - 1);
    gctools::tagged_pointer<Closure> ft = func->closure;
    core::T_O *arg0;
    core::T_O *arg1;
    core::T_O *arg2;
    VaList_S &valist_s = *args;
    LCC_VA_LIST_INDEXED_ARG(arg0, valist_s, 0);
    LCC_VA_LIST_INDEXED_ARG(arg1, valist_s, 1);
    LCC_VA_LIST_INDEXED_ARG(arg2, valist_s, 2);
    gc::return_type res = (*ft).invoke_va_list(NULL,
                                               args.raw_(),
                                               LCC_VA_LIST_NUMBER_OF_ARGUMENTS(args),
                                               arg0,  //LCC_VA_LIST_REGISTER_ARG0(args),
                                               arg1,  //LCC_VA_LIST_REGISTER_ARG1(args),
                                               arg2); //LCC_VA_LIST_REGISTER_ARG2(args) );
    return res;
  } else if (last.valistp() && lenArgs == 1) {
    VaList_sp valast((gc::Tagged)last.raw_());
    VaList_S valast_copy(*valast);
    VaList_sp valast_copy_sp(&valast_copy);
    return eval::apply_consume_VaList(func, valast_copy_sp);
  } else if (last.valistp()) {
    VaList_sp valast((gc::Tagged)last.raw_());
    VaList_S valist_scopy(*valast);
    VaList_sp lastArgs(&valist_scopy); // = gc::smart_ptr<VaList_S>((gc::Tagged)last.raw_());
    int lenFirst = lenArgs - 1;
    int lenRest = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(lastArgs);
    int nargs = lenFirst + lenRest;
    // Allocate a frame on the side stack that can take all arguments
    STACK_FRAME(buff, frame, nargs);
    T_sp obj = args;
    for (int i(0); i < lenFirst; ++i) {
      frame[i] = LCC_NEXT_ARG_RAW(args, i);
    }
    for (int i(lenFirst); i < nargs; ++i) {
      frame[i] = LCC_NEXT_ARG_RAW(lastArgs, i);
    }
    VaList_S valist_struct(frame);
    VaList_sp valist(&valist_struct); // = frame.setupVaList(valist_struct);
    return eval::apply_consume_VaList(func, valist);
  } else if (List_sp cargs = gc::As<Cons_sp>(last)) {
    // Cons as last argument
    int lenFirst = lenArgs - 1;
    int lenRest = cl_length(last);
    int nargs = lenFirst + lenRest;
    STACK_FRAME(buff, frame, nargs);
    T_sp obj = args;
    for (int i(0); i < lenFirst; ++i) {
      frame[i] = LCC_NEXT_ARG_RAW(args, i);
    }
    for (int i(lenFirst); i < nargs; ++i) {
      frame[i] = oCar(cargs).raw_();
      cargs = oCdr(cargs);
    }
    VaList_S valist_struct(frame);
    VaList_sp valist(&valist_struct); // = frame.setupVaList(valist_struct);;
    return eval::apply_consume_VaList(func, valist);
  }
  errorApplyLastArgumentNotList();
}

#if 1
// fast funcall
#define ARGS_cl_funcall "(function_desig &va-rest args)"
#define DECL_cl_funcall ""
#define DOCS_cl_funcall "See CLHS: funcall"
T_mv cl_funcall(T_sp function_desig, VaList_sp args) {
  //    printf("%s:%d cl_funcall should be inlined after the compiler starts up\n", __FILE__, __LINE__ );
  Function_sp func = coerce::functionDesignator(function_desig);
  if (func.nilp()) {
    ERROR_UNDEFINED_FUNCTION(function_desig);
  }
  T_mv res = eval::apply_consume_VaList(func, args);
  return res;
}
#else
// slow funcall
#define ARGS_cl_funcall "(function_desig &rest args)"
#define DECL_cl_funcall ""
#define DOCS_cl_funcall "See CLHS: funcall"
T_mv cl_funcall(T_sp function_desig, List_sp args) {
  //    printf("%s:%d cl_funcall should be inlined after the compiler starts up\n", __FILE__, __LINE__ );
  Function_sp func = coerce::functionDesignator(function_desig);
  if (func.nilp()) {
    ERROR_UNDEFINED_FUNCTION(function_desig);
  }
  STACK_FRAME(buff, passArgs, cl_length(args));
  int idx(0);
  for (auto cur : args) {
    passArgs[idx] = oCar(cur).raw_();
    ++idx;
  }
  VaList_S vargs_struct(passArgs);
  VaList_sp vargs(&vargs_struct);
  T_mv res = eval::apply_consume_VaList(func, vargs);
  return res;
}
#endif

/*!
 * This method:
 * 1) evaluates the arguments
 * 2) Looks up the method using the methodCall and the first argument
 * 3) evaluates the method
 * Can return MultipleValues
 */

int _evaluateVerbosity = 0;
int _evaluateDepth = 0;

#define ARGS_af_evaluateDepth "()"
#define DECL_af_evaluateDepth ""
#define DOCS_af_evaluateDepth "evaluateDepth"
int af_evaluateDepth() {
  _G();
  return _evaluateDepth;
};

#define ARGS_af_evaluateVerbosity "(arg)"
#define DECL_af_evaluateVerbosity ""
#define DOCS_af_evaluateVerbosity "evaluateVerbosity"
void af_evaluateVerbosity(Fixnum_sp level) {
  _G();
  _evaluateVerbosity = unbox_fixnum(level);
};

struct EvaluateDepthUpdater {
  EvaluateDepthUpdater() {
    ++_evaluateDepth;
  }
  ~EvaluateDepthUpdater() {
    --_evaluateDepth;
  }
};

T_mv evaluate_atom(T_sp exp, T_sp environment) {
  T_mv result;
  LOG(BF("Evaluating atom: %s") % exp->__repr__());
  if (exp.fixnump() || exp.characterp() || exp.single_floatp()) {
    return Values(exp);
  } else if (Symbol_sp sym = exp.asOrNull<Symbol_O>()) {
    _BLOCK_TRACEF(BF("Evaluating symbol: %s") % exp->__repr__());
    if (sym->isKeywordSymbol())
      return Values(sym);
    if (core_lookup_symbol_macro(sym, environment).notnilp()) {
      T_sp texpr;
      {
        MULTIPLE_VALUES_CONTEXT();
        texpr = cl_macroexpand(sym, environment);
      }
      try {
        result = eval::evaluate(texpr, environment);
      } catch (...) {
        result = handleConditionInEvaluate(environment);
      };
      return (result);
    }
    try {
      result = af_interpreter_lookup_variable(sym, environment);
    } catch (...) {
      result = handleConditionInEvaluate(environment);
    }
    return (result);
  }
  LOG(BF(" Its the self returning object: %s") % exp->__repr__());
  return (Values(exp));
}

T_mv evaluate_lambdaHead(List_sp headCons, List_sp form, T_sp environment) {
  T_mv result;
  if (oCar(headCons) == cl::_sym_lambda) {
    IMPLEMENT_MEF(BF("Handle lambda better"));
#if 0
      ASSERTF(oCar(headCons)==cl::_sym_lambda,BF("Illegal head %s - must be a LAMBDA expression") % _rep_(headCons) );
		//
		// The head is a cons with a non-symbol for a head, evaluate it
		//
      {
        if ( _lisp->isSingleStepOn() )
        {
          IMPLEMENT_ME();
#if 0
          LispDebugger::step();
#endif
        }
        ValueFrame_sp evaluatedArgs(ValueFrame_O::create(cl_length(oCdr(form)),_Nil<ActivationFrame_O>()));
        evaluateIntoActivationFrame(evaluatedArgs,oCdr(form),environment);
        try { result = eval::applyToActivationFrame(headCons,evaluatedArgs);}
        catch (...) { result = handleConditionInEvaluate(environment);};
      }
#endif
  } else {
    SIMPLE_ERROR(BF("Illegal form: %s") % _rep_(form));
  }
  return (result);
}

T_mv evaluate_specialForm(SpecialForm_sp specialForm, List_sp form, T_sp environment) {
  return specialForm->evaluate(oCdr(form), environment);
}

T_mv evaluate_cond(List_sp form, T_sp environment) {
  _G();
  T_mv result;
  try {
    result = interpret::interpreter_cond(oCdr(form), environment);
  } catch (...) {
    result = handleConditionInEvaluate(environment);
  }
  ASSERTNOTNULL(result);
  return (result);
}

T_mv evaluate_case(List_sp form, T_sp environment) {
  _G();
  T_mv result;
  try {
    result = interpret::interpreter_case(oCdr(form), environment);
  } catch (...) {
    result = handleConditionInEvaluate(environment);
  }
  ASSERTNOTNULL(result);
  return (result);
}

T_mv evaluate_multipleValueSetq(List_sp form, T_sp environment) {
  _G();
  T_mv result;
  SYMBOL_EXPORT_SC_(ClPkg, multipleValueSetq);
  try {
    result = interpret::interpreter_multipleValueSetq(oCdr(form), environment);
  } catch (...) {
    result = handleConditionInEvaluate(environment);
  }
  ASSERTNOTNULL(result);
  return (result);
}

T_mv evaluate_prog1(List_sp form, T_sp environment) {
  _G();
  T_mv result;
  SYMBOL_EXPORT_SC_(ClPkg, prog1);
  try {
    result = interpret::interpreter_prog1(oCdr(form), environment);
  } catch (...) {
    result = handleConditionInEvaluate(environment);
  }
  ASSERTNOTNULL(result);
  return (result);
}

SYMBOL_EXPORT_SC_(CompPkg, compileInEnv);

T_mv t1Progn(List_sp args, T_sp environment) {
  _G();
  if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
    printf("%s:%d t1Progn args: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
  }
  T_mv result(_Nil<T_O>());
  T_sp localEnv(environment);
  for (auto cur : args) {
    result = t1Evaluate(oCar(cur), localEnv);
  }
  return result;
}

T_mv t1EvalWhen(T_sp args, T_sp environment) {
  _G();
  if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
    printf("%s:%d t1EvalWhen args: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
  }
  List_sp situations = oCar(args);
  List_sp body = oCdr(args);
  bool execute = cl_member(kw::_sym_execute, situations, _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>()).isTrue();
  execute |= cl_member(cl::_sym_eval, situations, _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>()).isTrue();
  if (execute)
    return t1Progn(body, environment);
  return (Values(_Nil<T_O>()));
}

T_mv t1Locally(List_sp args, T_sp env) {
  _G();
  if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
    printf("%s:%d t1Locally args: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
  }
  List_sp declares;
  gc::Nilable<Str_sp> docstring;
  List_sp code;
  List_sp specials;
  extract_declares_docstring_code_specials(args, declares, false, docstring, code, specials);
  ValueEnvironment_sp le = ValueEnvironment_O::createForLocallySpecialEntries(specials, env);
  // ignore everything else for now
  return eval::t1Progn(code, le);
}

T_mv t1Macrolet(List_sp args, T_sp env) {
  _G();
  return doMacrolet(args, env, true /*toplevel*/);
#if 0
    if ( _sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp() ) {
      printf("%s:%d t1Macrolet args: %s\n", __FILE__, __LINE__, _rep_(args).c_str() );
    }
	    // TODO: handle trace
    List_sp macros = oCar(args);
    MacroletEnvironment_sp newEnv(MacroletEnvironment_O::make(env));
    List_sp body = oCdr(args);
    List_sp cur = macros;
    LOG(BF("macros part=%s") % macros->__repr__() );
    gc::Nilable<Str_sp> docString = _Nil<T_O>();
    while ( cur.notnilp() )
    {
      List_sp oneDef = oCar(cur);
      Symbol_sp name = oCar(oneDef).as<Symbol_O>();
      T_sp olambdaList = oCadr(oneDef);
      List_sp inner_body = oCdr(oCdr(oneDef));
      List_sp outer_func_cons = eval::funcall(core::_sym_parse_macro,name,olambdaList,inner_body);
#if 1
//                printf("%s:%d   outer_func_cons = %s\n", __FILE__, __LINE__, _rep_(outer_func_cons).c_str());
      Function_sp outer_func = eval::funcall(comp::_sym_compileInEnv
                                             , _Nil<T_O>()
                                             , outer_func_cons
                                             ,newEnv ).as<Function_O>();
      outer_func->setKind(kw::_sym_macro);
#else
      List_sp outer_ll = oCaddr(outer_func_cons);
      List_sp outer_body = cCdddr(outer_func_cons);
      List_sp declares;
      Str_sp docstring;
      List_sp code;
      parse_lambda_body(outer_body,declares,docstring,code);
      LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,declares,cl::_sym_function);
                // TODO: Change these to compiled functions when the compiler is available
//                printf("%s:%d Creating InterpretedClosure with no source info\n", __FILE__, __LINE__ );
      InterpretedClosure* ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(name
                                                                                          , _Nil<SourcePosInfo_O>()
                                                                                          , kw::_sym_macro
                                                                                          , outer_llh
                                                                                          , declares
                                                                                          , docstring
                                                                                          , newEnv
                                                                                          , code );
      Function_sp outer_func = Function_O::make(ic);
#endif
      LOG(BF("func = %s") % outer_func_cons->__repr__() );
//                printf("%s:%d addMacro name: %s  macro: %s\n", __FILE__, __LINE__, _rep_(name).c_str(), _rep_(outer_func).c_str());
      newEnv->addMacro(name,outer_func);
//		newEnv->bind_function(name,outer_func);
      cur = cCdr(cur);
    }
    List_sp declares;
    List_sp code;
    Str_sp docstring;
    List_sp specials;
    extract_declares_docstring_code_specials(body,declares,false,docstring,code,specials);
//            printf("%s:%d macrolet evaluating code: %s  in env: %s\n", __FILE__, __LINE__, _rep_(code).c_str(), _rep_(newEnv).c_str());
    return t1Progn(code,newEnv);
#endif
}

T_mv t1SymbolMacrolet(List_sp args, T_sp env) {
  _G();
  return do_symbolMacrolet(args, env, true);
#if 0
    List_sp macros = oCar(args);
    SymbolMacroletEnvironment_sp newEnv(SymbolMacroletEnvironment_O::make(env));
    List_sp body = cCdr(args);
    List_sp cur = macros;
    LOG(BF("macros part=%s") % macros->__repr__() );
    gc::Nilable<Str_sp> docstring = _Nil<T_O>();
    SYMBOL_SC_(CorePkg,whole);
    SYMBOL_SC_(CorePkg,env);
    List_sp outer_ll = Cons_O::createList(_sym_whole, _sym_env);
    SYMBOL_EXPORT_SC_(ClPkg,ignore);
    List_sp declares = Cons_O::createList(cl::_sym_declare,Cons_O::createList(cl::_sym_ignore,_sym_whole,_sym_env));
    while ( cur.notnilp() )
    {
      List_sp oneDef = oCar(cur);
      Symbol_sp name = oCar(oneDef).as<Symbol_O>();
      List_sp expansion = Cons_O::create(Cons_O::createList(cl::_sym_quote,oCadr(oneDef)),_Nil<T_O>());
//                printf("%s:%d  symbolmacrolet name=%s expansion=%s\n", __FILE__, __LINE__, _rep_(name).c_str(), _rep_(expansion).c_str() );
      Function_sp outer_func;
#if 0
      T_sp olambdaList = _Nil<T_O>();
      List_sp inner_body = oCadr(oneDef);
      List_sp outer_func_cons = eval::funcall(core::_sym_parse_macro,name,olambdaList,inner_body);
      printf("%s:%d  symbolmacrolet name=%s expansion I can compile=%s\n", __FILE__, __LINE__, _rep_(name).c_str(), _rep_(outer_func_cons).c_str() );
      printf("%s:%d   outer_func_cons = %s\n", __FILE__, __LINE__, _rep_(outer_func_cons).c_str());
      outer_func = eval::funcall(comp::_sym_compileInEnv
                                 , _Nil<T_O>()
                                 , outer_func_cons
                                 ,newEnv ).as<Function_O>();
      outer_func->setKind(kw::_sym_macro);
#else
      LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,
                                                                   oCadr(declares),
                                                                   cl::_sym_function);
                // TODO: Change these to compiled functions when the compiler is available
      printf("%s:%d Creating InterpretedClosure for expansion: %s\n", __FILE__, __LINE__, _rep_(expansion).c_str());
      InterpretedClosure* ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(_sym_symbolMacroletLambda
                                                                                          , _Nil<SourcePosInfo_O>()
                                                                                          , kw::_sym_macro
                                                                                          , outer_llh
                                                                                          , declares
                                                                                          , docstring
                                                                                          , newEnv
                                                                                          , expansion );
      outer_func = Function_O::make(ic);
#endif
      newEnv->addSymbolMacro(name,outer_func);
      cur = cCdr(cur);
    }
    return t1Locally(body,newEnv);
#endif
}

T_mv t1Evaluate(T_sp exp, T_sp environment) {
  if (cl_consp(exp)) {
    T_sp head = oCar(exp);
    if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
      printf("%s:%d Checking if top-level head: %s  cl::_sym_eval_when: %s eq=%d    form: %s\n", __FILE__, __LINE__, _rep_(head).c_str(), _rep_(cl::_sym_eval_when).c_str(), (head == cl::_sym_eval_when), _rep_(exp).c_str());
    }
    // TODO: Deal with Compiler macros here
    T_sp macroFunction(_Nil<T_O>());
    if (cl_symbolp(head)) {
      macroFunction = eval::funcall(cl::_sym_macroFunction, head, environment);
      if (macroFunction.notnilp()) {
        T_sp expanded = eval::funcall(macroFunction, exp, environment);
        return t1Evaluate(expanded, environment);
      } else if (head == cl::_sym_progn) {
        return t1Progn(oCdr(exp), environment);
      } else if (head == cl::_sym_eval_when) {
        //                        printf("%s:%d   head is eval-when\n", __FILE__, __LINE__ );
        return t1EvalWhen(oCdr(exp), environment);
      } else if (head == cl::_sym_locally) {
        return t1Locally(oCdr(exp), environment);
      } else if (head == cl::_sym_macrolet) {
        return t1Macrolet(oCdr(exp), environment);
      } else if (head == cl::_sym_symbol_macrolet) {
        return t1SymbolMacrolet(oCdr(exp), environment);
      }
    }
  }
  if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
    printf("%s:%d About to compileFormAndEvalWithEnv: %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
  }
  return eval::funcall(comp::_sym_STARimplicit_compile_hookSTAR->symbolValue(), exp, environment);
}

#define ARGS_core_eval_with_env_default "(form &optional env stepping compiler-env-p (execute t))"
#define DECL_core_eval_with_env_default ""
#define DOCS_core_eval_with_env_default "eval_with_env_default"
T_mv core_eval_with_env_default(T_sp form, T_sp env) {
  return t1Evaluate(form, env);
}

int global_interpreter_trace_depth = 0;
struct InterpreterTrace {
  InterpreterTrace() {
    ++global_interpreter_trace_depth;
  };
  ~InterpreterTrace() {
    --global_interpreter_trace_depth;
  };
};

T_mv evaluate(T_sp exp, T_sp environment) {
  //	    Environment_sp localEnvironment = environment;
  //            printf("%s:%d evaluate %s environment@%p\n", __FILE__, __LINE__, _rep_(exp).c_str(), environment.raw_());
  //            printf("    environment: %s\n", _rep_(environment).c_str() );
  T_mv result;
  Cons_sp cform;
  List_sp form;
  T_sp head;
  af_stackMonitor();
  EvaluateDepthUpdater evaluateDepthUpdater;
  if (_evaluateVerbosity > 0) {
    printf("core::eval::evaluate depth[%5d] -> %s\n", _evaluateDepth, _rep_(exp).c_str());
  }
  if (exp.nilp()) {
    //		LOG(BF("Expression is nil - returning nil"));
    if (_evaluateVerbosity > 0) {
      printf("core::eval::evaluate depth[%5d] return <- %s\n", _evaluateDepth, _rep_(exp).c_str());
    }
    result = Values(exp);
    goto DONE;
  }
  if (cl_atom(exp)) {
    result = evaluate_atom(exp, environment);
    goto DONE;
  }
  //
  // If it reached here then exp is a cons
  //
  //	    LOG(BF("Evaluating cons[%s]") % exp->__repr__() );
  //	    printf("    Evaluating: %s\n", _rep_(exp).c_str() );
  //	    printf("    In env: %s\n", _rep_(environment).c_str() );
  cform = exp.asOrNull<Cons_O>();
  form = cform;
  ASSERTNOTNULL(form);
  head = oCar(form);
  if (Symbol_sp headSym = head.asOrNull<Symbol_O>()) {
    T_sp specialForm = _lisp->specialFormOrNil(headSym);
    if (!specialForm.nilp()) {
      result = evaluate_specialForm(specialForm, form, environment);
      goto DONE;
    }

    if (headSym == cl::_sym_cond) {
      result = evaluate_cond(form, environment);
      goto DONE;
    } else if (headSym == cl::_sym_case) {
      result = evaluate_case(form, environment);
      goto DONE;
    } else if (headSym == cl::_sym_multipleValueSetq) {
      result = evaluate_multipleValueSetq(form, environment);
      goto DONE;
    } else if (headSym == cl::_sym_prog1) {
      result = evaluate_prog1(form, environment);
      goto DONE;
    }

    T_sp theadFunc = af_interpreter_lookup_macro(headSym, environment);
    if (theadFunc.notnilp()) {
      /* Macro expansion should be done immediately after the reader - 
		       - done here the macros are expanded again and again and again
		    */
      T_sp expanded = _Nil<T_O>();
      try {
#if 1
        if (_sym_STARinterpreterTraceSTAR->symbolValue().notnilp()) {
          if (gc::As<HashTable_sp>(_sym_STARinterpreterTraceSTAR->symbolValue())->gethash(headSym).notnilp()) {
            InterpreterTrace itrace;
            printf("eval::evaluate Trace [%d] macroexpand > %s\n", global_interpreter_trace_depth, _rep_(form).c_str());
            expanded = cl_macroexpand(form, environment);
            printf("eval::evaluate Trace [%d] < (%s ...)\n", global_interpreter_trace_depth, _rep_(headSym).c_str());
          } else {
            expanded = cl_macroexpand(form, environment);
          }
        } else {
          expanded = cl_macroexpand(form, environment);
        }
#else
        expanded = cl_macroexpand(form, environment);
#endif
        if (_evaluateVerbosity > 0) {
          string es = _rep_(expanded);
          printf("core::eval::evaluate expression is macro - expanded --> %s\n", es.c_str());
        }
      } catch (Condition &cond) {
        THROW_HARD_ERROR(BF("Figure out what to do from here"));
        //			_lisp->error(cond.conditionObject()/*,environment*/);
      }; // catch (...) {throw;};
      result = eval::evaluate(expanded, environment);
      goto DONE;
    }
    theadFunc = af_interpreter_lookup_function(headSym, environment);
    if (theadFunc.nilp()) {
      SIMPLE_ERROR(BF("Could not find form(%s) in the lexical/dynamic environment") % _rep_(headSym));
    }

    //
    // It is a form and its head is a symbol,
    // evaluate the arguments and apply the function bound to the head to them
    //
    //		LOG(BF("Symbol[%s] is a normal form - evaluating arguments") % head->__repr__() );
    size_t nargs = cl_length(oCdr(form));
    STACK_FRAME(buff, callArgs, nargs);
    size_t argIdx = 0;
    for (auto cur : (List_sp)oCdr(form)) {
      callArgs[argIdx] = eval::evaluate(oCar(cur), environment).raw_();
      ++argIdx;
    }
    VaList_S valist_struct(callArgs);
    VaList_sp valist(&valist_struct); // = callArgs.setupVaList(valist_struct);
    Function_sp headFunc = gc::As<Function_sp>(theadFunc);
    if (_sym_STARinterpreterTraceSTAR->symbolValue().notnilp()) {
      if (gc::As<HashTable_sp>(_sym_STARinterpreterTraceSTAR->symbolValue())->gethash(headSym).notnilp()) {
        InterpreterTrace itrace;
        printf("eval::evaluate Trace [%d] > (%s ", global_interpreter_trace_depth, _rep_(headSym).c_str());
        for (int i(0), iEnd(nargs); i < iEnd; ++i) {
          printf("%s ", _rep_(T_sp((gc::Tagged)callArgs[i])).c_str());
        }
        printf(" )\n");
        result = eval::apply_consume_VaList(headFunc, valist);
        printf("eval::evaluate Trace [%d] < (%s ...)\n", global_interpreter_trace_depth, _rep_(headSym).c_str());
      } else {
        result = eval::apply_consume_VaList(headFunc, valist);
      }
    } else {
      result = eval::apply_consume_VaList(headFunc, valist);
    }
    goto DONE;
  }
  {
    List_sp headCons = head;
    ASSERTF(headCons, BF("Illegal head %s - must be a LAMBDA expression") % _rep_(head));
    result = evaluate_lambdaHead(headCons, form, environment);
    goto DONE;
  }
DONE:
  if (_evaluateVerbosity > 0) {
    printf("core::eval::evaluate depth[%5d] <---- %p\n", _evaluateDepth, exp.raw_());
  }
  return result;
}

void evaluateIntoActivationFrame(ActivationFrame_sp af,
                                 List_sp args, T_sp environment) {
  _G();
  if (args.nilp()) {
    LOG(BF("Arguments before evaluateList: Nil ---> returning Nil"));
    return;
  }
  LOG(BF("Arguments before evaluateList: %s") % _rep_(args));
  {
    _BLOCK_TRACE("Evaluating...");
    int idx = 0;
    // Iterate through each car in exp and
    // evaluate it (handling Nil objects and results)
    // and string the results into a linked list
    for (auto p : args) {
      T_sp inObj = oCar(p);
      T_sp result = eval::evaluate(inObj, environment);
      LOG(BF("After evaluation result = %s") % _rep_(result));
      af->set_entry(idx, result);
      ++idx;
    }
  }
  LOG(BF("Arguments after evaluateList: %s") % _rep_(af));
}

List_sp evaluateList(List_sp args, T_sp environment) {
  _G();
  Cons_sp firstCons = Cons_O::create(_Nil<T_O>());
  Cons_sp curCons = firstCons;
  if (args.nilp()) {
    LOG(BF("Arguments before evaluateList: Nil ---> returning Nil"));
    return _Nil<T_O>();
  }
  LOG(BF("Arguments before evaluateList: %s") % args->__repr__());
  {
    _BLOCK_TRACE("Evaluating...");
    // Iterate through each car in exp and
    // evaluate it (handling Nil objects and results)
    // and string the results into a linked list
    for (auto p : args) {
      T_sp inObj = oCar(p);
      T_sp result = eval::evaluate(inObj, environment);
      ASSERTNOTNULL(result);
      LOG(BF("After evaluation result = %s @ %X") % result->__repr__() % (void *)(result.get()));
      Cons_sp outCons = Cons_O::create(result);
      curCons->setCdr(outCons);
      curCons = outCons;
    }
  }
#ifdef DEBUG_ON
  List_sp tempCons = firstCons->cdr();
  while (tempCons.notnilp()) {
    T_sp zobj = oCar(tempCons);
    LOG(BF("Argument after evaluateList in order: %s @ %X") % zobj->__repr__() % (void *)(zobj.get()));
    tempCons = tempCons->cdr();
  }
#endif
  LOG(BF("Arguments after evaluateList: %s") % _rep_(oCdr(firstCons)));
  return oCdr(firstCons);
}

T_mv evaluateListReturnLast(List_sp args, T_sp environment) {
  _G();
  T_sp inObj;
  T_mv outObj;
  outObj = Values(_Nil<T_O>());
  {
    _BLOCK_TRACE("Evaluating...");
    // Iterate through each car in exp and
    // evaluate it (handling Nil objects and results)
    // and string the results into a linked list
    //
    //
    for (auto p : args) {
      inObj = oCar(p);
      LOG(BF("Pushing code onto the backTrace: <%s>") % p->__repr__());
      {
        TRY() {
          outObj = eval::evaluate(inObj, environment); // used to use newEnvironment
        }
        catch (Condition &err) {
          THROW_HARD_ERROR(BF("Figure out what to do here"));
#if 0
            TRY()
            {
              _lisp->error(err.conditionObject() /*,environment */);
            }
            catch (DebuggerSaysContinue& dc)
            {
              outObj = dc.returnObject();
            }
#endif
        }
      }
    }
  }
  return outObj;
}

void defineSpecialOperatorsAndMacros(Package_sp pkg) {
  _G();
  SYMBOL_EXPORT_SC_(ClPkg, block);
  SYMBOL_EXPORT_SC_(ClPkg, quote);
  SYMBOL_EXPORT_SC_(ClPkg, progn);
  SYMBOL_EXPORT_SC_(ClPkg, throw);
  _lisp->defineSpecialOperator(ExtPkg, "special-var", &sp_specialVar);
  _lisp->defineSpecialOperator(ExtPkg, "lexical-var", &sp_lexicalVar);
  _lisp->defineSpecialOperator(ClPkg, "block", &sp_block);
  _lisp->defineSpecialOperator(ClPkg, "catch", &sp_catch);
  _lisp->defineSpecialOperator(ClPkg, "eval-when", &sp_eval_when);
  _lisp->defineSpecialOperator(ExtPkg, "debug-message", &sp_debug_message);
  _lisp->defineSpecialOperator(ClPkg, "flet", &sp_flet);
  _lisp->defineSpecialOperator(ClPkg, "function", &sp_function);
  _lisp->defineSpecialOperator(ClPkg, "the", &sp_the);
  // SBCL defined truly-the as a special operator
  _lisp->defineSpecialOperator(ExtPkg, "truly-the", &sp_the);
  _lisp->defineSpecialOperator(ClPkg, "go", &sp_go);
  _lisp->defineSpecialOperator(ClPkg, "if", &sp_if);
  _lisp->defineSpecialOperator(ClPkg, "labels", &sp_labels);
  _lisp->defineSpecialOperator(ClPkg, "let", &sp_let);
  _lisp->defineSpecialOperator(ClPkg, "let*", &sp_letSTAR);
  _lisp->defineSpecialOperator(ClPkg, "locally", &sp_locally);
  _lisp->defineSpecialOperator(ClPkg, "macrolet", &sp_macrolet);
  _lisp->defineSpecialOperator(ClPkg, "multipleValueProg1", &sp_multipleValueProg1);
  _lisp->defineSpecialOperator(ClPkg, "multipleValueCall", &sp_multipleValueCall);
  _lisp->defineSpecialOperator(ClPkg, "progn", &sp_progn);
  _lisp->defineSpecialOperator(ClPkg, "progv", &sp_progv);
  _lisp->defineSpecialOperator(ClPkg, "quote", &sp_quote);
  _lisp->defineSpecialOperator(ClPkg, "return-from", &sp_returnFrom);
  _lisp->defineSpecialOperator(ClPkg, "setq", &sp_setq);
  _lisp->defineSpecialOperator(ClPkg, "tagbody", &sp_tagbody);
  _lisp->defineSpecialOperator(ClPkg, "throw", &sp_throw);
  _lisp->defineSpecialOperator(ClPkg, "unwind-protect", &sp_unwindProtect);
  _lisp->defineSpecialOperator(ClPkg, "symbol-macrolet", &sp_symbolMacrolet);
  _lisp->defineSpecialOperator(ClPkg, "load-time-value", &sp_loadTimeValue);
  // missing special operator load-time-value
  // missing progv

  // These need to be converted to macros
  //	    _lisp->defineSpecialOperator(ExtPkg,"step",&sp_step);

  SYMBOL_SC_(CorePkg, processDeclarations);
  Defun(processDeclarations);
  SYMBOL_EXPORT_SC_(ClPkg, eval);
  ClDefun(eval);
  //	    SYMBOL_SC_(CorePkg,extractDeclaresDocstringCode);
  //	    Defun(extractDeclaresDocstringCode);
  SYMBOL_SC_(CorePkg, evaluateVerbosity);
  Defun(evaluateVerbosity);
  SYMBOL_EXPORT_SC_(CompPkg, compileFormAndEvalWithEnv);
  CoreDefun(compileFormAndEvalWithEnv);
  SYMBOL_SC_(CorePkg, evaluateDepth);
  Defun(evaluateDepth);
  SYMBOL_SC_(CorePkg, classifyLetVariablesAndDeclares);
  Defun(classifyLetVariablesAndDeclares);
  SYMBOL_EXPORT_SC_(ClPkg, apply);
  ClDefun(apply);
  SYMBOL_EXPORT_SC_(ClPkg, funcall);
  ClDefun(funcall);
  CoreDefun(extractLambdaNameFromDeclares);
  CoreDefun(extractLambdaName);
  CoreDefun(lookup_symbol_macro);
  CoreDefun(coerce_to_function);
  SYMBOL_EXPORT_SC_(CorePkg, STAReval_with_env_hookSTAR);
  SYMBOL_EXPORT_SC_(CorePkg, eval_with_env_default);
  af_def(CorePkg, "eval_with_env_default",
         &core_eval_with_env_default);
  core::_sym_STAReval_with_env_hookSTAR->defparameter(core::_sym_eval_with_env_default->symbolFunction());
};
};
};
