/*
    File: lambdaListHandler.cc
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
//#define DEBUG_LEVEL_FULL

#include <string.h>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/lispList.h>
#include <clasp/core/primitives.h>
#include <clasp/core/array.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/wrappers.h>

namespace core {

SYMBOL_EXPORT_SC_(KeywordPkg, calledFunction);
SYMBOL_EXPORT_SC_(KeywordPkg, givenNargs);
SYMBOL_EXPORT_SC_(KeywordPkg, minNargs);
SYMBOL_EXPORT_SC_(KeywordPkg, maxNargs);
SYMBOL_EXPORT_SC_(KeywordPkg, unrecognizedKeywords);
SYMBOL_EXPORT_SC_(CorePkg, AMPva_rest );
/*! Return true if the form represents a type
*/
DOCGROUP(clasp);
CL_DEFUN bool core__is_a_type(T_sp form)
{
  if ( form == _lisp->_true() ) return true;
  if ( form.nilp() ) return true;
  if ( form == cl::_sym_sequence ) return true;
  if ( form == cl::_sym_simple_base_string ) return true;
  if ( cl__symbolp(form) ) {
    if ( core::_sym__PLUS_known_typep_predicates_PLUS_.notnilp() ) {
      List_sp type_predicates = core::_sym__PLUS_known_typep_predicates_PLUS_->symbolValue();
      if ( type_predicates.notnilp() ) {
        T_sp predicate = oCdr(gc::As<Cons_sp>(type_predicates)->assoc(form,nil<core::T_O>(),cl::_sym_eq,nil<core::T_O>()));
        if ( predicate.notnilp() ) { return true; };
      }
    }
  }
  if ( form.consp() && core__proper_list_p(form) && oCar(form) == cl::_sym_member ) { return true; };
  if ( form.consp() && CONS_CAR(form) == cl::_sym_eql ) { return true; };
  if ( cl__symbolp(form) ) {
    if ( cl__find_class(form,false,nil<core::T_O>()).notnilp() ) return true;
  }
  return false;
}

/*! Canonicalize one declare.
* Arguments
- decl :: A list.
- canon :: A list.
* Description
Prepend the canonicalized_declarations from decl to canon - return the result. */
List_sp maybe_canonicalize_declaration(List_sp decl, List_sp canon)
{
  Symbol_sp sym = gc::As<Symbol_sp>(oCar(decl));
  T_sp too_many = oCddr(decl);
  if (too_many.notnilp()) {
    for ( auto sp : static_cast<List_sp>(oCdr(decl)) ) {
      canon = Cons_O::create(Cons_O::createList(sym,oCar(sp)),canon);
    }
  } else {
    canon = Cons_O::create(decl,canon);
  }
  return canon;
}

List_sp maybe_canonicalize_optimize_declaration( List_sp decl, List_sp canon )
{
  for ( auto cur : (List_sp)(oCdr(decl)) ) {
    T_sp d = oCar(cur);
    if (cl__symbolp(d)
        // We only canonicalize standard qualities in this way. With Cleavir
        // we define additional qualities which we want to leave to it.
        // See src/lisp/kernel/cleavir/policy.lisp
        && ((d == cl::_sym_speed)
            || (d == cl::_sym_space)
            || (d == cl::_sym_safety)
            || (d == cl::_sym_debug)
            || (d == cl::_sym_compilation_speed))) {
      canon = Cons_O::create(Cons_O::createList(cl::_sym_optimize,Cons_O::createList(d,core::clasp_make_fixnum(3))),canon);
    } else {
      canon = Cons_O::create(Cons_O::createList(cl::_sym_optimize,d),canon);
    }
  }
  return canon;
}

/*! Canonicalize the following declarations
dynamic-extent  ignore     optimize
ftype           inline     special
ignorable       notinline  type
And my own special one:    core:_sym_lambda_name
*/
DOCGROUP(clasp);
CL_DEFUN List_sp core__canonicalize_declarations(List_sp decls)
{
  List_sp canon = nil<T_O>();
  for ( auto decl : decls ) {
    Cons_sp d = gc::As<Cons_sp>(oCar(decl));
    T_sp head = oCar(d);
    if (head == cl::_sym_dynamic_extent
        || head == cl::_sym_ignore
        || head == cl::_sym_inline
        || head == cl::_sym_special
        || head == cl::_sym_ignorable
        || head == cl::_sym_notinline) {
      canon = maybe_canonicalize_declaration(d,canon);
    } else if ( head == cl::_sym_ftype ) {
      T_sp more_than_one = oCdddr(d);
      if (more_than_one.nilp()) {
        canon = Cons_O::create(d,canon);
      } else {
        T_sp ftype = oCadr(d);
        for ( auto fp : static_cast<List_sp>(oCddr(d)) ) {
          canon = Cons_O::create(Cons_O::createList(cl::_sym_ftype,ftype,oCar(fp)),canon);
        }
      }
    } else if ( head == cl::_sym_optimize ) {
      canon = maybe_canonicalize_optimize_declaration(d,canon);
    } else if ( head == core::_sym_lambdaName ) {
      canon = Cons_O::create(d,canon);
    } else if ( head == cl::_sym_type ) {
      T_sp more_than_one = oCdddr(d);
      if (more_than_one.nilp()) {
        canon = Cons_O::create(d,canon);
      } else {
        T_sp ttype = oCadr(d);
        for ( auto fp : static_cast<List_sp>(oCddr(d)) ) {
          canon = Cons_O::create(Cons_O::createList(cl::_sym_type,ttype,oCar(fp)),canon);
        }
      }
    } else if ( head == core::_sym_c_local ) {
      // Ignore
    } else if ( head == core::_sym_type_assertions ) {
      // Ignore
    } else if ( head == kw::_sym_read_only ) {
      // Ignore
    } else if ( head == ext::_sym_array_index ) {
      // Ignore
    } else if ( head == core::_sym_index ) {
      // Ignore
    } else if ( head == ext::_sym_check_arguments_type ) {
      // Ignore
    } else if ( head == ext::_sym_assume_no_errors ) {
      // Ignore
    } else if ( core__is_a_type(head) ) {
      for ( auto fp : static_cast<List_sp>(oCdr(d)) ) {
        canon = Cons_O::create(Cons_O::createList(cl::_sym_type,head,oCar(fp)),canon);
      }
    } else {
      canon = Cons_O::create(d,canon);
    }
  }
  return canon;
}



T_sp evaluate_lambda_list_form(T_sp form, T_sp env) {
  // TODO:: The code should be compiled and not interpreted
  //	TopLevelIHF stackFrame(my_thread->invocationHistoryStack(),form);
  return eval::evaluate(form, env);
}

void throw_if_not_destructuring_context(T_sp context) {
  if (context == cl::_sym_defmacro || cl::_sym_define_compiler_macro
      || context == cl::_sym_destructuring_bind)
    return;
  SIMPLE_ERROR("Lambda list is destructuring_bind but context does not support it context[{}]", _rep_(context));
}

/* Process llraw as a single-dispatch lambda-list.
       This means that one and only one of the required arguments must be a two element cons
       with the first element as the symbol to bind and the second the name of the
       class that this method dispatches on.
       Returns (values LLPROCESSED SINGLE_DISPATCH_SYMBOL SINGLE-DISPATCH-CLASS DISPATCH-INDEX)
       Where LLPROCESSED has replaced the required argument CONS of the form
       (SINGLE-DISPATCH-SYMBOL SINGLE-DISPATCH-CLASS) with SINGLE-DISPATCH-SYMBOL
       and SINGLE-DISPATCH-SYMBOL is the symbol that binds to the value whose class
       matches SINGLE-DISPATCH-CLASS.
       If allow_first_argument_default is true then the if no argument is specified as the
       dispatching argument then the first argument is used and SINGLE-DISPATCH-CLASS returns as nil.
    DISPATCH-INDEX is the index of the argument that is being dispatched on - it should never be anything
    other than zero but I'll let the caller decide.*/

string argument_mode_as_string(ArgumentMode mode) {
  switch (mode) {
  case required:
    return "required";
  case optional:
    return "optional";
  case dot_rest:
    return ".rest";
  case rest:
    return "rest";
  case va_rest:
    return "va-rest";
  case keyword:
    return "keyword";
  case aux:
    return "aux";
  case allowOtherKeys:
    return "allow-other-keys";
  }
  return "-unknownAddArgumentMode-";
}

bool switch_add_argument_mode(T_sp context, T_sp symbol, ArgumentMode &mode, T_sp &key_flag) {
  LOG("In switch_add_argument_mode argument is a symbol: {} {}" , _rep_(symbol) , symbol.get());
  switch (mode) {
  case required:
      LOG("Was in required mode");
      if (symbol == cl::_sym_AMPoptional) {
        mode = optional;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        mode = rest;
        goto NEWMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        mode = va_rest;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      break;
  case optional:
      LOG("Was in optional mode");
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        mode = rest;
        goto NEWMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        mode = va_rest;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      break;
  case rest:
      LOG("Was in rest mode");
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        goto BADMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      break;
  case va_rest:
      LOG("Was in va_rest mode");
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        goto BADMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      break;
  case keyword:
      LOG("Was in keyword mode");
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        goto BADMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        mode = allowOtherKeys;
        goto NEWMODE;
      }
      break;
  case allowOtherKeys:
      LOG("Did not recognize symbol({})" , _rep_(symbol));
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        goto BADMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      }
      break;
  case aux:
      LOG("Was in aux mode");
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        goto BADMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      }
      break;
  case dot_rest:
      if (symbol == cl::_sym_AMPoptional) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        goto BADMODE;
      } else if (symbol == core::_sym_AMPva_rest) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        goto BADMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        goto BADMODE;
      }

      break;
  };
  return false;
BADMODE:
  SIMPLE_ERROR("While in lambda-list mode {} encountered illegal symbol[{}]", argument_mode_as_string(mode) , _rep_(symbol));
NEWMODE:
  LOG("Switched to mode: {}" , argument_mode_as_string(mode));
  {
    switch (mode) {
    case keyword:
      key_flag = _lisp->_true();
    case allowOtherKeys:
      if (context == cl::_sym_define_modify_macro)
        goto ILLEGAL_MODE;
      break;
    case aux:
      if (context == cl::_sym_generic_function || context == cl::_sym_defsetf || context == cl::_sym_define_modify_macro)
        goto ILLEGAL_MODE;
      break;
    case dot_rest:
        if (!(context == cl::_sym_defmacro || cl::_sym_define_compiler_macro
              || context == cl::_sym_destructuring_bind || context == cl::_sym_deftype))
        goto ILLEGAL_MODE;
      break;
    default:
      break;
    }
  }
  return true;
ILLEGAL_MODE:
  SIMPLE_ERROR("Illegal mode {} for context[{}]", argument_mode_as_string(mode) , _rep_(context));
}

void throw_if_invalid_context(T_sp context) {
  if (context == cl::_sym_defmacro || context == cl::_sym_define_compiler_macro
      || context == cl::_sym_function || context == cl::_sym_method
      || context == cl::_sym_destructuring_bind || context == cl::_sym_deftype
      || context == _lisp->_true())
    return;
  printf("%s:%d context.raw_= %p     cl::_sym_destructuring_bind.raw_=%p\n",
         __FILE__, __LINE__, context.raw_(), cl::_sym_destructuring_bind.raw_());
  SIMPLE_ERROR("Illegal parse_lambda_list context[{}]", _rep_(context));
}

bool contextSupportsWhole(T_sp context) {
  if (context == cl::_sym_defmacro || context == cl::_sym_destructuring_bind
      || context == cl::_sym_define_compiler_macro || context == cl::_sym_deftype
      || context == cl::_sym_define_method_combination) {
    return true;
  }
  return false;
}

bool contextSupportsEnvironment(T_sp context) {
  if (context == cl::_sym_defmacro || cl::_sym_define_compiler_macro
      || context == cl::_sym_defsetf || context == cl::_sym_deftype) {
    return true;
  }
  return false;
}


/* Check to make sure target is not cl:T
 */
void checkTargetArgument(T_sp arg) {
  if (arg== cl::_sym_T) {
    SIMPLE_ERROR("The argument in a lambda list cannot be T");
  }
}


/*! Process the arguments and return the components
 * context may be: ordinary, macro, destructuring, deftype,
 * define_method_combination, defsetf  HOWEVER ECL>>clos/method.lisp:line 402 passes T!!!!
 * and determines the
 * valid sytax. The output is made of several values:
 *
 * VALUES(0) = (N req1 ... )			; required values
 * VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * VALUES(2) = rest-var				; rest-variable, if any
 * VALUES(3) = key-flag				; T if &key was supplied
 * VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * VALUES(6) = (aux1 init1 ... )		; auxiliary variables - pairs of auxN initN
 * Values(7) = whole-var			; whole-variable, if any
 * Values(8) = environment-var			; environment-variable, if any
 *
 * 1) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 *
 * Return true if bindings will be defined and false if not.
 */
bool parse_lambda_list(List_sp original_lambda_list,
                       T_sp context,
                       gctools::Vec0<RequiredArgument> &reqs,
                       gctools::Vec0<OptionalArgument> &optionals,
                       RestArgument &restarg,
                       T_sp &key_flag,
                       gctools::Vec0<KeywordArgument> &keys,
                       T_sp &allow_other_keys,
                       gctools::Vec0<AuxArgument> &auxs) {
  reqs.clear();
  optionals.clear();
  keys.clear();
  key_flag = nil<T_O>();
  auxs.clear();
  allow_other_keys = _lisp->_false();
  if (original_lambda_list.nilp())
    return false;
  throw_if_invalid_context(context);
  T_sp defaultDefault;
  // fix the default for &optional and &key that don't have a default specified
  if (context == cl::_sym_deftype)
    // Could stuff this list somewhere ahead of time to save a little consing,
    // but if we're here we're parsing deftype so whatever.
    defaultDefault = Cons_O::createList(cl::_sym_quote, cl::_sym__TIMES_);
  else defaultDefault = nil<T_O>();
  List_sp arguments = cl__copy_list(original_lambda_list);
  LOG("Argument handling mode starts in (required) - interpreting: {}" , _rep_(arguments));
  ArgumentMode add_argument_mode = required;
  restarg.clear();
  List_sp cur = arguments;
  while (cur.notnilp()) {
    LOG("Handing argument: {}" , _rep_(oCar(cur)));
    T_sp oarg = oCar(cur);
    if (cl__symbolp(oarg)) {
      T_sp sym = oarg;
      if (switch_add_argument_mode(context, sym, add_argument_mode, key_flag)) {
        if (add_argument_mode == allowOtherKeys) {
          allow_other_keys = _lisp->_true();
        }
        goto NEXT;
      }
    }
    //
    // Now put the argument symbol where it belongs
    //
    switch (add_argument_mode) {
    case required: {
      RequiredArgument required(oarg);
      reqs.push_back(required);
      //      printf("%s:%d   Required argument[%d] _ArgTarget@%p --> %p  array size=%d\n", __FILE__, __LINE__, reqs.size()-1, &(reqs.back()._ArgTarget.rawRef_()), reqs.back()._ArgTarget.raw_(), reqs.size());
      checkTargetArgument(oarg);
      break;
    }
    case optional: {
      T_sp sarg = nil<T_O>();
      T_sp defaultValue = defaultDefault;
      T_sp supplied = nil<T_O>();
      if ((oarg).consp()) {
        List_sp carg = oarg;
        LOG("Optional argument is a Cons: {}" , _rep_(carg));
        sarg = oCar(carg);
        if (oCdr(carg).notnilp()) {
          defaultValue = oCadr(carg);
          if (oCddr(carg).notnilp())
            supplied = oCaddr(carg);
        }
        LOG("Optional argument was a Cons_O[{}] with parts - symbol[{}] default[{}] supplied[{}]" , _rep_(carg) , _rep_(sarg) , _rep_(defaultValue) , _rep_(supplied));
      } else {
        sarg = oarg;
        LOG("Optional argument was a Symbol_O[{}]" , _rep_(sarg));
      }
      checkTargetArgument(sarg);
      LOG("Saving _OptionalArgument({}) default({}) supplied({})" , _rep_(sarg) , _rep_(defaultValue) , _rep_(supplied));
      OptionalArgument optional(sarg, defaultValue, supplied);
      optionals.push_back(optional);
      break;
    }
    case rest: {
      if (restarg.isDefined()) {
        SIMPLE_ERROR("Only one name is allowed after &rest - you have already defined: {}", restarg.asString());
      }
      checkTargetArgument(oarg);
      restarg.setTarget(oarg);
      restarg.VaRest = false;
      break;
    }
    case va_rest: {
      if (restarg.isDefined()) {
        SIMPLE_ERROR("Only one name is allowed after &rest - you have already defined: {}", restarg.asString());
      }
      checkTargetArgument(oarg);
      restarg.setTarget(oarg);
      restarg.VaRest = true;
      break;
    }
    case dot_rest: {
      if (oCdr(cur).notnilp()) {
        SIMPLE_ERROR("Lambda list dot followed by more than one argument");
      }
      checkTargetArgument(oarg);
      restarg.setTarget(oarg);
      goto DONE;
    }
    case keyword: {
      Symbol_sp keySymbol = nil<Symbol_O>();
      T_sp localTarget = nil<T_O>();
      T_sp defaultValue = defaultDefault;
      T_sp sensorSymbol = nil<T_O>();
      if (cl__symbolp(oarg)) {
        localTarget = oarg;
        keySymbol = gc::As<Symbol_sp>(localTarget)->asKeywordSymbol();
      } else if ((oarg).consp()) {
        List_sp carg = oarg;
        T_sp head = oCar(carg);
        if ((head).consp()) {
          List_sp namePart = head;
          keySymbol = gc::As<Symbol_sp>(oCar(namePart)); // This is the keyword name
          localTarget = oCadr(namePart); // this is the symbol to rename it to
        } else {
          localTarget = head;
          keySymbol = gc::As<Symbol_sp>(localTarget)->asKeywordSymbol();
        }
        //
        // Is there a default value?
        //
        if (oCdr(carg).notnilp()) {
          defaultValue = oCadr(carg);
          if (oCddr(carg).notnilp()) {
            sensorSymbol = oCaddr(carg);
          }
        }
      } else {
        SIMPLE_ERROR("key arguments must be symbol or cons");
      }
      LOG("Saving keyword({}) local({}) default({}) sensor({})" , _rep_(keySymbol) , _rep_(localTarget) , _rep_(defaultValue) , _rep_(sensorSymbol));
      checkTargetArgument(keySymbol);
      KeywordArgument keyed(keySymbol, localTarget, defaultValue, sensorSymbol);
      keys.push_back(keyed);
      break;
    }
    case allowOtherKeys:
      SIMPLE_ERROR("&allow-other-keys must be processed just after switch_add_argument_mode");
      break;
    case aux: {
      T_sp localSymbol = nil<T_O>();
      T_sp expression = nil<T_O>();
      if ((oarg).consp()) {
        List_sp carg = oarg;
        localSymbol = gc::As<Symbol_sp>(oCar(carg));
        //
        // Is there an expression
        //
        if (oCdr(carg).notnilp())
          expression = oCadr(carg);
      } else {
        localSymbol = oarg;
      }
      AuxArgument aux(localSymbol, expression);
      auxs.push_back(aux);
      break;
    }
    }
  NEXT:
    T_sp ocur = oCdr(cur);
    if (ocur.nilp() || (ocur).consp()) {
      // Advance to next element of the list
      cur = ocur;
      continue;
    }
    // This is a dotted list. The cdr must be a symbol and
    // treat it like &rest
    T_sp sarg = ocur;
    restarg.setTarget(sarg);
    break;
  }
DONE:
  LOG("Returning from parse_lambda_list");
  return true;
}

List_sp lexical_variable_names(gctools::Vec0<RequiredArgument> &reqs,
                               gctools::Vec0<OptionalArgument> &optionals,
                               RestArgument &restarg,
                               gctools::Vec0<KeywordArgument> &keys,
                               gctools::Vec0<AuxArgument> &auxs) {
  ql::list result;
  // required arguments  req = ( num req1 req2 ...)
  for (auto &it : reqs) result << it._ArgTarget;
  // optional arguments   opts = (num opt1 init1 flag1 ...)
  for (auto &it : optionals) {
    result << it._ArgTarget;
    if (it._Sensor._ArgTarget.notnilp()) {
      result << it._Sensor._ArgTarget;
    }
  }
  if (restarg._ArgTarget.notnilp()) {
    result << restarg._ArgTarget;
  }
  // optional arguments   keys = (num key1 var1 init1 flag1 ...)
  for (auto &it : keys) {
    result << it._ArgTarget;
    if (it._Sensor._ArgTarget.notnilp()) {
      result << it._Sensor._ArgTarget;
    }
  }
  //	    lauxs << make_fixnum((int)auxs.size());
  for (auto &it : auxs) {
    result << it._ArgTarget;
  }
  return result.cons();
}

CL_LAMBDA(vl context);
CL_DECLARE();
CL_DOCSTRING(R"dx(This is like ECL::process-lambda-list)dx");
CL_DOCSTRING_LONG(R"dx(Differences are auxs are returned as nil or a list of 2*n elements of the form (sym1 init1 sym2 init2 ...) In ECL they say you need to prepend the number of auxs - that breaks the destructure macro. ECL process-lambda-list says context may be MACRO, FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND but in ECL>>clos/method.lsp they pass T!!!)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__process_lambda_list(List_sp lambdaList, T_sp context) {
  
  gctools::Vec0<RequiredArgument> reqs;
  gctools::Vec0<OptionalArgument> optionals;
  gctools::Vec0<KeywordArgument> keys;
  gctools::Vec0<AuxArgument> auxs;
  RestArgument restarg;
  T_sp key_flag;
  T_sp allow_other_keys;
  T_sp decl_dict = nil<T_O>();
  parse_lambda_list(lambdaList,
                    context,
                    reqs,
                    optionals,
                    restarg,
                    key_flag,
                    keys,
                    allow_other_keys,
                    auxs);
  ql::list lreqs;
  { // required arguments  req = ( num req1 req2 ...)
    lreqs << make_fixnum((int)reqs.size());
    for (auto &it : reqs) {
      lreqs << it._ArgTarget;
    }
  }
  ql::list lopts;
  { // optional arguments   opts = (num opt1 init1 flag1 ...)
    lopts << make_fixnum((int)optionals.size());
    for (auto &it : optionals) {
      lopts << it._ArgTarget << it._Default << it._Sensor._ArgTarget;
    }
  }
  ql::list lkeys;
  { // optional arguments   keys = (num key1 var1 init1 flag1 ...)
    lkeys << make_fixnum((int)keys.size());
    for (auto &it : keys) {
      lkeys << it._Keyword << it._ArgTarget << it._Default << it._Sensor._ArgTarget;
    }
  }
  ql::list lauxs;
  if (auxs.size() != 0) { // auxes arguments   auxs = (num aux1 init1 ...)
    // !!!! The above is not true auxs = nil or (aux1 init1 aux2 init2)
    //	    lauxs << make_fixnum((int)auxs.size());
    for (auto &it : auxs) {
      lauxs << it._ArgTarget << it._Expression;
    }
  }
  T_sp tlreqs = lreqs.cons();
  return Values(tlreqs,
                lopts.cons(),
                restarg._ArgTarget,
                key_flag,
                lkeys.cons(),
                allow_other_keys,
                lauxs.cons(),
                _lisp->_boolean(restarg.VaRest));
};

// Given a lambda list, return a lambda list suitable for display purposes.
// This means only the external interface is required.
// No default values, no -p variables, no &aux.
T_sp lambda_list_for_name(T_sp raw_lambda_list) {
  gctools::Vec0<RequiredArgument> reqs;
  gctools::Vec0<OptionalArgument> optionals;
  gctools::Vec0<KeywordArgument> keys;
  gctools::Vec0<AuxArgument> auxs;
  RestArgument restarg;
  T_sp key_flag;
  T_sp allow_other_keys;
  parse_lambda_list(raw_lambda_list, cl::_sym_Function_O,
                    reqs, optionals, restarg,
                    key_flag, keys, allow_other_keys, auxs);
  ql::list result;
  for (auto &it : reqs) result << it._ArgTarget;
  if (optionals.size() > 0) {
    result << cl::_sym_AMPoptional;
    for (auto &it : optionals) result << it._ArgTarget;
  }
  if (restarg._ArgTarget.notnilp())
    result << cl::_sym_AMPrest << restarg._ArgTarget;
  if (key_flag.notnilp()) {
    result << cl::_sym_AMPkey;
    for (auto &it : keys) result << it._Keyword;
  }
  if (allow_other_keys.notnilp())
    result << cl::_sym_AMPallow_other_keys;
  return result.cons();
}

/*
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 * ------------------------------------------------------------
 */


/*! Trivial initializers are atomic values that aren't non-keyword symbols
*/
bool initializerIsTrivial(T_sp lambda_list, List_sp seen, T_sp initializer) {
  if (initializer.consp()) {
    Cons_sp cinit = gc::As_unsafe<Cons_sp>(initializer);
    for ( auto cur : (List_sp)seen ) {
      T_sp oseen = CONS_CAR(cur);
      if (cinit->memberEq(oseen).notnilp()) {
        if (!_lisp->_Roots._TheSystemIsUp) {
          printf("%s:%d:%s\n  In lambda-list %s\n    the initializer %s is a list\n    and references the seen symbol %s \n    - so it is not trivial and needs a ValueEnvironment_O\n", __FILE__, __LINE__, __FUNCTION__, _rep_(lambda_list).c_str(), _rep_(initializer).c_str(), _rep_(oseen).c_str() );
        }
        return false;
      }
    }
    return true;
  } else if (gc::IsA<Symbol_sp>(initializer)) {
    if (seen.consp()) {
      Cons_sp cseen = gc::As_unsafe<Cons_sp>(seen);
      Symbol_sp sdefault = gc::As_unsafe<Symbol_sp>(initializer);
      T_sp result = cseen->memberEq(sdefault);
      if (result.notnilp()) {
        if (!_lisp->_Roots._TheSystemIsUp) {
          printf("%s:%d:%s default initializer: %s\n   lambda-list: %s\n   references a seen symbol: %s\n    - needs a ValueEnvironment_O\n", __FILE__, __LINE__, __FUNCTION__, _rep_(sdefault).c_str(), _rep_(lambda_list).c_str(), _rep_(seen).c_str() );
        }
        return false;
      }
    }
  }
  return true;
}

T_mv process_single_dispatch_lambda_list(List_sp llraw, bool allow_first_argument_default_dispatcher) {
  List_sp llprocessed = cl__copy_list(llraw);
  Symbol_sp sd_symbol = nil<Symbol_O>();
  Symbol_sp sd_class = nil<Symbol_O>();
  int dispatchIndex = 0;
  int idx = 0;
  for (auto cur : llprocessed) {
    T_sp arg = oCar(cur);
    if (cl__symbolp(arg)) {
      // Originally checked if the symbol started with an ampersand.
      // Unfortunately, I don't fully understand the logic here.
      if (arg == cl::_sym_AMPoptional ||
          arg == cl::_sym_AMPrest || arg == cl::_sym_AMPbody ||
          arg == core::_sym_AMPva_rest ||
          arg == cl::_sym_AMPkey || arg == cl::_sym_AMPallow_other_keys ||
          arg == cl::_sym_AMPaux)
        break;
    } else if ((arg).consp()) {
      List_sp carg = arg;
      if (cl__length(carg) != 2) {
        SYMBOL_SC_(CorePkg, singleDispatchWrongNumberArgumentsError);
        SYMBOL_EXPORT_SC_(KeywordPkg, arguments);
        ERROR(_sym_singleDispatchWrongNumberArgumentsError,
              Cons_O::createList(kw::_sym_arguments, carg));
      }
      if (sd_symbol.notnilp()) {
        // There is already a sd_symbol defined -
        // This means there are too many specialized arguments
        SYMBOL_SC_(CorePkg, singleDispatchTooManyArgumentsError);
        ERROR(_sym_singleDispatchTooManyArgumentsError,
              Cons_O::createList(kw::_sym_arguments, llraw));
      }
      sd_symbol = gc::As<Symbol_sp>(oCar(carg));
      sd_class = gc::As<Symbol_sp>(oCadr(carg));
      cur->setCar(sd_symbol);
      dispatchIndex = idx;
    } else {
      SYMBOL_SC_(CorePkg, singleDispatchBadLambdaListError);
      ERROR(_sym_singleDispatchBadLambdaListError,
            Cons_O::createList(kw::_sym_arguments, llraw));
    }
    idx++;
  }
  if (sd_symbol.nilp()) {
    if (allow_first_argument_default_dispatcher) {
      T_sp car = oCar(llprocessed);
      sd_symbol = gc::As<Symbol_sp>(car);
    } else {
      SYMBOL_SC_(CorePkg, singleDispatchMissingDispatchArgumentError);
      ERROR(_sym_singleDispatchMissingDispatchArgumentError,
            Cons_O::createList(kw::_sym_arguments, llraw));
    }
  }
  T_sp tllprocessed = llprocessed;
  return (Values(tllprocessed, sd_symbol, sd_class, make_fixnum(dispatchIndex)));
}

/*! Pull the &whole and &environment variables out of the lambda_list and
      prefix them as required arguments.  If they weren't present then use gensym to
      generate temporary symbols */
List_sp process_macro_lambda_list(List_sp lambda_list) {
  List_sp new_lambda_list = cl__copy_list(lambda_list);
  Symbol_sp whole_symbol = nil<Symbol_O>();
  Symbol_sp environment_symbol = nil<Symbol_O>();
  if (oCar(new_lambda_list) == cl::_sym_AMPwhole) {
    whole_symbol = gc::As<Symbol_sp>(oCadr(new_lambda_list));
    new_lambda_list = oCddr(new_lambda_list);
  }
  if (oCar(new_lambda_list) == cl::_sym_AMPenvironment) {
    environment_symbol = gc::As<Symbol_sp>(oCadr(new_lambda_list));
    new_lambda_list = oCddr(new_lambda_list);
  } else {
    List_sp cur = oCdr(new_lambda_list);
    Cons_sp prev = new_lambda_list.asCons();
    for (; cur.notnilp(); prev = cur.asCons(), cur = oCdr(cur)) {
      if (oCar(cur) == cl::_sym_AMPenvironment) {
        environment_symbol = gc::As<Symbol_sp>(oCadr(cur));
        // remove the &environment from the lambda-list
        prev->setCdr(oCddr(cur));
        break;
      }
    }
  }
  if (whole_symbol.nilp())
    whole_symbol = cl__gensym(SimpleBaseString_O::make("whole"));
  if (environment_symbol.nilp())
    environment_symbol = cl__gensym(SimpleBaseString_O::make("environment"));

  Symbol_sp name_symbol = cl__gensym(SimpleBaseString_O::make("macro-name"));
  //	SourceCodeList_sp new_name_ll = SourceCodeCons_O::createWithDuplicateSourceCodeInfo(name_symbol,new_lambda_list,lambda_list,_lisp);
  ql::list sclist; // (af_lineNumber(lambda_list),af_column(lambda_list),core__file_scope(lambda_list));
  sclist << whole_symbol << environment_symbol << Cons_O::create(name_symbol, new_lambda_list);
  List_sp macro_ll = sclist.cons();
  return macro_ll;
}

CL_LAMBDA(lambda-list);
CL_DECLARE();
CL_DOCSTRING(R"dx(process_macro_lambda_list)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__process_macro_lambda_list(List_sp lambda_list) {
  List_sp new_ll = process_macro_lambda_list(lambda_list);
  return new_ll;
}

CL_LAMBDA(lambda-list);
CL_DECLARE();
CL_DOCSTRING(R"dx(process_single_dispatch_lambda_list)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__process_single_dispatch_lambda_list(List_sp lambda_list) {
  return process_single_dispatch_lambda_list(lambda_list);
}



  SYMBOL_SC_(CorePkg, process_macro_lambda_list);
  SYMBOL_SC_(CorePkg, process_single_dispatch_lambda_list);
  SYMBOL_SC_(CorePkg, processLambdaList);


}; /* core */
