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
#define DEBUG_LEVEL_NONE

#include <string.h>
#include <clasp/core/config.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/lispList.h>
#include <clasp/core/primitives.h>
#include <clasp/core/str.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/wrappers.h>
namespace core {

void lambdaListHandler_createBindings(core::FunctionClosure *closure, core::LambdaListHandler_sp llh, core::DynamicScopeManager &scope, LCC_ARGS) {
  // TODO: I should allocate this on the stack - but clang doesn't behave consistently
  // when I use variable stack arrays
  //        printf("%s:%d About to alloca with lcc_nargs = %zu\n", __FILE__, __LINE__, lcc_nargs);
  //        T_sp* args = (T_sp*)alloca(sizeof(T_sp)*lcc_nargs);
  MultipleValues *mvP = NULL;
  LCC_SWITCH_TO_COPY_PASSED_ARGS_INTO_MULTIPLE_VALUES_ARRAY(mvP);
  try {
    llh->createBindingsInScope_argArray(mvP->getSize(), mvP->callingArgsStart(), scope);
  } catch (...) {
    printf("%s:%d Caught an exception while in createBindingsInScope_argArray_TPtr\n", __FILE__, __LINE__);
    handleArgumentHandlingExceptions(closure);
  }
  //        printf("%s:%d returning from lambdaListHandler_createBindings\n", __FILE__, __LINE__);
  return;
}

T_sp evaluate_lambda_list_form(T_sp form, T_sp env) {
  // TODO:: The code should be compiled and not interpreted
  //	TopLevelIHF stackFrame(_lisp->invocationHistoryStack(),form);
  return eval::evaluate(form, env);
}

TargetClassifier::TargetClassifier(const std::set<int> &skip) : lexicalIndex(-1), skipLexicalIndices(skip) {
  _G();
  this->_SpecialSymbols = _Nil<SymbolSet_O>();
  this->_LambdaListSpecials = SymbolSet_O::create();
  this->advanceLexicalIndex();
};
TargetClassifier::TargetClassifier(SymbolSet_sp specialSymbols, const std::set<int> &skip)
    : _SpecialSymbols(specialSymbols), lexicalIndex(-1), skipLexicalIndices(skip) {
  this->_LambdaListSpecials = SymbolSet_O::create();
  this->advanceLexicalIndex();
};

void TargetClassifier::advanceLexicalIndex() {
  while (true) {
    ++this->lexicalIndex;
    if (this->skipLexicalIndices.count(this->lexicalIndex) == 0)
      return;
  }
}

/*! Any Special symbols that haven't been seen in the lambda list need to be added to the
      AccumulatedClassifiedSymbols */
Cons_sp TargetClassifier::finalClassifiedSymbols() {
  if (this->_SpecialSymbols.notnilp()) {
    this->_SpecialSymbols->map([this](Symbol_sp s) {
                    if ( !this->_LambdaListSpecials->contains(s) )
                    {
                        this->_AccumulatedClassifiedSymbols
                            << Cons_O::create(ext::_sym_specialVar,s);
                    }
    });
  }
  return this->_AccumulatedClassifiedSymbols.cons();
}

void throw_if_not_destructuring_context(T_sp context) {
  _G();
  if (context == _sym_macro || context == cl::_sym_destructuring_bind)
    return;
  SIMPLE_ERROR(BF("Lambda list is destructuring_bind but context does not support it context[%s]") % _rep_(context));
}

T_sp LambdaListHandler_O::lambdaList() {
  ql::list ll(_lisp);
  { // required arguments  req = ( num req1 req2 ...)
    for (gctools::Vec0<RequiredArgument>::const_iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); it++) {
      ll << it->_ArgTarget;
    }
  }
  if (this->_OptionalArguments.size() > 0) {
    ll << cl::_sym_AMPoptional;
    for (gctools::Vec0<OptionalArgument>::const_iterator it = this->_OptionalArguments.begin();
         it != this->_OptionalArguments.end(); it++) {
      if (it->_Sensor._ArgTarget.notnilp()) {
        Cons_sp one = Cons_O::createList(it->_ArgTarget, it->_Default, it->_Sensor._ArgTarget);
        ll << one;
      } else if (it->_Default.notnilp()) {
        Cons_sp one = Cons_O::createList(it->_ArgTarget, it->_Default);
        ll << one;
      } else {
        ll << it->_ArgTarget;
      }
    }
  }
  if (this->_RestArgument._ArgTarget.notnilp()) {
    ll << cl::_sym_AMPrest;
    ll << this->_RestArgument._ArgTarget;
  }
  if (this->_KeyFlag.notnilp() || this->_KeywordArguments.size() > 0) {
    ll << cl::_sym_AMPkey;
    for (gctools::Vec0<KeywordArgument>::const_iterator it = this->_KeywordArguments.begin();
         it != this->_KeywordArguments.end(); it++) {
      T_sp keywordTarget = it->_ArgTarget;
      if (it->_Keyword.as<Symbol_O>()->symbolName()->get() != keywordTarget.as<Symbol_O>()->symbolName()->get()) {
        keywordTarget = Cons_O::createList(it->_Keyword, keywordTarget);
      }
      if (it->_Sensor._ArgTarget.notnilp()) {
        Cons_sp one = Cons_O::createList(keywordTarget, it->_Default, it->_Sensor._ArgTarget);
        ll << one;
      } else if (it->_Default.notnilp()) {
        Cons_sp one = Cons_O::createList(keywordTarget, it->_Default);
        ll << one;
      } else {
        ll << keywordTarget;
      }
    }
  }
  if (this->_AllowOtherKeys.notnilp()) {
    ll << cl::_sym_AMPallow_other_keys;
  }
  return ll.cons();
}

SymbolSet_sp LambdaListHandler_O::identifySpecialSymbols(Cons_sp declareSpecifierList) {
  _G();
  SymbolSet_sp specials(SymbolSet_O::create());
  LOG(BF("Processing declareSpecifierList: %s") % declareSpecifierList->__repr__());
  if (declareSpecifierList.notnilp()) {
    ASSERTF(oCar(declareSpecifierList) != cl::_sym_declare, BF("The declareSpecifierList were not processed properly coming into this function - only declare specifiers should be passed - I got: %s") % _rep_(declareSpecifierList));
    for (Cons_sp cur = declareSpecifierList; cur.notnilp(); cur = cCdr(cur)) {
      T_sp entry = oCar(cur);
      if (cl_consp(entry) && oCar(entry) == cl::_sym_special) {
        for (Cons_sp spcur = cCdr(entry.as_or_nil<Cons_O>()); spcur.notnilp(); spcur = cCdr(spcur)) {
          specials->insert(oCar(spcur).as<Symbol_O>());
        }
      }
    }
  }
  LOG(BF("Returning symbolSet: %s") % specials->__repr__());
  return specials;
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

Cons_mv LambdaListHandler_O::process_single_dispatch_lambda_list(Cons_sp llraw, bool allow_first_argument_default_dispatcher) {
  _G();
  Cons_sp llprocessed = cl_copyList(llraw).as_or_nil<Cons_O>();
  Symbol_sp sd_symbol = _Nil<Symbol_O>();
  Symbol_sp sd_class = _Nil<Symbol_O>();
  bool saw_amp = false;
  int dispatchIndex = 0;
  int idx = 0;
  for (Cons_sp cur = llprocessed; cur.notnilp(); cur = cCdr(cur)) {
    T_sp arg = oCar(cur);
    if (af_symbolp(arg)) {
      if (arg.as<Symbol_O>()->amp_symbol_p()) {
        saw_amp = true;
        break;
      }
    } else if (cl_consp(arg)) {
      Cons_sp carg = arg.as_or_nil<Cons_O>();
      if (cl_length(carg) != 2) {
        SYMBOL_SC_(CorePkg, singleDispatchWrongNumberArgumentsError);
        SYMBOL_SC_(KeywordPkg, arguments);
        ERROR(_sym_singleDispatchWrongNumberArgumentsError,
              Cons_O::createList(kw::_sym_arguments, carg));
      }
      if (sd_symbol.notnilp()) {
        SYMBOL_SC_(CorePkg, singleDispatchTooManyArgumentsError);
        ERROR(_sym_singleDispatchTooManyArgumentsError,
              Cons_O::createList(kw::_sym_arguments, llraw));
      }
      sd_symbol = oCar(carg).as<Symbol_O>();
      sd_class = oCadr(carg).as<Symbol_O>();
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
      sd_symbol = oCar(llprocessed).as<Symbol_O>();
    } else {
      SYMBOL_SC_(CorePkg, singleDispatchMissingDispatchArgumentError);
      ERROR(_sym_singleDispatchMissingDispatchArgumentError,
            Cons_O::createList(kw::_sym_arguments, llraw));
    }
  }
  return (Values(llprocessed, sd_symbol, sd_class, Fixnum_O::create(dispatchIndex)));
}

/*! Pull the &whole and &environment variables out of the lambda_list and
      prefix them as required arguments.  If they weren't present then use gensym to
      generate temporary symbols */
Cons_sp LambdaListHandler_O::process_macro_lambda_list(Cons_sp lambda_list) {
  _G();
  Cons_sp new_lambda_list = cl_copyList(lambda_list).as_or_nil<Cons_O>();
  Symbol_sp whole_symbol = _Nil<Symbol_O>();
  Symbol_sp environment_symbol = _Nil<Symbol_O>();
  if (oCar(new_lambda_list) == cl::_sym_AMPwhole) {
    whole_symbol = oCadr(new_lambda_list).as<Symbol_O>();
    new_lambda_list = cCddr(new_lambda_list);
  }
  if (oCar(new_lambda_list) == cl::_sym_AMPenvironment) {
    environment_symbol = oCadr(new_lambda_list).as<Symbol_O>();
    new_lambda_list = cCddr(new_lambda_list);
  } else {
    Cons_sp cur = cCdr(new_lambda_list);
    Cons_sp prev = new_lambda_list;
    for (; cur.notnilp(); prev = cur, cur = cCdr(cur)) {
      if (oCar(cur) == cl::_sym_AMPenvironment) {
        environment_symbol = oCadr(cur).as<Symbol_O>();
        // remove the &environment from the lambda-list
        prev->setCdr(cCddr(cur));
        break;
      }
    }
  }
  if (whole_symbol.nilp())
    whole_symbol = af_gensym(Str_O::create("whole"));
  if (environment_symbol.nilp())
    environment_symbol = af_gensym(Str_O::create("environment"));

  Symbol_sp name_symbol = af_gensym(Str_O::create("macro-name"));
  //	SourceCodeCons_sp new_name_ll = SourceCodeCons_O::createWithDuplicateSourceCodeInfo(name_symbol,new_lambda_list,lambda_list,_lisp);
  ql::list sclist; // (af_lineNumber(lambda_list),af_column(lambda_list),core_sourceFileInfo(lambda_list));
  sclist << whole_symbol << environment_symbol << Cons_O::create(name_symbol, new_lambda_list);
  Cons_sp macro_ll = sclist.cons();
  if (_lisp->sourceDatabase().notnilp()) {
    _lisp->sourceDatabase()->duplicateSourcePosInfo(lambda_list, macro_ll);
  }
  return macro_ll;
}

#define DOCS_af_process_macro_lambda_list "process_macro_lambda_list"
#define LOCK_af_process_macro_lambda_list 1
#define ARGS_af_process_macro_lambda_list "(lambda-list)"
#define DECL_af_process_macro_lambda_list ""
Cons_mv af_process_macro_lambda_list(Cons_sp lambda_list) {
  _G();
  Cons_sp new_ll = LambdaListHandler_O::process_macro_lambda_list(lambda_list);
  return (Values(new_ll));
}

#define DOCS_af_process_single_dispatch_lambda_list "process_single_dispatch_lambda_list"
#define LOCK_af_process_single_dispatch_lambda_list 1
#define ARGS_af_process_single_dispatch_lambda_list "(lambda-list)"
#define DECL_af_process_single_dispatch_lambda_list ""
Cons_mv af_process_single_dispatch_lambda_list(Cons_sp lambda_list) {
  _G();
  return LambdaListHandler_O::process_single_dispatch_lambda_list(lambda_list);
}

void LambdaListHandler_O::initialize() {
  _OF();
  this->Base::initialize();
  this->_CreatesBindings = true;
  this->_DeclareSpecifierList = _Nil<Cons_O>();
  this->_RequiredArguments.clear();
  this->_OptionalArguments.clear();
  this->_RestArgument.clear();
  this->_KeyFlag = _Nil<T_O>();
  this->_KeywordArguments.clear();
  this->_AllowOtherKeys = _Nil<T_O>();
  this->_AuxArguments.clear();
}

void TargetClassifier::targetIsSubLambdaList(Argument &target, LambdaListHandler_sp subHandler) {
  target._ArgTarget = subHandler;
  target._ArgTargetFrameIndex = SUB_LAMBDA_LIST;
}

void TargetClassifier::classifyTarget(Argument &target) {
  Symbol_sp sym = target._ArgTarget.as<Symbol_O>();
  if (sym->specialP() || (this->_SpecialSymbols.notnilp() && this->_SpecialSymbols->contains(sym))) {
    target._ArgTargetFrameIndex = SPECIAL_TARGET;
    this->_AccumulatedClassifiedSymbols << Cons_O::create(ext::_sym_specialVar, target._ArgTarget);
    this->_LambdaListSpecials->insert(sym);
  } else {
    target._ArgTargetFrameIndex = this->lexicalIndex;
    this->_AccumulatedClassifiedSymbols << Cons_O::create(ext::_sym_heapVar, Cons_O::create(target._ArgTarget, Fixnum_O::create(target._ArgTargetFrameIndex)));
    this->advanceLexicalIndex();
  }
}

void LambdaListHandler_O::recursively_build_handlers_count_arguments(Cons_sp declares, T_sp context, TargetClassifier &classifier) {
  _G();
  { // required arguments
    for (gctools::Vec0<RequiredArgument>::iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); it++) {
      if (it->_lambdaListP()) {
        Cons_sp sub_lambda_list = it->lambdaList();
        //		    throw_if_not_destructuring_context(context);
        LambdaListHandler_sp sub_handler = LambdaListHandler_O::createRecursive(sub_lambda_list, declares, context, classifier);
        classifier.targetIsSubLambdaList(*it, sub_handler);
      } else {
        classifier.classifyTarget(*it);
      }
    }
  }
  { // optional arguments
    for (gctools::Vec0<OptionalArgument>::iterator it = this->_OptionalArguments.begin();
         it != this->_OptionalArguments.end(); it++) {
      if (it->_lambdaListP()) {
        //		    throw_if_not_destructuring_context(context);
        Cons_sp sub_lambda_list = it->lambdaList();
        LambdaListHandler_sp sub_handler = LambdaListHandler_O::createRecursive(sub_lambda_list, declares, context, classifier);
        classifier.targetIsSubLambdaList(*it, sub_handler);
      } else {
        classifier.classifyTarget(*it);
      }
      if (it->_Sensor.isDefined())
        classifier.classifyTarget(it->_Sensor);
    }
  }
  if (this->_RestArgument.isDefined()) {
    classifier.classifyTarget(this->_RestArgument);
  }
  { // keyed arguments
    for (gctools::Vec0<KeywordArgument>::iterator it = this->_KeywordArguments.begin();
         it != this->_KeywordArguments.end(); it++) {
      if (it->_lambdaListP()) {
        //		    throw_if_not_destructuring_context(context);
        Cons_sp sub_lambda_list = it->lambdaList();
        LambdaListHandler_sp sub_handler = LambdaListHandler_O::createRecursive(sub_lambda_list, declares, context, classifier);
        classifier.targetIsSubLambdaList(*it, sub_handler);
      } else {
        classifier.classifyTarget(*it);
      }
      if (it->_Sensor.isDefined())
        classifier.classifyTarget(it->_Sensor);
    }
  }

  { // aux arguments
    for (gctools::Vec0<AuxArgument>::iterator it = this->_AuxArguments.begin();
         it != this->_AuxArguments.end(); it++) {
      classifier.classifyTarget(*it);
    }
  }
}

SYMBOL_SC_(CorePkg, tooFewArguments);

// Setup argument binding for ActivationFrame
#define PASS_FUNCTION_REQUIRED bind_required_af
#define PASS_FUNCTION_OPTIONAL bind_optional_af
#define PASS_FUNCTION_REST bind_rest_af
#define PASS_FUNCTION_KEYWORD bind_keyword_af
#define PASS_ARGS ActivationFrame_sp args
#define PASS_ARGS_NUM cl_length(args)
#define PASS_NEXT_ARG() args->entry(arg_idx)
#include "argumentBinding.cc"
#undef PASS_FUNCTION_REQUIRED
#undef PASS_FUNCTION_OPTIONAL
#undef PASS_FUNCTION_REST
#undef PASS_FUNCTION_KEYWORD
#undef PASS_ARGS
#undef PASS_ARGS_NUM
#undef PASS_NEXT_ARG

#define PASS_FUNCTION_REQUIRED bind_required_argArray
#define PASS_FUNCTION_OPTIONAL bind_optional_argArray
#define PASS_FUNCTION_REST bind_rest_argArray
#define PASS_FUNCTION_KEYWORD bind_keyword_argArray
#define PASS_ARGS int n_args, ArgArray ap
#define PASS_ARGS_NUM n_args
#define PASS_NEXT_ARG() ap[arg_idx]
#include "argumentBinding.cc"
#undef PASS_FUNCTION_REQUIRED
#undef PASS_FUNCTION_OPTIONAL
#undef PASS_FUNCTION_REST
#undef PASS_FUNCTION_KEYWORD
#undef PASS_ARGS
#undef PASS_ARGS_NUM
#undef PASS_NEXT_ARG

#define PASS_FUNCTION_REQUIRED bind_required_argArray_TPtr
#define PASS_FUNCTION_OPTIONAL bind_optional_argArray_TPtr
#define PASS_FUNCTION_REST bind_rest_argArray_TPtr
#define PASS_FUNCTION_KEYWORD bind_keyword_argArray_TPtr
#define PASS_ARGS int n_args, T_O *ap[]
#define PASS_ARGS_NUM n_args
#define PASS_NEXT_ARG() gctools::smart_ptr<T_O>(ap[arg_idx])
#include "argumentBinding.cc"
#undef PASS_FUNCTION_REQUIRED
#undef PASS_FUNCTION_OPTIONAL
#undef PASS_FUNCTION_REST
#undef PASS_FUNCTION_KEYWORD
#undef PASS_ARGS
#undef PASS_ARGS_NUM
#undef PASS_NEXT_ARG

void bind_aux(gctools::Vec0<AuxArgument> const &auxs, DynamicScopeManager &scope) {
  _G();
  int num_auxs = auxs.size();
  if (num_auxs == 0)
    return;
  LOG(BF("There are %d aux variables") % auxs.size());
  gctools::Vec0<AuxArgument>::const_iterator ci;
  {
    _BLOCK_TRACE("Assigning aux variables");
    for (ci = auxs.begin(); ci != auxs.end(); ci++) {
      T_sp expr = ci->_Expression;
      if (expr.notnilp()) {
        T_sp value = evaluate_lambda_list_form(expr, scope.lexenv());
        scope.new_binding(*ci, value);
      } else {
        scope.new_binding(*ci, _Nil<T_O>());
      }
    }
  }
}

void LambdaListHandler_O::createBindingsInScope_af(
    ActivationFrame_sp args,
    DynamicScopeManager &scope) {
  _G();
  if (UNLIKELY(!this->_CreatesBindings))
    return;
  int arg_idx = 0;
  arg_idx = bind_required_af(this->_RequiredArguments, args, arg_idx, scope);
  arg_idx = bind_optional_af(this->_OptionalArguments, args, arg_idx, scope);
  if (UNLIKELY(arg_idx < cl_length(args) && (!this->_RestArgument.isDefined()) && (this->_KeywordArguments.size() == 0))) {
    throwTooManyArgumentsError(cl_length(args), this->numberOfLexicalVariables());
    //	    TOO_MANY_ARGUMENTS_ERROR();
  }
  bind_rest_af(this->_RestArgument, args, arg_idx, scope);
  bind_keyword_af(this->_KeywordArguments, this->_AllowOtherKeys, args, arg_idx, scope);
  bind_aux(this->_AuxArguments, scope);
}

void LambdaListHandler_O::createBindingsInScope_argArray(int n_args, ArgArray argArray,
                                                         DynamicScopeManager &scope) {
  _G();
  if (UNLIKELY(!this->_CreatesBindings))
    return;
  int arg_idx = 0;
  arg_idx = bind_required_argArray(this->_RequiredArguments, n_args, argArray, arg_idx, scope);
  arg_idx = bind_optional_argArray(this->_OptionalArguments, n_args, argArray, arg_idx, scope);
  if (UNLIKELY(arg_idx < n_args && (!this->_RestArgument.isDefined()) && (this->_KeywordArguments.size() == 0))) {
    throwTooManyArgumentsError(n_args, this->numberOfLexicalVariables());
    //	    TOO_MANY_ARGUMENTS_ERROR();
  }
  bind_rest_argArray(this->_RestArgument, n_args, argArray, arg_idx, scope);
  bind_keyword_argArray(this->_KeywordArguments, this->_AllowOtherKeys, n_args, argArray, arg_idx, scope);
  bind_aux(this->_AuxArguments, scope);
}

void LambdaListHandler_O::createBindingsInScope_argArray_TPtr(int n_args, T_O *argArray[],
                                                              DynamicScopeManager &scope) {
  _G();
  if (UNLIKELY(!this->_CreatesBindings))
    return;
  int arg_idx = 0;
  arg_idx = bind_required_argArray_TPtr(this->_RequiredArguments, n_args, argArray, arg_idx, scope);
  arg_idx = bind_optional_argArray_TPtr(this->_OptionalArguments, n_args, argArray, arg_idx, scope);
  if (UNLIKELY(arg_idx < n_args && (!this->_RestArgument.isDefined()) && (this->_KeywordArguments.size() == 0))) {
    throwTooManyArgumentsError(n_args, this->numberOfLexicalVariables());
    //	    TOO_MANY_ARGUMENTS_ERROR();
  }
  bind_rest_argArray_TPtr(this->_RestArgument, n_args, argArray, arg_idx, scope);
  bind_keyword_argArray_TPtr(this->_KeywordArguments, this->_AllowOtherKeys, n_args, argArray, arg_idx, scope);
  bind_aux(this->_AuxArguments, scope);
}

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
  _G();
  LOG(BF("In switch_add_argument_mode argument is a symbol: %s %X") % _rep_(symbol) % symbol.get());
  bool isAmpSymbol = false;
  if (symbol.notnilp()) {
    if (symbol.pointerp()) {
      if (Symbol_sp sym = symbol.asOrNull<Symbol_O>()) {
        isAmpSymbol = (sym == _sym_DOT || sym->amp_symbol_p());
      }
    }
  }
  //	bool isAmpSymbol = ( symbol == _sym_DOT || (symbol.notnilp() && symbol->amp_symbol_p()) );
  if (isAmpSymbol) {
    LOG(BF("It is an amp symbol"));
    switch (mode) {
    case required:
      LOG(BF("Was in required mode"));
      if (symbol == cl::_sym_AMPoptional) {
        mode = optional;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        mode = rest;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      goto BADMODE;
      break;
    case optional:
      LOG(BF("Was in optional mode"));
      if (symbol == cl::_sym_AMPrest || symbol == cl::_sym_AMPbody) {
        mode = rest;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      goto BADMODE;
      break;
    case rest:
      LOG(BF("Was in rest mode"));
      if (symbol == cl::_sym_AMPkey) {
        mode = keyword;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == _sym_DOT) {
        mode = dot_rest;
        goto NEWMODE;
      }
      goto BADMODE;
      break;
    case keyword:
      LOG(BF("Was in keyword mode"));
      if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      } else if (symbol == cl::_sym_AMPallow_other_keys) {
        mode = allowOtherKeys;
        goto NEWMODE;
      }
      goto BADMODE;
      break;
    case allowOtherKeys:
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      if (symbol == cl::_sym_AMPaux) {
        mode = aux;
        goto NEWMODE;
      }
      goto BADMODE;
      break;
    case aux:
      LOG(BF("Was in aux mode"));
      goto BADMODE;
      break;
    case dot_rest:
      goto BADMODE;
      break;
    };
  } else {
    LOG(BF("It is not an amp symbol"));
  }
  return false;
BADMODE:
  SIMPLE_ERROR(BF("While in lambda-list mode %s encountered illegal symbol[%s]") % argument_mode_as_string(mode) % _rep_(symbol));
NEWMODE:
  LOG(BF("Switched to mode: %s") % argument_mode_as_string(mode));
  {
    _BLOCK_TRACEF(BF("Checking if mode[%s] is valid for context[%s]") % argument_mode_as_string(mode) % context->__repr__());
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
      if (!(context == _sym_macro || context == cl::_sym_destructuring_bind || context == cl::_sym_deftype))
        goto ILLEGAL_MODE;
      break;
    default:
      break;
    }
  }
  return true;
ILLEGAL_MODE:
  SIMPLE_ERROR(BF("Illegal mode %s for context[%s]") % argument_mode_as_string(mode) % _rep_(context));
}

void throw_if_invalid_context(T_sp context) {
  _G();
  if (context == _sym_macro || context == cl::_sym_function || context == cl::_sym_method || context == cl::_sym_destructuring_bind || context == _lisp->_true())
    return;
  printf("%s:%d context.px_ref= %p     cl::_sym_destructuring_bind.px_ref=%p\n",
         __FILE__, __LINE__, context.px_ref(), cl::_sym_destructuring_bind.px_ref());
  SIMPLE_ERROR(BF("Illegal parse_lambda_list context[%s]") % _rep_(context));
}

bool contextSupportsWhole(T_sp context) {
  _G();
  if (context == _sym_macro || context == cl::_sym_destructuring_bind || context == cl::_sym_deftype || context == cl::_sym_define_method_combination) {
    return true;
  }
  return false;
}

bool contextSupportsEnvironment(T_sp context) {
  _G();
  if (context == _sym_macro || context == cl::_sym_defsetf || context == cl::_sym_deftype) {
    return true;
  }
  return false;
}

/*! Process the arguments and return the components
 * context may be: ordinary, macro, destructuring, deftype,
 * define_method_combination, defsetf  HOWEVER ECL>>clos/method.lsp:line 402 passes T!!!!
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
 * 1°) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2°) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3°) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 *
 * Return true if bindings will be defined and false if not.
 */
bool parse_lambda_list(Cons_sp original_lambda_list,
                       T_sp context,
                       gctools::Vec0<RequiredArgument> &reqs,
                       gctools::Vec0<OptionalArgument> &optionals,
                       RestArgument &restarg,
                       T_sp &key_flag,
                       gctools::Vec0<KeywordArgument> &keys,
                       T_sp &allow_other_keys,
                       gctools::Vec0<AuxArgument> &auxs) {
  _G();
  reqs.clear();
  optionals.clear();
  keys.clear();
  key_flag = _Nil<T_O>();
  auxs.clear();
  allow_other_keys = _lisp->_false();
  if (original_lambda_list.nilp())
    return false;
  throw_if_invalid_context(context);
  Cons_sp arguments = cl_copyList(original_lambda_list).as_or_nil<Cons_O>();
  LOG(BF("Argument handling mode starts in (required) - interpreting: %s") % arguments->__repr__());
  ArgumentMode add_argument_mode = required;
  restarg.clear();
  Cons_sp cur = arguments;
  while (cur.notnilp()) {
    LOG(BF("Handing argument: %s") % _rep_(oCar(cur)));
    T_sp oarg = oCar(cur);
    if (af_symbolp(oarg)) {
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
      break;
    }
    case optional: {
      T_sp sarg = _Nil<T_O>();
      T_sp defaultValue = _Nil<T_O>();
      T_sp supplied = _Nil<T_O>();
      if (cl_consp(oarg)) {
        Cons_sp carg = oarg.as_or_nil<Cons_O>();
        LOG(BF("Optional argument is a Cons: %s") % carg->__repr__());
        sarg = oCar(carg);
        if (cCdr(carg).notnilp()) {
          defaultValue = oCadr(carg);
          if (cCddr(carg).notnilp()) {
            supplied = oCaddr(carg);
          }
        }
        LOG(BF("Optional argument was a Cons_O[%s] with parts - symbol[%s] default[%s] supplied[%s]") % carg->__repr__() % sarg->__repr__() % defaultValue->__repr__() % supplied->__repr__());
      } else {
        sarg = oarg;
        LOG(BF("Optional argument was a Symbol_O[%s]") % sarg->__repr__());
      }

      LOG(BF("Saving _OptionalArgument(%s) default(%s) supplied(%s)") % sarg->__repr__() % defaultValue->__repr__() % supplied->__repr__());
      OptionalArgument optional(sarg, defaultValue, supplied);
      optionals.push_back(optional);
      break;
    }
    case rest: {
      if (restarg.isDefined()) {
        SIMPLE_ERROR(BF("Only one name is allowed after &rest - you have already defined: ") % restarg.asString());
      }
      if (Symbol_sp sarg = oarg.asOrNull<Symbol_O>()) {
        LOG(BF("Saving _Rest argument: %s") % sarg->__repr__());
        restarg.setTarget(sarg);
      } else if (Cons_sp carg = oarg.asOrNull<Cons_O>()) {
        restarg.setTarget(carg);
      }
      break;
    }
    case dot_rest: {
      if (cCdr(cur).notnilp()) {
        SIMPLE_ERROR(BF("Lambda list dot followed by more than one argument"));
      }
      if (Symbol_sp sarg = oarg.asOrNull<Symbol_O>()) {
        LOG(BF("Saving _Rest argument: %s") % sarg->__repr__());
        restarg.setTarget(sarg);
      } else if (Cons_sp carg = oarg.asOrNull<Cons_O>()) {
        restarg.setTarget(carg);
      }
      goto DONE;
    }
    case keyword: {
      Symbol_sp keySymbol = _Nil<Symbol_O>();
      T_sp localTarget = _Nil<T_O>();
      T_sp defaultValue = _Nil<T_O>();
      T_sp sensorSymbol = _Nil<T_O>();
      if (af_symbolp(oarg)) {
        localTarget = oarg;
        keySymbol = localTarget.as<Symbol_O>()->asKeywordSymbol();
      } else if (cl_consp(oarg)) {
        Cons_sp carg = oarg.as_or_nil<Cons_O>();
        T_sp head = oCar(carg);
        if (cl_consp(head)) {
          Cons_sp namePart = head.as_or_nil<Cons_O>();
          keySymbol = oCar(namePart).as<Symbol_O>(); // This is the keyword name
          if (!keySymbol->isKeywordSymbol()) {
            SIMPLE_ERROR(BF("With key arguments of the form ((:x y) ...) the first argument must be a keyword symbol - you gave: ") % _rep_(keySymbol));
          }
          localTarget = oCadr(namePart); // this is the symbol to rename it to
        } else {
          localTarget = head;
          keySymbol = localTarget.as<Symbol_O>()->asKeywordSymbol();
        }

        //
        // Is there a default value?
        //
        if (cCdr(carg).notnilp()) {
          defaultValue = oCadr(carg);
          if (cCddr(carg).notnilp()) {
            sensorSymbol = oCaddr(carg);
          }
        }
      } else {
        SIMPLE_ERROR(BF("key arguments must be symbol or cons"));
      }
      LOG(BF("Saving keyword(%s) local(%s) default(%s) sensor(%s)") % keySymbol->__repr__() % localTarget->__repr__() % defaultValue->__repr__() % sensorSymbol->__repr__());
      KeywordArgument keyed(keySymbol, localTarget, defaultValue, sensorSymbol);
      keys.push_back(keyed);
      break;
    }
    case allowOtherKeys:
      SIMPLE_ERROR(BF("&allow-other-keys must be processed just after switch_add_argument_mode"));
      break;
    case aux: {
      T_sp localSymbol = _Nil<T_O>();
      T_sp expression = _Nil<T_O>();
      if (cl_consp(oarg)) {
        Cons_sp carg = oarg.as_or_nil<Cons_O>();
        localSymbol = oCar(carg).as<Symbol_O>();
        //
        // Is there an expression
        //
        if (cCdr(carg).notnilp())
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
    if (ocur.nilp() || cl_consp(ocur)) {
      // Advance to next element of the list
      cur = ocur.as_or_nil<Cons_O>();
      continue;
    }
    // This is a dotted list. The cdr must be a symbol and
    // treat it like &rest
    T_sp sarg = ocur;
    restarg.setTarget(sarg);
    break;
  }
DONE:
  return true;
}

#define ARGS_af_processLambdaList "(vl context)"
#define DECL_af_processLambdaList ""
#define DOCS_af_processLambdaList "processLambdaList - this is like ECL::process-lambda-list except auxs are returned as nil or a list of 2*n elements of the form (sym1 init1 sym2 init2 ...) In ECL they say you need to prepend the number of auxs - that breaks the destructure macro. ECL process-lambda-list says context may be MACRO, FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND but in ECL>>clos/method.lsp they pass T!!!"
Cons_mv af_processLambdaList(Cons_sp lambdaList, T_sp context) {
  _G();
  gctools::Vec0<RequiredArgument> reqs;
  gctools::Vec0<OptionalArgument> optionals;
  gctools::Vec0<KeywordArgument> keys;
  gctools::Vec0<AuxArgument> auxs;
  RestArgument restarg;
  T_sp key_flag;
  T_sp allow_other_keys;
  parse_lambda_list(lambdaList,
                    context,
                    reqs,
                    optionals,
                    restarg,
                    key_flag,
                    keys,
                    allow_other_keys,
                    auxs);
  ql::list lreqs(_lisp);
  { // required arguments  req = ( num req1 req2 ...)
    lreqs << Fixnum_O::create((int)reqs.size());
    for (auto &it : reqs) {
      lreqs << it._ArgTarget;
    }
  }
  ql::list lopts(_lisp);
  { // optional arguments   opts = (num opt1 init1 flag1 ...)
    lopts << Fixnum_O::create((int)optionals.size());
    for (auto &it : optionals) {
      lopts << it._ArgTarget << it._Default << it._Sensor._ArgTarget;
    }
  }
  ql::list lkeys(_lisp);
  { // optional arguments   keys = (num key1 var1 init1 flag1 ...)
    lkeys << Fixnum_O::create((int)keys.size());
    for (auto &it : keys) {
      lkeys << it._Keyword << it._ArgTarget << it._Default << it._Sensor._ArgTarget;
    }
  }
  ql::list lauxs(_lisp);
  if (auxs.size() != 0) { // auxes arguments   auxs = (num aux1 init1 ...)
    // !!!! The above is not true auxs = nil or (aux1 init1 aux2 init 2)
    //	    lauxs << Fixnum_O::create((int)auxs.size());
    for (auto &it : auxs) {
      lauxs << it._ArgTarget << it._Expression;
    }
  }
  return (Values(lreqs.cons(),
                 lopts.cons(),
                 restarg._ArgTarget,
                 key_flag,
                 lkeys.cons(),
                 allow_other_keys,
                 lauxs.cons()));
};

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

LambdaListHandler_sp LambdaListHandler_O::createRecursive(Cons_sp lambda_list, Cons_sp declares, T_sp context, TargetClassifier &classifier) {
  _G();
  GC_ALLOCATE(LambdaListHandler_O, llh);
  llh->parse_lambda_list_declares(lambda_list, declares, context, classifier);
  return llh;
}

LambdaListHandler_sp LambdaListHandler_O::create(Cons_sp lambda_list, Cons_sp declares, T_sp context, const std::set<int> &skipFrameIndices) {
  _G();
  ql::list all_arguments_list;
  SymbolSet_sp specialSymbols(LambdaListHandler_O::identifySpecialSymbols(declares));
  TargetClassifier classifier(specialSymbols, skipFrameIndices);
  LambdaListHandler_sp ollh = LambdaListHandler_O::createRecursive(lambda_list, declares, context, classifier);
  ollh->_NumberOfLexicalVariables = classifier.totalLexicalVariables();
  return ollh;
}

LambdaListHandler_sp LambdaListHandler_O::create(int numArgs, const std::set<int> &skipIndices) {
  _G();
  GC_ALLOCATE(LambdaListHandler_O, ollh);
  ollh->create_required_arguments(numArgs, skipIndices);
  return ollh;
}

#define ARGS_LambdaListHandler_O_makeLambdaListHandler "(lambda-list &optional declares (context 'ordinary))"
#define DECL_LambdaListHandler_O_makeLambdaListHandler ""
#define DOCS_LambdaListHandler_O_makeLambdaListHandler "makeLambdaListHandler"
LambdaListHandler_sp LambdaListHandler_O::makeLambdaListHandler(Cons_sp lambda_list, Cons_sp declares, T_sp context) {
  _G();
  LambdaListHandler_sp llh = LambdaListHandler_O::create(lambda_list, declares, context);
  return llh;
}

void LambdaListHandler_O::create_required_arguments(int num, const std::set<int> &skipIndices) {
  _OF();
  TargetClassifier classifier(skipIndices);
  for (int i = 0, iEnd(num - skipIndices.size()); i < iEnd; ++i) {
    Symbol_sp name = af_gensym(Str_O::create("arg"));
    RequiredArgument req(name, i);
    this->_RequiredArguments.push_back(req);
    classifier.classifyTarget(req);
  }
  this->_ClassifiedSymbolList = classifier.finalClassifiedSymbols();
  ASSERTF(this->_ClassifiedSymbolList.nilp() || cl_consp(oCar(this->_ClassifiedSymbolList)), BF("LambdaListHandler _classifiedSymbols must contain only conses - it contains %s") % _rep_(this->_ClassifiedSymbolList));
  this->_NumberOfLexicalVariables = classifier.totalLexicalVariables();
}

void LambdaListHandler_O::parse_lambda_list_declares(Cons_sp lambda_list, Cons_sp declareSpecifierList, T_sp context, TargetClassifier &classifier) {
  _OF();
  T_sp whole;
  Symbol_sp environment;
  this->_DeclareSpecifierList = declareSpecifierList;
  this->_CreatesBindings = parse_lambda_list(lambda_list,
                                             context,
                                             this->_RequiredArguments,
                                             this->_OptionalArguments,
                                             this->_RestArgument,
                                             this->_KeyFlag,
                                             this->_KeywordArguments,
                                             this->_AllowOtherKeys,
                                             this->_AuxArguments);
  if (this->_CreatesBindings) {
    this->recursively_build_handlers_count_arguments(declareSpecifierList, context, classifier);
    this->_ClassifiedSymbolList = classifier.finalClassifiedSymbols();
    ASSERTF(this->_ClassifiedSymbolList.nilp() || cl_consp(oCar(this->_ClassifiedSymbolList)), BF("LambdaListHandler _classifiedSymbols must contain only conses - it contains %s") % _rep_(this->_ClassifiedSymbolList));
  } else {
    this->_ClassifiedSymbolList = _Nil<Cons_O>();
  }
}

int LambdaListHandler_O::single_dispatch_on_argument(Symbol_sp target) {
  _G();
  for (auto &ri : this->_RequiredArguments) {
    if (ri._symbolP()) {
      if (ri.symbol() == target)
        return ri.targetFrameIndex();
    } else {
      SIMPLE_ERROR(BF("Required arguments of single dispatch generic functions must be symbols - I got: %s") % ri.asString());
    }
  }
  SIMPLE_ERROR(BF("Could not find single dispatch target symbol[%s]") % _rep_(target));
#if 0
	// Slide all the frameIndexPointers up one
	for ( int i=0; i<arguments.size()-1; i++ )
	{
	    ASSERTF((*arguments[i])==i,BF("FrameIndex %d is out of sequence") % i);
	    (*arguments[i]) = (*arguments[i])+1;
	}
	// Now move the FrameIndex associated with target (the last one) to zero
	int single_dispatch_argument_index = *(arguments.back());
	*(arguments.back()) = 0;
	return single_dispatch_argument_index;
#endif
}

string LambdaListHandler_O::partsAsString() const {
  _OF();
  stringstream ss;
  if (!this->_ClassifiedSymbolList.pointerp()) {
    ss << " :ClassifiedSymbols UNDEFINED ";
  } else {
    ss << " :ClassifiedSymbols " << _rep_(this->_ClassifiedSymbolList)
       << " ";
  }
  if (this->_RequiredArguments.size() > 0) {
    ss << " &required ";
    ss << asString(this->_RequiredArguments);
  }
  if (this->_OptionalArguments.size() > 0) {
    ss << " &optional ";
    ss << asString(this->_OptionalArguments);
  }
  if (this->_RestArgument.isDefined()) {
    ss << " &rest ";
    ss << this->_RestArgument.asString();
  }
  if (this->_KeyFlag.notnilp()) {
    ss << " &key ";
  }
  if (this->_KeywordArguments.size() > 0) {
    ss << asString(this->_KeywordArguments);
  }
  if (this->_AllowOtherKeys.isTrue()) {
    ss << " &allowOtherKeys ";
    ss << _rep_(this->_AllowOtherKeys);
  }
  if (this->_AuxArguments.size() > 0) {
    ss << " &aux ";
    ss << asString(this->_AuxArguments);
  }
  return ss.str();
}

string LambdaListHandler_O::__repr__() const {
  _OF();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString();
  {
    ss << this->partsAsString();
  }
  ss << " :comment \"" << this->_Comment.c_str() << "\"";
  ss << "> ";
  return ss.str();
}

Cons_mv LambdaListHandler_O::processLambdaListHandler() const {
  _G();

  ql::list reqs(_lisp);
  { // required arguments  req = ( num req1 req2 ...)
    reqs << Fixnum_O::create((int)this->_RequiredArguments.size());
    for (gctools::Vec0<RequiredArgument>::const_iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); it++) {
      reqs << it->classified();
    }
  }
  ql::list opts(_lisp);
  { // optional arguments   opts = (num opt1 init1 flag1 ...)
    opts << Fixnum_O::create((int)this->_OptionalArguments.size());
    for (gctools::Vec0<OptionalArgument>::const_iterator it = this->_OptionalArguments.begin();
         it != this->_OptionalArguments.end(); it++) {
      opts << it->classified() << it->_Default << it->_Sensor.classified();
    }
  }
  ql::list keys(_lisp);
  bool keyFlag = this->_KeyFlag.notnilp();
  { // optional arguments   keys = (num key1 var1 init1 flag1 ...)
    keys << Fixnum_O::create((int)this->_KeywordArguments.size());
    for (gctools::Vec0<KeywordArgument>::const_iterator it = this->_KeywordArguments.begin();
         it != this->_KeywordArguments.end(); it++) {
      keys << it->_Keyword << it->classified() << it->_Default << it->_Sensor.classified();
      keyFlag = true;
    }
  }
  ql::list auxs(_lisp);
  { // auxes arguments   auxs = (num aux1 init1 ...)
    auxs << Fixnum_O::create((int)this->_AuxArguments.size());
    for (gctools::Vec0<AuxArgument>::const_iterator it = this->_AuxArguments.begin();
         it != this->_AuxArguments.end(); it++) {
      auxs << it->classified() << it->_Expression;
    }
  }
  return (Values(reqs.cons(),
                 opts.cons(),
                 this->_RestArgument.classified(),
                 _lisp->_boolean(keyFlag),
                 keys.cons(),
                 this->_AllowOtherKeys,
                 auxs.cons()));
}

bool LambdaListHandler_O::requiredLexicalArgumentsOnlyP() const {
  _G();
  bool requiredArgumentsOnlyP = (this->_OptionalArguments.size() == 0) && (!this->_RestArgument.isDefined()) && (this->_KeywordArguments.size() == 0) && (!this->_AllowOtherKeys.isTrue()) && (this->_AuxArguments.size() == 0);
  if (requiredArgumentsOnlyP) {
    for (gctools::Vec0<RequiredArgument>::const_iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); it++) {
      if (!it->targetIsLexical())
        return false;
    }
    return true;
  }
  return false;
}

Cons_sp LambdaListHandler_O::namesOfLexicalVariables() const {
  _G();
  Cons_sp namesRev = _Nil<Cons_O>();
  for (Cons_sp cur = this->_ClassifiedSymbolList; cur.notnilp(); cur = cCdr(cur)) {
    if (oCar(oCar(cur)) == ext::_sym_heapVar) {
      namesRev = Cons_O::create(oCadr(oCar(cur)), namesRev);
    }
  }
  return cl_nreverse(namesRev).as_or_nil<Cons_O>();
}

void LambdaListHandler_O::calculateNamesOfLexicalVariablesForDebugging() {
  Cons_sp names = this->namesOfLexicalVariables();
  this->_LexicalVariableNamesForDebugging = VectorObjects_O::make(_Nil<T_O>(), names, cl_length(names), false);
}

VectorObjects_sp LambdaListHandler_O::namesOfLexicalVariablesForDebugging() {
  _G();
  if (this->_LexicalVariableNamesForDebugging.nilp()) {
    this->calculateNamesOfLexicalVariablesForDebugging();
  }
  return this->_LexicalVariableNamesForDebugging;
}

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, LambdaListHandler_O);
LambdaListHandler_O::LambdaListHandler_O() : _SpecialSymbolSet(_Nil<SymbolSet_O>()), _LexicalVariableNamesForDebugging(_Nil<VectorObjects_O>()){};
LambdaListHandler_O::~LambdaListHandler_O(){};

void LambdaListHandler_O::exposeCando(Lisp_sp lisp) {
  _G();
  class_<LambdaListHandler_O>()
      .def("single-dispatch-on-argument", &LambdaListHandler_O::single_dispatch_on_argument)
      .def("classifiedSymbols", &LambdaListHandler_O::classifiedSymbols)
      .def("processLambdaListHandler", &LambdaListHandler_O::processLambdaListHandler)
      .def("lambdaListHandlerRequiredLexicalArgumentsOnlyP", &LambdaListHandler_O::requiredLexicalArgumentsOnlyP)
      .def("numberOfRequiredArguments", &LambdaListHandler_O::numberOfRequiredArguments)
      .def("numberOfOptionalArguments", &LambdaListHandler_O::numberOfOptionalArguments)
      .def("numberOfRestArguments", &LambdaListHandler_O::numberOfRestArguments)
      .def("numberOfKeyArguments", &LambdaListHandler_O::numberOfKeyArguments)
      .def("numberOfAuxArguments", &LambdaListHandler_O::numberOfAuxArguments)
      .def("allowOtherKeys", &LambdaListHandler_O::allowOtherKeys)
      .def("numberOfLexicalVariables", &LambdaListHandler_O::numberOfLexicalVariables)
      .def("namesOfLexicalVariables", &LambdaListHandler_O::namesOfLexicalVariables)
      .def("namesOfLexicalVariablesForDebugging", &LambdaListHandler_O::namesOfLexicalVariablesForDebugging)
      .def("LambdaListHandler-lambdaList", &LambdaListHandler_O::lambdaList);
  SYMBOL_SC_(CorePkg, process_macro_lambda_list);
  Defun(process_macro_lambda_list);
  SYMBOL_SC_(CorePkg, process_single_dispatch_lambda_list);
  Defun(process_single_dispatch_lambda_list);
  af_def(CorePkg, "makeLambdaListHandler", &LambdaListHandler_O::makeLambdaListHandler, ARGS_LambdaListHandler_O_makeLambdaListHandler, DECL_LambdaListHandler_O_makeLambdaListHandler, DOCS_LambdaListHandler_O_makeLambdaListHandler);
  SYMBOL_SC_(CorePkg, makeLambdaListHandler);
  SYMBOL_SC_(CorePkg, processLambdaList);
  Defun(processLambdaList);
}

void LambdaListHandler_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, LambdaListHandler, "", "", _lisp);
#endif
}

}; /* core */
