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
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
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
SYMBOL_EXPORT_SC_(KeywordPkg, givenNumberOfArguments);
SYMBOL_EXPORT_SC_(KeywordPkg, requiredNumberOfArguments);
SYMBOL_EXPORT_SC_(KeywordPkg, unrecognizedKeyword);

void handleArgumentHandlingExceptions(Closure_sp closure) {
  Function_sp func = closure;
  try {
    throw;
  } catch (TooManyArgumentsError &error) {
    lisp_error(core::_sym_tooManyArgumentsError, lisp_createList(kw::_sym_calledFunction, func, kw::_sym_givenNumberOfArguments, make_fixnum(error.givenNumberOfArguments), kw::_sym_requiredNumberOfArguments, make_fixnum(error.requiredNumberOfArguments)));
  } catch (TooFewArgumentsError &error) {
    lisp_error(core::_sym_tooFewArgumentsError, lisp_createList(kw::_sym_calledFunction, func, kw::_sym_givenNumberOfArguments, make_fixnum(error.givenNumberOfArguments), kw::_sym_requiredNumberOfArguments, make_fixnum(error.requiredNumberOfArguments)));
  } catch (UnrecognizedKeywordArgumentError &error) {
    lisp_error(core::_sym_unrecognizedKeywordArgumentError, lisp_createList(kw::_sym_calledFunction, func, kw::_sym_unrecognizedKeyword, error.argument));
  }
}


/*! Return true if the form represents a type
*/
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
        T_sp predicate = oCdr(gc::As<Cons_sp>(type_predicates)->assoc(form,_Nil<core::T_O>(),cl::_sym_eq,_Nil<core::T_O>()));
        if ( predicate.notnilp() ) { return true; };
      }
    }
  }
  if ( form.consp() && core__proper_list_p(form) && oCar(form) == cl::_sym_member ) { return true; };
  if ( form.consp() && CONS_CAR(form) == cl::_sym_eql ) { return true; };
  if ( cl__symbolp(form) ) {
    if ( cl__find_class(form,false,_Nil<core::T_O>()).notnilp() ) return true;
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
  Symbol_sp sym = oCar(decl);
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
    if ( cl__symbolp(d) ) {
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
CL_DEFUN List_sp canonicalize_declarations(List_sp decls)
{
  List_sp canon = _Nil<T_O>();
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
        T_sp ftype = oCadr(d);
        for ( auto fp : static_cast<List_sp>(oCddr(d)) ) {
          canon = Cons_O::create(Cons_O::createList(cl::_sym_ftype,ftype,oCar(fp)),canon);
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


void lambdaListHandler_createBindings(Closure_sp closure, core::LambdaListHandler_sp llh, core::DynamicScopeManager &scope, LCC_ARGS_LLH) {
  if (llh->requiredLexicalArgumentsOnlyP()) {
    size_t numReq = llh->numberOfRequiredArguments();
    if (numReq <= LCC_ARGS_IN_REGISTERS && numReq == lcc_nargs) {
      switch (numReq) {
      case 4:
        scope.new_binding(llh->_RequiredArguments[3], T_sp((gc::Tagged)lcc_fixed_arg3));
        scope.new_binding(llh->_RequiredArguments[2], T_sp((gc::Tagged)lcc_fixed_arg2));
        scope.new_binding(llh->_RequiredArguments[1], T_sp((gc::Tagged)lcc_fixed_arg1));
        scope.new_binding(llh->_RequiredArguments[0], T_sp((gc::Tagged)lcc_fixed_arg0));
        return;
      case 3:
        scope.new_binding(llh->_RequiredArguments[2], T_sp((gc::Tagged)lcc_fixed_arg2));
        scope.new_binding(llh->_RequiredArguments[1], T_sp((gc::Tagged)lcc_fixed_arg1));
        scope.new_binding(llh->_RequiredArguments[0], T_sp((gc::Tagged)lcc_fixed_arg0));
        return;
      case 2:
        scope.new_binding(llh->_RequiredArguments[1], T_sp((gc::Tagged)lcc_fixed_arg1));
        scope.new_binding(llh->_RequiredArguments[0], T_sp((gc::Tagged)lcc_fixed_arg0));
        return;
      case 1:
        scope.new_binding(llh->_RequiredArguments[0], T_sp((gc::Tagged)lcc_fixed_arg0));
        return;
      }
      return;
    }
  }
  try {
    llh->createBindingsInScopeVaList(lcc_nargs, lcc_vargs, scope);
  } catch (...) {
    handleArgumentHandlingExceptions(closure);
  }
  return;
}

T_sp evaluate_lambda_list_form(T_sp form, T_sp env) {
  // TODO:: The code should be compiled and not interpreted
  //	TopLevelIHF stackFrame(my_thread->invocationHistoryStack(),form);
  return eval::evaluate(form, env);
}

TargetClassifier::TargetClassifier(const std::set<int> &skip) : lexicalIndex(-1), skipLexicalIndices(skip) {
  this->_SpecialSymbols = _Nil<T_O>();
  this->_LambdaListSpecials = HashTableEq_O::create_default();
  this->advanceLexicalIndex();
};
TargetClassifier::TargetClassifier(HashTableEq_sp specialSymbols, const std::set<int> &skip)
    : _SpecialSymbols(specialSymbols), lexicalIndex(-1), skipLexicalIndices(skip) {
  this->_LambdaListSpecials = HashTableEq_O::create_default();
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
List_sp TargetClassifier::finalClassifiedSymbols() {
  if (this->_SpecialSymbols.notnilp()) {
    gc::As<HashTableEq_sp>(this->_SpecialSymbols)->maphash([this](T_sp s, T_sp val) {
                    if ( !this->_LambdaListSpecials->contains(s) ) {
                        this->_AccumulatedClassifiedSymbols
                            << Cons_O::create(ext::_sym_specialVar,s);
                    } });
  }
  return this->_AccumulatedClassifiedSymbols.cons();
}

void throw_if_not_destructuring_context(T_sp context) {
  if (context == _sym_macro || context == cl::_sym_destructuring_bind)
    return;
  SIMPLE_ERROR(BF("Lambda list is destructuring_bind but context does not support it context[%s]") % _rep_(context));
}

CL_LISPIFY_NAME("LambdaListHandler-lambdaList");
CL_DEFMETHOD T_sp LambdaListHandler_O::lambdaList() {
  ql::list ll;
  { // required arguments  req = ( num req1 req2 ...)
    for (gctools::Vec0<RequiredArgument>::const_iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); ++it) {
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
    if (this->_RestArgument.VaRest) {
      ll << core::_sym_AMPva_rest;
    } else {
      ll << cl::_sym_AMPrest;
    }
    ll << this->_RestArgument._ArgTarget;
  }
  if (this->_KeyFlag.notnilp() || this->_KeywordArguments.size() > 0) {
    ll << cl::_sym_AMPkey;
    for (gctools::Vec0<KeywordArgument>::const_iterator it = this->_KeywordArguments.begin();
         it != this->_KeywordArguments.end(); it++) {
      T_sp keywordTarget = it->_ArgTarget;
      if (gc::As<Symbol_sp>(it->_Keyword)->symbolName()->get() != gc::As<Symbol_sp>(keywordTarget)->symbolName()->get()) {
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
  if (this->_AuxArguments.size() > 0) {
    ll << cl::_sym_AMPaux;
    for (gctools::Vec0<AuxArgument>::const_iterator it = this->_AuxArguments.begin();
         it != this->_AuxArguments.end(); it++) {
      Cons_sp one = Cons_O::createList(it->_ArgTarget, it->_Expression);
      ll << one;
    }
  }
  return ll.cons();
}


CL_LISPIFY_NAME("add-missing-sensors");
CL_DOCSTRING(R"doc(Any optional or keyword parameters that are missing their sensors have them set to gensyms)doc");
CL_DEFMETHOD void LambdaListHandler_O::add_missing_sensors() {
  if (this->_OptionalArguments.size() > 0) {
    for (gctools::Vec0<OptionalArgument>::iterator it = this->_OptionalArguments.begin();
         it != this->_OptionalArguments.end(); it++) {
      if (it->_Sensor._ArgTarget.nilp()) {
        it->_Sensor._ArgTarget = cl__gensym();
      }
    }
  }
  if (this->_KeyFlag.notnilp() || this->_KeywordArguments.size() > 0) {
    for (gctools::Vec0<KeywordArgument>::iterator it = this->_KeywordArguments.begin();
         it != this->_KeywordArguments.end(); it++) {
      if (it->_Sensor._ArgTarget.nilp()) {
        it->_Sensor._ArgTarget = cl__gensym();
      }
    }
  }
}

HashTableEq_sp LambdaListHandler_O::identifySpecialSymbols(List_sp declareSpecifierList) {
  HashTableEq_sp specials(HashTableEq_O::create_default());
  LOG(BF("Processing declareSpecifierList: %s") % declareSpecifierList->__repr__());
  if (declareSpecifierList.notnilp()) {
    ASSERTF(oCar(declareSpecifierList) != cl::_sym_declare, BF("The declareSpecifierList were not processed properly coming into this function - only declare specifiers should be passed - I got: %s") % _rep_(declareSpecifierList));
    for (auto cur : declareSpecifierList) {
      T_sp entry = oCar(cur);
      if ((entry).consp() && oCar(entry) == cl::_sym_special) {
        for (auto spcur : coerce_to_list(oCdr(entry))) {
          specials->insert(gc::As<Symbol_sp>(oCar(spcur)));
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

T_mv LambdaListHandler_O::process_single_dispatch_lambda_list(List_sp llraw, bool allow_first_argument_default_dispatcher) {
  List_sp llprocessed = cl__copy_list(llraw);
  Symbol_sp sd_symbol = _Nil<Symbol_O>();
  Symbol_sp sd_class = _Nil<Symbol_O>();
  bool saw_amp = false;
  int dispatchIndex = 0;
  int idx = 0;
  for (auto cur : llprocessed) {
    T_sp arg = oCar(cur);
    if (cl__symbolp(arg)) {
      if (gc::As<Symbol_sp>(arg)->amp_symbol_p()) {
        saw_amp = true;
        break;
      }
    } else if ((arg).consp()) {
      List_sp carg = arg;
      if (cl__length(carg) != 2) {
        SYMBOL_SC_(CorePkg, singleDispatchWrongNumberArgumentsError);
        SYMBOL_EXPORT_SC_(KeywordPkg, arguments);
        ERROR(_sym_singleDispatchWrongNumberArgumentsError,
              Cons_O::createList(kw::_sym_arguments, carg));
      }
      if (sd_symbol.notnilp()) {
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
List_sp LambdaListHandler_O::process_macro_lambda_list(List_sp lambda_list) {
  List_sp new_lambda_list = cl__copy_list(lambda_list);
  Symbol_sp whole_symbol = _Nil<Symbol_O>();
  Symbol_sp environment_symbol = _Nil<Symbol_O>();
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
  ql::list sclist; // (af_lineNumber(lambda_list),af_column(lambda_list),core__source_file_info(lambda_list));
  sclist << whole_symbol << environment_symbol << Cons_O::create(name_symbol, new_lambda_list);
  List_sp macro_ll = sclist.cons();
#if 0
  if (_lisp->sourceDatabase().notnilp()) {
    gc::As<SourceManager_sp>(_lisp->sourceDatabase())->duplicateSourcePosInfo(lambda_list, macro_ll);
  }
#endif
  return macro_ll;
}

CL_LAMBDA(lambda-list);
CL_DECLARE();
CL_DOCSTRING("process_macro_lambda_list");
CL_DEFUN T_sp core__process_macro_lambda_list(List_sp lambda_list) {
  List_sp new_ll = LambdaListHandler_O::process_macro_lambda_list(lambda_list);
  return new_ll;
}

CL_LAMBDA(lambda-list);
CL_DECLARE();
CL_DOCSTRING("process_single_dispatch_lambda_list");
CL_DEFUN T_mv core__process_single_dispatch_lambda_list(List_sp lambda_list) {
  return LambdaListHandler_O::process_single_dispatch_lambda_list(lambda_list);
}

void LambdaListHandler_O::initialize() {
  this->_CreatesBindings = true;
  this->_DeclareSpecifierList = _Nil<T_O>();
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
  //  printf("%s:%d  TargetClassifier::classifyTarget target._ArgTarget@%p --> %p\n", __FILE__, __LINE__, &target._ArgTarget.rawRef_(), target._ArgTarget.raw_());
  Symbol_sp sym = gc::As<Symbol_sp>(target._ArgTarget);
  if (sym->specialP() || (this->_SpecialSymbols.notnilp() && gc::As<HashTable_sp>(this->_SpecialSymbols)->contains(sym))) {
    target._ArgTargetFrameIndex = SPECIAL_TARGET;
    this->_AccumulatedClassifiedSymbols << Cons_O::create(ext::_sym_specialVar, target._ArgTarget);
    this->_LambdaListSpecials->insert(sym);
  } else {
    target._ArgTargetFrameIndex = this->lexicalIndex;
    this->_AccumulatedClassifiedSymbols << Cons_O::create(ext::_sym_lexicalVar, Cons_O::create(target._ArgTarget, make_fixnum(target._ArgTargetFrameIndex)));
    this->advanceLexicalIndex();
  }
}

void LambdaListHandler_O::recursively_build_handlers_count_arguments(List_sp declares, T_sp context, TargetClassifier &classifier) {
  { // required arguments
    for (gctools::Vec0<RequiredArgument>::iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); it++) {
      if (it->_lambdaListP()) {
        SIMPLE_ERROR(BF("A nested lambda list was discovered while building handler for %s") % this->__repr__());
        List_sp sub_lambda_list = it->lambda_list();
        //		    throw_if_not_destructuring_context(context);
        LambdaListHandler_sp sub_handler = LambdaListHandler_O::createRecursive_(sub_lambda_list, declares, context, classifier);
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
        DEPRECATED();
        //		    throw_if_not_destructuring_context(context);
        List_sp sub_lambda_list = it->lambda_list();
        LambdaListHandler_sp sub_handler = LambdaListHandler_O::createRecursive_(sub_lambda_list, declares, context, classifier);
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
        DEPRECATED();
        //		    throw_if_not_destructuring_context(context);
        List_sp sub_lambda_list = it->lambda_list();
        LambdaListHandler_sp sub_handler = LambdaListHandler_O::createRecursive_(sub_lambda_list, declares, context, classifier);
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

#define PASS_FUNCTION_REQUIRED bind_required_va_list
#define PASS_FUNCTION_OPTIONAL bind_optional_va_list
#define PASS_FUNCTION_REST bind_rest_va_list
#define PASS_FUNCTION_KEYWORD bind_keyword_va_list
#define PASS_ARGS size_t n_args, VaList_sp arglist
#define PASS_ARGS_NUM n_args
#define PASS_NEXT_ARG(_ai) arglist->next_arg()
#include "argumentBinding.cc"
#undef PASS_FUNCTION_REQUIRED
#undef PASS_FUNCTION_OPTIONAL
#undef PASS_FUNCTION_REST
#undef PASS_FUNCTION_KEYWORD
#undef PASS_ARGS
#undef PASS_ARGS_NUM
#undef PASS_NEXT_ARG

void bind_aux(gctools::Vec0<AuxArgument> const &auxs, DynamicScopeManager &scope) {
  LOG(BF("There are %d aux variables") % auxs.size());
  gctools::Vec0<AuxArgument>::iterator ci;
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

void LambdaListHandler_O::createBindingsInScopeVaList(size_t nargs, VaList_sp va,
                                                      DynamicScopeManager &scope) {
  if (UNLIKELY(!this->_CreatesBindings))
    return;
  VaList_S arglist_struct(*va);
  VaList_sp arglist(&arglist_struct);
  int arg_idx = 0;
  arg_idx = bind_required_va_list(this->_RequiredArguments, nargs, arglist, arg_idx, scope);
  if (UNLIKELY(this->_OptionalArguments.size() != 0)) {
    arg_idx = bind_optional_va_list(this->_OptionalArguments, nargs, arglist, arg_idx, scope);
  }
  if (UNLIKELY(arg_idx < nargs && !(this->_RestArgument.isDefined()) && (this->_KeywordArguments.size() == 0))) {
    throwTooManyArgumentsError(nargs, this->numberOfLexicalVariables());
    //	    TOO_MANY_ARGUMENTS_ERROR();
  }
  if (UNLIKELY(this->_RestArgument.isDefined())) {
    bind_rest_va_list(this->_RestArgument, nargs, arglist, arg_idx, scope);
  }
  if (UNLIKELY(this->_KeywordArguments.size() != 0)) {
    bind_keyword_va_list(this->_KeywordArguments, this->_AllowOtherKeys, nargs, arglist, arg_idx, scope);
  }
  if (UNLIKELY(this->_AuxArguments.size() != 0))
    bind_aux(this->_AuxArguments, scope);
}

void LambdaListHandler_O::dump_keywords()
{
  for ( int i = 0; i< this->_KeywordArguments.size(); ++i ) {
    printf("%s:%d Keyword: %s@%p\n", __FILE__, __LINE__, _rep_(this->_KeywordArguments[i]._Keyword).c_str(), this->_KeywordArguments[i]._Keyword.raw_());
  }
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
  LOG(BF("In switch_add_argument_mode argument is a symbol: %s %X") % _rep_(symbol) % symbol.get());
  bool isAmpSymbol = false;
  if (symbol.notnilp()) {
    if (Symbol_sp sym = symbol.asOrNull<Symbol_O>()) {
      isAmpSymbol = (sym == _sym_DOT || sym->amp_symbol_p());
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
      } else if (symbol == core::_sym_AMPva_rest) {
        mode = va_rest;
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
      } else if (symbol == core::_sym_AMPva_rest) {
        mode = va_rest;
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
    case va_rest:
      LOG(BF("Was in va_rest mode"));
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
  if (context == _sym_macro || context == cl::_sym_function || context == cl::_sym_method || context == cl::_sym_destructuring_bind || context == _lisp->_true())
    return;
  printf("%s:%d context.raw_= %p     cl::_sym_destructuring_bind.raw_=%p\n",
         __FILE__, __LINE__, context.raw_(), cl::_sym_destructuring_bind.raw_());
  SIMPLE_ERROR(BF("Illegal parse_lambda_list context[%s]") % _rep_(context));
}

bool contextSupportsWhole(T_sp context) {
  if (context == _sym_macro || context == cl::_sym_destructuring_bind || context == cl::_sym_deftype || context == cl::_sym_define_method_combination) {
    return true;
  }
  return false;
}

bool contextSupportsEnvironment(T_sp context) {
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
  key_flag = _Nil<T_O>();
  auxs.clear();
  allow_other_keys = _lisp->_false();
  if (original_lambda_list.nilp())
    return false;
  throw_if_invalid_context(context);
  List_sp arguments = cl__copy_list(original_lambda_list);
  LOG(BF("Argument handling mode starts in (required) - interpreting: %s") % arguments->__repr__());
  ArgumentMode add_argument_mode = required;
  restarg.clear();
  List_sp cur = arguments;
  while (cur.notnilp()) {
    LOG(BF("Handing argument: %s") % _rep_(oCar(cur)));
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
      break;
    }
    case optional: {
      T_sp sarg = _Nil<T_O>();
      T_sp defaultValue = _Nil<T_O>();
      T_sp supplied = _Nil<T_O>();
      if ((oarg).consp()) {
        List_sp carg = oarg;
        LOG(BF("Optional argument is a Cons: %s") % carg->__repr__());
        sarg = oCar(carg);
        if (oCdr(carg).notnilp()) {
          defaultValue = oCadr(carg);
          if (oCddr(carg).notnilp())
            supplied = oCaddr(carg);
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
      restarg.setTarget(oarg);
      restarg.VaRest = false;
      break;
    }
    case va_rest: {
      if (restarg.isDefined()) {
        SIMPLE_ERROR(BF("Only one name is allowed after &rest - you have already defined: ") % restarg.asString());
      }
      restarg.setTarget(oarg);
      restarg.VaRest = true;
      break;
    }
    case dot_rest: {
      if (oCdr(cur).notnilp()) {
        SIMPLE_ERROR(BF("Lambda list dot followed by more than one argument"));
      }
      restarg.setTarget(oarg);
      goto DONE;
    }
    case keyword: {
      Symbol_sp keySymbol = _Nil<Symbol_O>();
      T_sp localTarget = _Nil<T_O>();
      T_sp defaultValue = _Nil<T_O>();
      T_sp sensorSymbol = _Nil<T_O>();
      if (cl__symbolp(oarg)) {
        localTarget = oarg;
        keySymbol = gc::As<Symbol_sp>(localTarget)->asKeywordSymbol();
      } else if ((oarg).consp()) {
        List_sp carg = oarg;
        T_sp head = oCar(carg);
        if ((head).consp()) {
          List_sp namePart = head;
          keySymbol = gc::As<Symbol_sp>(oCar(namePart)); // This is the keyword name
          if (!keySymbol->isKeywordSymbol()) {
            SIMPLE_ERROR(BF("With key arguments of the form ((:x y) ...) the first argument must be a keyword symbol - you gave: %s") % _rep_(keySymbol));
          }
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
  return true;
}

CL_LAMBDA(vl context);
CL_DECLARE();
CL_DOCSTRING("processLambdaList - this is like ECL::process-lambda-list except auxs are returned as nil or a list of 2*n elements of the form (sym1 init1 sym2 init2 ...) In ECL they say you need to prepend the number of auxs - that breaks the destructure macro. ECL process-lambda-list says context may be MACRO, FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND but in ECL>>clos/method.lsp they pass T!!!");
CL_DEFUN T_mv core__process_lambda_list(List_sp lambdaList, T_sp context) {
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
    // !!!! The above is not true auxs = nil or (aux1 init1 aux2 init 2)
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
                _lisp->_boolean(restarg.VaRest)
                );
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

LambdaListHandler_sp LambdaListHandler_O::createRecursive_(List_sp lambda_list, List_sp declares, T_sp context, TargetClassifier &classifier) {
  GC_ALLOCATE(LambdaListHandler_O, llh);
  llh->parse_lambda_list_declares(lambda_list, declares, context, classifier);
  return llh;
}

LambdaListHandler_sp LambdaListHandler_O::create(List_sp lambda_list, List_sp declares, T_sp context, const std::set<int> &skipFrameIndices) {
  HashTableEq_sp specialSymbols(LambdaListHandler_O::identifySpecialSymbols(declares));
  TargetClassifier classifier(specialSymbols, skipFrameIndices);
  LambdaListHandler_sp ollh = LambdaListHandler_O::createRecursive_(lambda_list, declares, context, classifier);
  ollh->_NumberOfLexicalVariables = classifier.totalLexicalVariables();
  ollh->_RequiredLexicalArgumentsOnly = ollh->requiredLexicalArgumentsOnlyP_();
  return ollh;
}

void LambdaListHandler_O::setComment(const string &s) { this->_Comment = SimpleBaseString_O::make(s); };
string LambdaListHandler_O::getComment() const { if (this->_Comment) return this->_Comment->get_std_string(); else return ""; };


LambdaListHandler_sp LambdaListHandler_O::create(int numArgs, const std::set<int> &skipIndices) {
  GC_ALLOCATE(LambdaListHandler_O, ollh);
  ollh->create_required_arguments(numArgs, skipIndices);
  return ollh;
}

CL_LAMBDA("lambda-list &optional declares (context 'core::function)");
CL_LISPIFY_NAME(makeLambdaListHandler);
CL_DEFUN LambdaListHandler_sp LambdaListHandler_O::makeLambdaListHandler(List_sp lambda_list, List_sp declares, T_sp context) {
  LambdaListHandler_sp llh = LambdaListHandler_O::create(lambda_list, declares, context);
  return llh;
}

void LambdaListHandler_O::create_required_arguments(int num, const std::set<int> &skipIndices) {
  _OF();
  TargetClassifier classifier(skipIndices);
  for (int i = 0, iEnd(num - skipIndices.size()); i < iEnd; ++i) {
    Symbol_sp name = cl__gensym(SimpleBaseString_O::make("arg"));
    RequiredArgument req(name, i);
    this->_RequiredArguments.push_back(req);
    classifier.classifyTarget(req);
  }
  this->_ClassifiedSymbolList = classifier.finalClassifiedSymbols();
  ASSERTF(this->_ClassifiedSymbolList.nilp() || (oCar(this->_ClassifiedSymbolList)).consp(), BF("LambdaListHandler _classifiedSymbols must contain only conses - it contains %s") % _rep_(this->_ClassifiedSymbolList));
  this->_NumberOfLexicalVariables = classifier.totalLexicalVariables();
  this->_RequiredLexicalArgumentsOnly = this->requiredLexicalArgumentsOnlyP_();
}

void LambdaListHandler_O::parse_lambda_list_declares(List_sp lambda_list, List_sp declareSpecifierList, T_sp context, TargetClassifier &classifier) {
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
    ASSERTF(this->_ClassifiedSymbolList.nilp() || (oCar(this->_ClassifiedSymbolList)).consp(), BF("LambdaListHandler _classifiedSymbols must contain only conses - it contains %s") % _rep_(this->_ClassifiedSymbolList));
  } else {
    this->_ClassifiedSymbolList = _Nil<T_O>();
  }
}

CL_LISPIFY_NAME("single-dispatch-on-argument");
CL_DEFMETHOD int LambdaListHandler_O::single_dispatch_on_argument(Symbol_sp target) {
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
  if (!this->_ClassifiedSymbolList.objectp()) {
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
    if (this->_RestArgument.VaRest) {
      ss << " &va-rest ";
    } else {
      ss << " &rest ";
    }
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
  ss << "#<" << cl__class_of(this->asSmartPtr())->classNameAsString();
  {
    ss << this->partsAsString();
  }
  if (this->_Comment) { ss << " :comment \"" << this->_Comment->get_std_string() << "\"";}
  ss << "> ";
  return ss.str();
}

CL_LISPIFY_NAME("processLambdaListHandler");
CL_DEFMETHOD T_mv LambdaListHandler_O::processLambdaListHandler() const {

  ql::list reqs;
  { // required arguments  req = ( num req1 req2 ...)
    reqs << make_fixnum((int)this->_RequiredArguments.size());
    for (gctools::Vec0<RequiredArgument>::const_iterator it = this->_RequiredArguments.begin();
         it != this->_RequiredArguments.end(); it++) {
      reqs << it->classified();
    }
  }
  ql::list opts;
  { // optional arguments   opts = (num opt1 init1 flag1 ...)
    opts << make_fixnum((int)this->_OptionalArguments.size());
    for (gctools::Vec0<OptionalArgument>::const_iterator it = this->_OptionalArguments.begin();
         it != this->_OptionalArguments.end(); it++) {
      opts << it->classified() << it->_Default << it->_Sensor.classified();
    }
  }
  ql::list keys;
  bool keyFlag = this->_KeyFlag.notnilp();
  { // optional arguments   keys = (num key1 var1 init1 flag1 ...)
    keys << make_fixnum((int)this->_KeywordArguments.size());
    for (gctools::Vec0<KeywordArgument>::const_iterator it = this->_KeywordArguments.begin();
         it != this->_KeywordArguments.end(); it++) {
      keys << it->_Keyword << it->classified() << it->_Default << it->_Sensor.classified();
      keyFlag = true;
    }
  }
  ql::list auxs;
  { // auxes arguments   auxs = (num aux1 init1 ...)
    auxs << make_fixnum((int)this->_AuxArguments.size());
    for (gctools::Vec0<AuxArgument>::const_iterator it = this->_AuxArguments.begin();
         it != this->_AuxArguments.end(); it++) {
      auxs << it->classified() << it->_Expression;
    }
  }
  T_sp treqs = reqs.cons();
  return (Values(treqs,
                 opts.cons(),
                 this->_RestArgument.classified(),
                 _lisp->_boolean(keyFlag),
                 keys.cons(),
                 this->_AllowOtherKeys,
                 auxs.cons(),
                 _lisp->_boolean(this->_RestArgument.VaRest)
                 ));
}

bool LambdaListHandler_O::requiredLexicalArgumentsOnlyP_() const {
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

CL_LISPIFY_NAME("namesOfLexicalVariables");
CL_DEFMETHOD List_sp LambdaListHandler_O::namesOfLexicalVariables() const {
  List_sp namesRev = _Nil<T_O>();
  for (auto cur : this->_ClassifiedSymbolList) {
    if (oCar(oCar(cur)) == ext::_sym_lexicalVar) {
      namesRev = Cons_O::create(oCadr(oCar(cur)), namesRev);
    }
  }
  return cl__nreverse(namesRev);
}

void LambdaListHandler_O::calculateNamesOfLexicalVariablesForDebugging() {
  List_sp names = this->namesOfLexicalVariables();
  this->_LexicalVariableNamesForDebugging = VectorObjects_O::make(cl__length(names),_Nil<T_O>());
  gc::As<VectorObjects_sp>(this->_LexicalVariableNamesForDebugging)->fillInitialContents(names);
}

CL_LISPIFY_NAME("namesOfLexicalVariablesForDebugging");
CL_DEFMETHOD VectorObjects_sp LambdaListHandler_O::namesOfLexicalVariablesForDebugging() {
  if (this->_LexicalVariableNamesForDebugging.nilp()) {
    this->calculateNamesOfLexicalVariablesForDebugging();
  }
  return gc::As<VectorObjects_sp>(this->_LexicalVariableNamesForDebugging);
}

// ----------------------------------------------------------------------
//


LambdaListHandler_O::LambdaListHandler_O() : _SpecialSymbolSet(_Nil<T_O>()), _LexicalVariableNamesForDebugging(_Nil<VectorObjects_O>()), _RequiredLexicalArgumentsOnly(false){};



  SYMBOL_SC_(CorePkg, process_macro_lambda_list);
  SYMBOL_SC_(CorePkg, process_single_dispatch_lambda_list);
  SYMBOL_SC_(CorePkg, makeLambdaListHandler);
  SYMBOL_SC_(CorePkg, processLambdaList);


}; /* core */
