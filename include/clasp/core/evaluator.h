/*
    File: evaluator.h
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
#ifndef evaluator_H
#define evaluator_H

#include <clasp/core/foundation.h>
#include <clasp/core/ql.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/executables.h>
#include <clasp/core/activationFrame.h>

namespace cl {
extern core::Symbol_sp _sym_findClass;
extern core::Symbol_sp _sym_undefinedFunction;
};
namespace kw {
extern core::Symbol_sp _sym_name;
};

namespace core {

T_sp af_interpreter_lookup_variable(Symbol_sp sym, T_sp env);
T_sp af_interpreter_lookup_function(Symbol_sp sym, T_sp env);
T_sp af_interpreter_lookup_macro(Symbol_sp sym, T_sp env);
T_sp core_lookup_symbol_macro(Symbol_sp sym, T_sp env);

namespace eval {

/*! Evaluate within env.
          See ecl/src/c/compiler.d:eval-with-env */
T_mv core_evalWithEnv(T_sp form, T_sp env, bool stepping = false, bool compiler_env_p = true, bool execute = true);

extern List_sp evaluateList(List_sp args, T_sp environment);
extern T_mv evaluateListReturnLast(List_sp args, T_sp environment);

extern T_mv sp_progn(List_sp code, T_sp env);
extern T_mv sp_setq(List_sp args, T_sp environment);

extern void defineSpecialOperatorsAndMacros(Package_sp pkg);

T_sp lookupFunction(T_sp functionDesignator, T_sp env);

/*! Evaluate a list of expressions (args) into an ActivationFrame that has
	  enough storage to accept each of the objects that is generated by the list of args */
extern void evaluateIntoActivationFrame(ActivationFrame_sp af, List_sp args, T_sp environment);

/*! See the CLHS for "apply" - all arguments are in args 
  (functionDesignator) can be a Symbol or an Function
*/

extern T_mv applyClosureToActivationFrame(gctools::tagged_functor<Closure> closureP, ActivationFrame_sp af);

extern T_mv applyToActivationFrame(T_sp functionDesignator, ActivationFrame_sp af);

/*! I want a variadic template function that does APPLY.  C++ variadic template parameter packs
	  must be the last arguments of a function.   APPLY has as its last arguments argsPLUS.
	  So we move argsPLUS up to be the second argument (after the function designator) and list
	  the variadic arguments following it */
template <class... Args>
inline T_mv applyLastArgsPLUSFirst(T_sp fn, List_sp argsPLUS, Args... args) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.nilp())
    ERROR_UNDEFINED_FUNCTION(fn);
  Function_sp func = gc::As<Function_sp>(tfunc);
  int numArgsPassed = sizeof...(Args);
  int numArgsPlus = cl_length(argsPLUS);
  int nargs = numArgsPassed + numArgsPlus;
  ValueFrame_sp frob(ValueFrame_O::create_fill_numExtraArgs(numArgsPlus, _Nil<ActivationFrame_O>(), args...));
  List_sp cur = argsPLUS;
  for (int i = numArgsPassed; i < nargs; ++i) {
    frob->operator[](i) = oCar(cur);
    cur = oCdr(cur);
  }
  gctools::tagged_functor<Closure> closureP = func->closure;
  ASSERTF(closureP, BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
  return applyClosureToActivationFrame(closureP, frob);
}

#if 0
	inline T_mv apply( T_sp fn, T_sp arg1, List_sp argsPLUS )
	{
	    Function_sp func = lookupFunction(fn,_Nil<T_O>());
            if ( func.nilp() ) {
                ERROR_UNDEFINED_FUNCTION(fn);
            }
	    int numArgsPassed = 1;
	    int numArgsPlus = cl_length(argsPLUS);
	    int nargs = numArgsPassed + numArgsPlus;
	    ValueFrame_sp frob(ValueFrame_O::create_fill_numExtraArgs(numArgsPlus,_Nil<ActivationFrame_O>(),arg1));
	    List_sp cur = argsPLUS;
	    for ( int i=numArgsPassed; i<nargs; ++i ) {
		frob->operator[](i) = oCar(cur);
		cur=cCdr(cur);
	    }
            Closure* closureP = func->closure;
            ASSERTF(closureP,BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
	    return applyClosureToActivationFrame(closureP,frob);
	}
#endif

#define USE_ARRAY0

inline T_mv funcall(T_sp fn) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.nilp())
    ERROR_UNDEFINED_FUNCTION(fn);
  Function_sp func = gc::As<Function_sp>(tfunc);
  gctools::tagged_functor<Closure> ft = func->closure;
  return (*ft)(0, LCC_UNUSED_rest0());
}

template <class ARG0>
inline T_mv funcall(T_sp fn, ARG0 arg0) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.nilp())
    ERROR_UNDEFINED_FUNCTION(fn);
  Function_sp func = gc::As<Function_sp>(tfunc);
  gctools::tagged_functor<Closure> ft = func->closure;
  return (*ft)(1, LCC_FROM_SMART_PTR(arg0), LCC_UNUSED_rest1());
}

template <class ARG0, class ARG1>
inline T_mv funcall(T_sp fn, ARG0 arg0, ARG1 arg1) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.raw_() == NULL || tfunc.nilp()) {
    // While booting, cl::_sym_findClass will apply'd before
    // it is bound to a symbol
    if (fn == cl::_sym_findClass) {
      return (cl_findClass(gc::As<Symbol_sp>(arg0), false, _Nil<T_O>()));
    }
    ERROR_UNDEFINED_FUNCTION(fn);
  }
  Function_sp func = tfunc.asOrNull<Function_O>();
  ASSERT(func);
  gctools::tagged_functor<Closure> ft = func->closure;
  return (*ft)(2, LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_UNUSED_rest2());
}

template <class ARG0, class ARG1, class ARG2>
inline T_mv funcall(T_sp fn, ARG0 arg0, ARG1 arg1, ARG2 arg2) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.nilp())
    ERROR_UNDEFINED_FUNCTION(fn);
  Function_sp func = gc::As<Function_sp>(tfunc);
  gctools::tagged_functor<Closure> ft = func->closure;
  return (*ft)(3, LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2), LCC_UNUSED_rest3());
}

template <class ARG0, class ARG1, class ARG2, class ARG3>
inline T_mv funcall(T_sp fn, ARG0 arg0, ARG1 arg1, ARG2 arg2, ARG3 arg3) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.nilp())
    ERROR_UNDEFINED_FUNCTION(fn);
  Function_sp func = gc::As<Function_sp>(tfunc);
  gctools::tagged_functor<Closure> ft = func->closure;
  return (*ft)(4, LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2), LCC_FROM_SMART_PTR(arg3));
}


// Do I need a variadic funcall???
template <class ARG0, class ARG1, class ARG2, class ARG3, class... ARGS>
inline T_mv funcall(T_sp fn, ARG0 arg0, ARG1 arg1, ARG2 arg2, ARG3 arg3, ARGS &&... args) {
  T_sp tfunc = lookupFunction(fn, _Nil<T_O>());
  if (tfunc.nilp())
    ERROR_UNDEFINED_FUNCTION(fn);
  Function_sp func = gc::As<Function_sp>(tfunc);
  gctools::tagged_functor<Closure> ft = func->closure;
  size_t vnargs = sizeof...(ARGS);
  size_t nargs = vnargs + LCC_FIXED_NUM;
  if (nargs > core::MultipleValues::MultipleValuesLimit) {
    SIMPLE_ERROR(BF("Too many arguments %d only %d supported") % nargs % core::MultipleValues::MultipleValuesLimit);
  }

#if 0
  MultipleValues &mv = lisp_callArgs();
  // Do a placement new of an array of T_sp in the remainingArgumentsInMultipleValues
  // and initialize it using the variadic arguments in the parameter pack ARGS...args
  T_O **remainingArgumentsInMultipleValues = mv.callingArgsExtraArgStart();
  /*T_sp *mvargs = */new (remainingArgumentsInMultipleValues) T_sp[vnargs]{std::forward<ARGS>(args)...};
#endif
  return (*ft)(nargs
               , LCC_FROM_SMART_PTR(arg0)
               , LCC_FROM_SMART_PTR(arg1)
               , LCC_FROM_SMART_PTR(arg2)
               , LCC_FROM_SMART_PTR(arg3)
               , std::forward<ARGS>(args).raw_()... );
}

 
};
};

#endif
