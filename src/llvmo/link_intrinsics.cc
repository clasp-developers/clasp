/*
    File: intrinsics.cc
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
#define DEBUG_LANDING_PAD 1


#define DEBUG_LEVEL_FULL
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mps.h>
};
#endif
#include <typeinfo>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/bignum.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/arrayObjects.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/arguments.h>
#include <clasp/core/designators.h>
#include <clasp/core/compPackage.h>
#include <clasp/core/package.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/stacks.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/numbers.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/str.h>
#include <clasp/llvmo/symbolTable.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/intrinsics.h>

#define	DEBUG_FLOW_CONTROL	1

using namespace core;

#pragma GCC visibility push(default)

extern "C" {

LispCallingConventionPtr lccGlobalFunction(core::Symbol_sp sym) {
  printf("%s:%d lccSymbolFunction for %s returning NULL for now\n", __FILE__, __LINE__, _rep_(sym).c_str());
  return NULL;
}

typedef enum { noFunctionBoundToSymbol
               , badKeywordArgument
               , couldNotCoerceToClosure
               , destinationMustBeActivationFrame
               , invalidIndexForFunctionFrame
               } ErrorCode;

void errorMessage(ErrorCode err, core::T_sp arg0=_Nil<core::T_O>(), core::T_sp arg1=_Nil<core::T_O>(), core::T_sp arg2=_Nil<core::T_O>() ) {
  switch (err) {
  case noFunctionBoundToSymbol:
      SIMPLE_ERROR(BF("There is no function bound to the symbol %s") % _rep_(arg0) );
  case badKeywordArgument:
      SIMPLE_ERROR(BF("Bad keyword argument %s") % _rep_(arg0));
  case couldNotCoerceToClosure:
      SIMPLE_ERROR(BF(" symbol %s") % _rep_(arg0));
  case destinationMustBeActivationFrame:
      SIMPLE_ERROR(BF("Destination must be ActivationFrame"));
  case invalidIndexForFunctionFrame:
      SIMPLE_ERROR(BF("Invalid index[%d] for FunctionFrame(size=%d)") % _rep_(arg0) % _rep_(arg1));

  };
};
};


extern "C" {

const std::type_info &typeidCoreCatchThrow = typeid(core::CatchThrow);
const std::type_info &typeidCoreLexicalGo = typeid(core::LexicalGo);
const std::type_info &typeidCoreDynamicGo = typeid(core::DynamicGo);
const std::type_info &typeidCoreReturnFrom = typeid(core::ReturnFrom);
const std::type_info &typeidCoreUnwind = typeid(core::Unwind);


#define LOW_LEVEL_TRACE_QUEUE_SIZE 1024
uint _LLVMLowLevelTraceQueueIn = 0;
uint _LLVMLowLevelTraceQueueWrapped = false;
uint _LLVMLowLevelTraceQueue[LOW_LEVEL_TRACE_QUEUE_SIZE];

NOINLINE void lowLevelTrace(uint traceid) {
  if (comp::_sym_STARlowLevelTracePrintSTAR->symbolValue().isTrue()) {
    printf("+++ lowLevelTrace[%d]\n", traceid);
    if ( traceid == 1000115396 ) {
      printf("%s:%d Set a breakpoint here\n", __FILE__, __LINE__);
    }
  }
  _LLVMLowLevelTraceQueue[_LLVMLowLevelTraceQueueIn] = traceid;
  ++_LLVMLowLevelTraceQueueIn;
  if (_LLVMLowLevelTraceQueueIn >= LOW_LEVEL_TRACE_QUEUE_SIZE) {
    _LLVMLowLevelTraceQueueIn = 0;
    _LLVMLowLevelTraceQueueWrapped = true;
  }
}

void unreachableError() {
  printf("%s:%d In unreachableError -  Hit an unreachable block\n",
         __FILE__, __LINE__);
}

void dumpLowLevelTrace(int numLowLevels) {
int cur = _LLVMLowLevelTraceQueueIn;
  for (int i = 0; i < numLowLevels; i++) {
    --cur;
    if (cur < 0) {
      if (_LLVMLowLevelTraceQueueWrapped) {
        cur = LOW_LEVEL_TRACE_QUEUE_SIZE - 1;
      } else {
        printf("-----Ran out of block trace entries----\n");
        break;
      }
    }
    printf("LowLevel-trace#%d -> %u\n", -i, _LLVMLowLevelTraceQueue[cur]);
  }
}
};

extern "C" {

NOINLINE void va_tooManyArgumentsException(const char *funcName, std::size_t givenNumberOfArguments, std::size_t requiredNumberOfArguments) {
  SIMPLE_ERROR(BF("Too many arguments for %s - got %d and expected %d") % funcName % givenNumberOfArguments % requiredNumberOfArguments);
}

NOINLINE void va_notEnoughArgumentsException(const char *funcName, std::size_t givenNumberOfArguments, std::size_t requiredNumberOfArguments) {
  SIMPLE_ERROR(BF("Too few arguments for %s - got %d and expected %d") % funcName % givenNumberOfArguments % requiredNumberOfArguments);
}

NOINLINE extern void va_ifExcessKeywordArgumentsException(char *fnName, std::size_t nargs, core::T_O **argArray, size_t argIdx) {
  if (argIdx >= nargs) return;
  stringstream ss;
  for (int i(0); i < nargs; ++i) {
    ss << _rep_(core::T_sp(argArray[i])) << " ";
  }
  SIMPLE_ERROR(BF("va_ifExcessKeywordArgumentsException>> Excess keyword arguments fnName: %s argIdx: %d  args: %s") % fnName % argIdx % ss.str());
  //        core::throwUnrecognizedKeywordArgumentError(argArray[argIdx]);
}

#if 0
void va_fillActivationFrameWithRequiredVarargs(core::ActivationFrame_sp *afP, int nargs, core::T_sp *ap) {
  for (int i = 0; i < nargs; ++i) {
    (*afP)->operator[](i) = ap[i];
  }
}
#endif


Closure *va_coerceToClosure(core::T_sp *argP) {
  if (!(*argP).objectp()) {
    errorMessage(couldNotCoerceToClosure,*argP);
  }
  core::Function_sp func = core::coerce::functionDesignator((*argP));
  return &(*func->closure);
}


ALWAYS_INLINE core::Closure *va_symbolFunction(core::Symbol_sp *symP) {
  if (!(*symP)->fboundp()) errorMessage(noFunctionBoundToSymbol,*symP);
  core::Function_sp func((gc::Tagged)(*symP)->_Function.theObject);
  core::Closure *funcPtr = &(*func->closure);
  return funcPtr;
}

ALWAYS_INLINE core::Closure *va_lexicalFunction(int depth, int index, core::T_sp *evaluateFrameP) {
  core::Function_sp func = core::Environment_O::clasp_lookupFunction(*evaluateFrameP, depth, index);
  ASSERTF(func.objectp(), BF("UNDEFINED lexicalFunctionRead!! value depth[%d] index[%d] activationFrame: %s") % depth % index % _rep_(*evaluateFrameP));
  return &(*func->closure);
}

ALWAYS_INLINE void mv_FUNCALL(core::T_mv *resultP, core::Closure *closure, LCC_ARGS) {
  ASSERTF(resultP, BF("mv_FUNCALL resultP is NULL!!!"));
  closure->invoke(resultP, LCC_PASS_ARGS);
}

ALWAYS_INLINE void sp_FUNCALL(core::T_sp *resultP, core::Closure *closure, LCC_ARGS_BASE, ...) {
  ASSERTF(resultP, BF("sp_FUNCALL resultP is NULL!!!"));
  T_mv result;
  closure->invoke(&result, LCC_PASS_ARGS);
  *resultP = result;
}

ALWAYS_INLINE void mv_FUNCALL_activationFrame(core::T_mv *resultP, core::Closure *closure, core::ActivationFrame_sp af) {
  (*resultP) = core::eval::applyClosureToActivationFrame(closure, af);
}

ALWAYS_INLINE void sp_FUNCALL_activationFrame(core::T_sp *resultP, core::Closure *closure, core::ActivationFrame_sp af) {
  (*resultP) = core::eval::applyClosureToActivationFrame(closure, af);
}

__attribute__((visibility("default"))) void va_fillRestTarget(core::T_sp *restP, std::size_t nargs, core::T_O **argArray, std::size_t startRest, char *fnName) {
  _G();
  core::List_sp result = _Nil<core::T_O>();
  int inargs = nargs;
  int istartRest = startRest;
  for (int i = inargs - 1; i >= istartRest; i--) {
    result = core::Cons_O::create(gc::smart_ptr<core::T_O>((gc::Tagged)argArray[i]), result);
  }
  (*restP) = result;
  ASSERTNOTNULL(*restP);
}

extern int va_allowOtherKeywords(int saw_aok, std::size_t nargs, core::T_O **argArray, std::size_t argIdx) {
  if (saw_aok)
    return saw_aok;
  bool aokTrue = !(gctools::tagged_nilp(argArray[argIdx + 1]));
  return aokTrue ? 2 : 1;
}

void va_ifBadKeywordArgumentException(int allowOtherKeys, std::size_t badKwIdx, std::size_t nargs, core::T_O **argArray) {
  if (allowOtherKeys == 2) return;
  if (badKwIdx != 65536) errorMessage(badKeywordArgument,core::T_sp(argArray[badKwIdx]));
}

void newFunction_sp(core::Function_sp *sharedP) {
  ASSERT(sharedP != NULL);
  new (sharedP) core::Function_sp();
}
#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#if 0
void destructFunction_sp(core::Function_sp *sharedP) {
  _G();
  ASSERT(sharedP != NULL);
  DEPRECIATED(); // April 2015
  if ((*sharedP).objectp()) {
    typedef core::Function_sp dummy;
    (*sharedP).~dummy();
  }
}
void destructTsp(core::T_sp *sharedP) {
  _G();
  ASSERT(sharedP != NULL);
  DEPRECIATED(); // April 2015
  if ((*sharedP).objectp()) {
    typedef core::T_sp dummy;
    (*sharedP).~dummy();
  }
}
void destructTmv(core::T_mv *sharedP) {
  _G();
  ASSERT(sharedP != NULL);
  DEPRECIATED(); // April 2015
  if ((*sharedP).objectp()) {
    typedef core::T_mv dummy;
    (*sharedP).~dummy();
  }
}
void destructAFsp(core::ActivationFrame_sp *frameP) {
  _G();
  DEPRECIATED(); // April 2015
  ASSERT(frameP != NULL);
  if ((*frameP).objectp()) {
    typedef core::ActivationFrame_sp dummy;
    (*frameP).~dummy();
  }
}
#endif
#pragma clang diagnostic pop
#if 0

void resetTsp(core::T_sp *sharedP) {
  _G();
  ASSERT(sharedP != NULL);
  (*sharedP).reset_();
}

void makeUnboundTsp(core::T_sp *sharedP) {
  (*sharedP) = _Unbound<T_O>();
}
#endif
#if 0
ALWAYS_INLINE extern int compareTsp(core::T_sp *xP, core::T_sp *yP) {
  //	ASSERT(xP!=NULL);
  //	ASSERT(yP!=NULL);
  return ((*xP) == (*yP)) ? 1 : 0;
}
#endif
NOINLINE extern void copyArgs(core::T_sp *destP, int nargs, core::T_O *arg0, core::T_O *arg1, core::T_O *arg2, va_list args) {
  _G();
  //        printf("%s:%d copyArgs destP=%p  nargs=%d\n", __FILE__, __LINE__, destP, nargs);
  switch (nargs) {
  case 0:
    return;
  case 1:
      destP[0] = gctools::smart_ptr<core::T_O>(arg0); // Should these all be (gc::Tagged)???
    return;
  case 2:
    destP[0] = gctools::smart_ptr<core::T_O>(arg0);
    destP[1] = gctools::smart_ptr<core::T_O>(arg1);
    return;
  case 3:
    destP[0] = gctools::smart_ptr<core::T_O>(arg0);
    destP[1] = gctools::smart_ptr<core::T_O>(arg1);
    destP[2] = gctools::smart_ptr<core::T_O>(arg2);
    return;
  default:
    destP[0] = gctools::smart_ptr<core::T_O>(arg0);
    destP[1] = gctools::smart_ptr<core::T_O>(arg1);
    destP[2] = gctools::smart_ptr<core::T_O>(arg2);
    int idx = 3;
    while (idx < nargs) {
      destP[idx] = gctools::smart_ptr<core::T_O>(va_arg(args, core::T_O *));
      ++idx;
    }
    return;
  }
}


void resetTmv(core::T_mv *sharedP) {
MAY_BE_DEPRECIATED();
ASSERT(sharedP != NULL);
(*sharedP).reset_();
}

extern void mv_copyTmv(core::T_mv *destP, core::T_mv *sourceP) {
  ASSERT(sourceP != NULL);
  ASSERT(destP != NULL);
  *destP = *sourceP;
}

/* This function slices a T_mv down to a T_sp */
extern void sp_copyTmv(core::T_sp *destP, core::T_mv *sourceP) {
  _G();
  ASSERT(sourceP != NULL);
  ASSERT(destP != NULL);
  *destP = *sourceP;
}
};

extern "C" {

};

extern "C" {

NOINLINE core::T_O **getMultipleValues(int offset) {
  return &lisp_multipleValues().callingArgsStart()[offset];
}

/*! Return i32 1 if (valP) is != unbound 0 if it is */
int isBound(core::T_sp *valP) {
  ASSERT(valP != NULL);
  return (*valP).unboundp() ? 0 : 1;
  //return (gctools::tagged_ptr<core::T_O>::tagged_unboundp(valP)) ? 0 : 1;
}

/*! Return i32 1 if (valP) is != nil 0 if it is */
int isTrue(core::T_sp *valP) {
  _G();
  ASSERT(valP != NULL);
  return (*valP).nilp() ? 0 : 1;
  //	return (gctools::tagged_ptr<core::T_O>::tagged_nilp(valP)) ? 0 : 1;
}

/*! Return i32 1 if (*valP) is Nil or 0 if not */
int isNilTsp(core::T_sp *valP) {
  _G();
  ASSERT(valP != NULL);
  return (*valP).nilp();
}

void internSymbol_tsp(core::T_sp *resultP, const char *symbolNameP, const char *packageNameP) {
  core::Symbol_sp newSym = _lisp->internWithPackageName(packageNameP, symbolNameP);
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d  internSymbol_tsp(%s::%s)  newSym.px_ref() = %p   cl::destructuring-bind.px_ref()=%p\n", __FILE__, __LINE__, packageNameP, symbolNameP, newSym.px_ref(), cl::_sym_destructuring_bind.px_ref());
#endif
  ASSERTNOTNULL(newSym);
  (*resultP) = newSym;
}

void makeSymbol_tsp(core::T_sp *resultP, const char *symbolNameP) {
  _G();
  core::Symbol_sp newSym = core::Symbol_O::create(symbolNameP);
  ASSERTNOTNULL(newSym);
  (*resultP) = newSym;
}

void internSymbol_symsp(core::Symbol_sp *resultP, const char *symbolNameP, const char *packageNameP) {
  core::Symbol_sp newSym = _lisp->internWithPackageName(packageNameP, symbolNameP);
  ASSERTNOTNULL(newSym);
  (*resultP) = newSym;
}

void makeSymbol_symsp(core::Symbol_sp *resultP, const char *symbolNameP) {
  core::Symbol_sp newSym = core::Symbol_O::create(symbolNameP);
  ASSERTNOTNULL(newSym);
  (*resultP) = newSym;
}

void makeFixnum(core::T_sp *fnP, gc::Fixnum s) {
  ASSERT(fnP != NULL);
  (*fnP) = core::Fixnum_sp(core::make_fixnum(s));
}

void makeCharacter(core::T_sp *fnP, int s) {
  ASSERT(fnP != NULL);
  (*fnP) = core::clasp_make_character((char)s);
}

void makeBignum(core::T_sp *fnP, const char *cP) {
  _G();
  ASSERT(fnP != NULL);
  string str = cP;
  core::Bignum_sp ns = core::Bignum_O::make(str);
  (*fnP) = ns;
}

void makeString(core::T_sp *fnP, const char *str) {
  _G();
  // placement new into memory passed into this function
  ASSERT(fnP != NULL);
  core::Str_sp ns = core::Str_O::create(str);
  (*fnP) = ns;
}

void makePathname(core::T_sp *fnP, const char *cstr) {
  _G();
  // placement new into memory passed into this function
  ASSERT(fnP != NULL);

  core::Str_sp str = core::Str_O::create(cstr);
  core::Pathname_sp ns = core::cl_pathname(str);
  (*fnP) = ns;
}

void ltv_findPackage(core::T_sp *fnP, const char *cstr) {
  _G();
  // placement new into memory passed into this function
  ASSERT(fnP != NULL);
  string packageName = cstr;
  core::Package_sp pkg = _lisp->findPackage(packageName);
  (*fnP) = pkg;
}

void makeShortFloat(core::T_sp *fnP, double s) {
  _G();
  ASSERT(fnP != NULL);
  (*fnP) = core::ShortFloat_sp(core::ShortFloat_O::create(s));
}

void makeSingleFloat(core::T_sp *fnP, float s) {
  _G();
  ASSERT(fnP != NULL);
  (*fnP) = clasp_make_single_float(s);
}

void makeDoubleFloat(core::T_sp *fnP, double s) {
  _G();
  ASSERT(fnP != NULL);
  (*fnP) = core::DoubleFloat_sp(core::DoubleFloat_O::create(s));
}

void makeComplex(core::T_sp *fnP, double r, double i) {
  _G();
  ASSERT(fnP != NULL);
  (*fnP) = core::Complex_sp(core::Complex_O::create(r, i));
}

#ifdef CLASP_LONG_FLOAT
void makeLongFloat(core::T_sp *fnP, LongFloat s) {
  _G();
  ASSERT(fnP != NULL);
  (*fnP) = core::LongFloat_sp(core::LongFloat_O::create(s));
}
#endif
};

core::T_sp proto_makeCompiledFunction(fnLispCallingConvention funcPtr, char *sourceName, size_t filePos, int lineno, int column, core::T_sp *functionNameP, core::T_sp *compiledFuncsP, core::ActivationFrame_sp *frameP, core::T_sp *lambdaListP) {
  _G();
  // TODO: If a pointer to an integer was passed here we could write the sourceName SourceFileInfo_sp index into it for source line debugging
  core::Str_sp sourceStr = core::Str_O::create(sourceName);
  core::SourceFileInfo_mv sfi = core::core_sourceFileInfo(sourceStr);
  int sfindex = unbox_fixnum(gc::As<core::Fixnum_sp>(sfi.valueGet(1))); // sfindex could be written into the Module global for debugging
  core::SourcePosInfo_sp spi = core::SourcePosInfo_O::create(sfindex, filePos, lineno, column);
  core::FunctionClosure *closure = gctools::ClassAllocator<llvmo::CompiledClosure>::allocateClass(*functionNameP, spi, kw::_sym_function, funcPtr, _Nil<core::T_O>(), *frameP, *compiledFuncsP, *lambdaListP);
  core::CompiledFunction_sp compiledFunction = core::CompiledFunction_O::make(closure);
  return compiledFunction;
};
extern "C" {
void sp_makeCompiledFunction(core::T_sp *resultCompiledFunctionP, fnLispCallingConvention funcPtr, char *sourceName, size_t filePos, int lineno, int column, core::T_sp *functionNameP, core::T_sp *compiledFuncsP, core::ActivationFrame_sp *frameP, core::T_sp *lambdaListP) {
  (*resultCompiledFunctionP) = proto_makeCompiledFunction(funcPtr, sourceName, filePos, lineno, column, functionNameP, compiledFuncsP, frameP, lambdaListP);
}

void mv_makeCompiledFunction(core::T_mv *resultCompiledFunctionP, fnLispCallingConvention funcPtr, char *sourceName, size_t filePos, int lineno, int column, core::T_sp *functionNameP, core::T_sp *compiledFuncsP, core::ActivationFrame_sp *frameP, core::T_sp *lambdaListP) {
  (*resultCompiledFunctionP) = Values(proto_makeCompiledFunction(funcPtr, sourceName, filePos, lineno, column, functionNameP, compiledFuncsP, frameP, lambdaListP));
}
};

extern "C" {
void invokeTopLevelFunction(core::T_mv *resultP,
                            fnLispCallingConvention fptr,
                            core::ActivationFrame_sp *frameP,
                            char *cpname,
                            int *sourceFileInfoHandleP,
                            size_t filePos,
                            int lineno,
                            int column,
                            core::LoadTimeValues_O **ltvPP) {
  ActivationFrame_sp frame = (*frameP);
  SourcePosInfo_sp tempSourcePosInfo = SourcePosInfo_O::create(*sourceFileInfoHandleP, filePos, lineno, column);
  core::Str_sp name = core::Str_O::create(cpname);
  BuiltinClosure tempClosure(name, tempSourcePosInfo, kw::_sym_function);
  core::InvocationHistoryFrame invFrame(&tempClosure, *frameP);
  core::T_sp closedEnv = _Nil<T_O>();
  ASSERT(ltvPP != NULL);
  core::LoadTimeValues_O *ltvP = *ltvPP;
  ASSERT(ltvP != NULL);
#define TIME_TOP_LEVEL_FUNCTIONS
#ifdef TIME_TOP_LEVEL_FUNCTIONS
  core::Number_sp startTime;
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    startTime = gc::As<core::Number_sp>(core::cl_getInternalRealTime());
  }
#endif
  // Evaluate the function
  fptr(resultP, LCC_FROM_SMART_PTR(closedEnv), LCC_PASS_ARGS1(ltvP));
#ifdef TIME_TOP_LEVEL_FUNCTIONS
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    core::Number_sp endTime = gc::As<core::Number_sp>(core::cl_getInternalRealTime());
    core::Number_sp diff = core::contagen_sub(endTime, startTime);
    core::Number_sp seconds = core::contagen_div(diff, gc::As<Number_sp>(cl::_sym_internalTimeUnitsPerSecond->symbolValue()));
    double dseconds = clasp_to_double(seconds);
    core::SourceFileInfo_sp sfi = core::core_sourceFileInfo(core::make_fixnum(*sourceFileInfoHandleP));
    printf("TOP-LEVEL-FUNCTION-TIME %lf %s %d\n", dseconds, sfi->namestring().c_str(), lineno);
  }
#endif
  ASSERTNOTNULL(*resultP);
};

void invokeMainFunctions(T_mv *result, fnLispCallingConvention fptr[], int *numfunP) {
  int numfun = *numfunP;
  //        printf("%s:%d invokeMainFunctions(%d) fptr[] = %p\n", __FILE__, __LINE__, numfun, fptr);
  for (int i = 0; i < numfun; ++i) {
    //            printf("%s:%d invoking fptr[%d] @%p\n", __FILE__, __LINE__, i, (void*)fptr[i]);

    (fptr[i])(result, _Nil<core::T_O>().raw_(), LCC_PASS_ARGS0());
  }
}

void invokeLlvmFunctionVoid(fnLispCallingConvention fptr) {
  core::T_mv result;
  core::T_sp env = _Nil<core::T_O>();
  fptr(&result, env.raw_(), LCC_PASS_ARGS0());
};

extern void sp_symbolValueReadOrUnbound(core::T_sp *resultP, const core::Symbol_sp *symP) {
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  *resultP = (*symP)->symbolValueUnsafe();
  ASSERTNOTNULL(*resultP);
}
extern void mv_symbolValueReadOrUnbound(core::T_mv *resultP, const core::Symbol_sp *symP) {
  _G();
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  *resultP = Values((*symP)->symbolValueUnsafe());
  ASSERTNOTNULL(*resultP);
}

extern void sp_symbolValueRead(core::T_sp *resultP, const core::Symbol_sp *symP) {
  _G();
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  *resultP = (*symP)->symbolValue();
  ASSERTNOTNULL(*resultP);
}
extern void mv_symbolValueRead(core::T_mv *resultP, const core::Symbol_sp *symP) {
  _G();
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  *resultP = Values((*symP)->symbolValue());
  ASSERTNOTNULL(*resultP);
}

core::T_sp *symbolValueReference(core::Symbol_sp *symbolP) {
  _G();
  ASSERT(symbolP != NULL);
  return ((*symbolP)->valueReference());
}

extern core::T_sp *lexicalValueReference(int depth, int index, core::ActivationFrame_sp *frameP) {
  ASSERT(frameP != NULL);
  return const_cast<core::T_sp *>(&((*frameP)->lookupValueReference(depth, index)));
}
};

core::T_sp proto_lexicalValueRead(int depth, int index, core::ActivationFrame_sp *renvP) {
  ActivationFrame_sp renv = *renvP;
  core::T_sp res = core::Environment_O::clasp_lookupValue(renv, depth, index);
  return res;
}

extern "C" {
void sp_lexicalValueRead(core::T_sp *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = proto_lexicalValueRead(depth, index, renvP);
  ASSERTNOTNULL(*resultP);
}
void mv_lexicalValueRead(core::T_mv *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = Values(proto_lexicalValueRead(depth, index, renvP));
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {

extern void makeValueFrame(core::T_sp *resultActivationFrameP, int numargs, int id)
// was ActivationFrame_sp
{
  _G();
  ASSERT(resultActivationFrameP != NULL);
  core::ValueFrame_sp valueFrame(core::ValueFrame_O::create(numargs, _Nil<core::T_O>()));
  valueFrame->setEnvironmentId(id);
  (*resultActivationFrameP) = valueFrame;
  //        printf("%s:%d makeValueFrame address &result->%p (&result)->px->%p valueFrame->%p\n", __FILE__, __LINE__, resultActivationFrameP, resultActivationFrameP->px_ref(), valueFrame.px_ref() );
}

extern void makeTagbodyFrame(core::ActivationFrame_sp *resultP)
// was ActivationFrame_sp
{
  _G();
  ASSERT(resultP != NULL);
  core::TagbodyFrame_sp tagbodyFrame(core::TagbodyFrame_O::create(_Nil<core::T_O>()));
  (*resultP) = tagbodyFrame;
  ASSERTNOTNULL(*resultP);
}

extern void makeValueFrameFromReversedCons(core::ActivationFrame_sp *afP, core::T_sp *consP, uint id) {
  _G();
  core::List_sp cons = (*consP);
  core::ValueFrame_sp vf = core::ValueFrame_O::createFromReversedCons(cons, _Nil<core::T_O>());
  vf->setEnvironmentId(id);
  (*afP) = vf;
  ASSERTNOTNULL(*afP);
}

extern void setParentOfActivationFrameTPtr(core::T_sp *resultP, core::T_O *parentP) {
  if (resultP->framep()) {
    frame::SetParentFrame(*resultP, gctools::smart_ptr<core::T_O>((gc::Tagged)parentP));
    return;
  } else if (ActivationFrame_sp af = gc::As<ActivationFrame_sp>((*resultP))) {
    af->setParentFrame(parentP);
    return;
  }
  errorMessage(destinationMustBeActivationFrame);
}

extern void setParentOfActivationFrame(core::T_sp *resultP, core::T_sp *parentsp) {
  T_O *parentP = parentsp->raw_();
  if (resultP->framep()) {
    frame::SetParentFrame(*resultP, gctools::smart_ptr<core::T_O>((gc::Tagged)parentP));
    return;
  } else if (ActivationFrame_sp af = gc::As<ActivationFrame_sp>((*resultP))) {
    af->setParentFrame(parentP);
    return;
  }
  errorMessage(destinationMustBeActivationFrame);
}

extern void attachDebuggingInfoToValueFrame(core::ActivationFrame_sp *resultP,
                                            core::T_sp *debuggingInfoP) {
  ASSERT(resultP != NULL);
  ASSERT(debuggingInfoP != NULL);
  ASSERT((*resultP));
  core::ValueFrame_sp vf = gc::As<core::ValueFrame_sp>((*resultP));
  core::VectorObjects_sp vo = gc::As<core::VectorObjects_sp>((*debuggingInfoP));
  ASSERTF(vf->length() == vo->length(), BF("There is a mismatch between the size of the ValueFrame[%d] and the number of Symbols[%d] attaching to it") % vf->length() % vo->length());
  vf->attachDebuggingInfo(vo);
}

ALWAYS_INLINE extern core::T_sp *valueFrameReference(core::ActivationFrame_sp *frameP, int idx) {
  _G();
  ASSERT(frameP != NULL);
  ASSERT((*frameP));
  ASSERTF(idx >= 0 && idx < ((*frameP)->length()), BF("Illegal value of idx[%d] must be in range [0<=idx<%d]") % idx % (*frameP)->length());
  core::ValueFrame_sp frame = gc::As<core::ValueFrame_sp>((*frameP));
  core::T_sp *pos_gc_safe = const_cast<core::T_sp *>(&frame->entryReference(idx));
  return pos_gc_safe;
}

ALWAYS_INLINE extern core::T_sp *valueFrameReferenceWithOffset(core::ActivationFrame_sp *frameP, int idx, int offset) {
  int ridx = idx + offset;
  ASSERT(frameP != NULL);
  ASSERT(*frameP);
  ASSERT(ridx >= 0 && ridx < (*frameP)->length());
  core::ValueFrame_sp frame = gc::As<core::ValueFrame_sp>((*frameP));
  core::T_sp *pos_gc_safe = const_cast<core::T_sp *>(&frame->entryReference(ridx));
  return pos_gc_safe;
}

extern void makeFunctionFrame(core::ActivationFrame_sp *resultP, int numargs, core::ActivationFrame_sp *parentP)
// was ActivationFrame_sp
{
  _G();
  ASSERT(resultP != NULL);
  ASSERT(parentP != NULL);
  (*resultP) = core::FunctionFrame_sp(core::FunctionFrame_O::create(numargs, (*parentP)));
  ASSERTNOTNULL(*resultP);
}

extern core::T_sp *functionFrameReference(core::ActivationFrame_sp *frameP, int idx) {
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  core::FunctionFrame_sp frame = gc::As<core::FunctionFrame_sp>((*frameP));
  if (idx < 0 || idx >= frame->length()) {
    errorMessage(invalidIndexForFunctionFrame
                 ,clasp_make_fixnum(idx)
                 ,clasp_make_fixnum(frame->length()));
  }
  core::T_sp *pos_gc_safe = const_cast<core::T_sp *>(&frame->entryReference(idx));
  return pos_gc_safe;
}

#if 0
    extern void fillRestTarget( core::T_sp* restP, core::ActivationFrame_sp* frameP, int startRest, char* fnName)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT(frameP->objectp());
	core::ValueFrame_sp frame = (*frameP).as<core::ValueFrame_O>();
	core::List_sp result = _Nil<core::T_O>();
	for ( int i=frame->length()-1; i>=startRest; i-- )
	{
	    result = core::Cons_O::create(frame->entry(i),result);
	}
	(*restP) = result;
	ASSERTNOTNULL(*restP);
    }
#endif

/*! Look for the :allow-other-keywords XX keyword argument and
      calculate (or (*ampAllowOtherKeywordsP) XX) return 1 if result is true otherwise 0 */
extern int checkForAllowOtherKeywords(int ampAllowOtherKeywords, core::ActivationFrame_sp *frameP, int argIdx) {
  _G();
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  ASSERT(argIdx >= 0);
  core::ValueFrame_sp vf = gc::As<core::ValueFrame_sp>((*frameP));
  if (argIdx >= vf->length())
    return 0;
  int argsLeft = vf->length() - argIdx;
  if ((argsLeft % 2) != 0) {
    stringstream serr;
    serr << "There must be an even number of keyword arguments - you passed: ";
    for (int ei = argIdx; ei < vf->length(); ei++) {
      serr << _rep_(vf->entry(ei)) << " ";
    }
    SIMPLE_ERROR(BF("%s") % serr.str());
  }
  if (ampAllowOtherKeywords)
    return 1;
  for (int ii = argIdx; ii < vf->length(); ii += 2) {
    if (vf->entry(ii) == kw::_sym_allow_other_keys) {
      core::T_sp val = vf->entry(ii + 1);
      if (val.isTrue())
        return 1;
      // TODO: Handle :allow-other-keys nil :allow-other-keys t
      // In safe mode this should throw an exceptions
      // (see 3.4.1.4.1.1 Examples of Suppressing Keyword Argument Checking)
    }
  }
  return 0;
}

/*! Look for the keyword in (*frameP) after argIdx.
 */
extern void throwIfExcessKeywordArguments(char *fnName, core::ActivationFrame_sp *frameP, int argIdx) {
  _G();
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  ASSERT(argIdx >= 0);
  if (argIdx >= (*frameP)->length())
    return;
  stringstream ss;
  for (int i(0); i < (*frameP)->length(); ++i) {
    ss << _rep_((*frameP)->entry(i)) << " ";
  }
  SIMPLE_ERROR(BF("Excess keyword arguments fnName: %s argIdx: %d  args: %s") % fnName % argIdx % ss.str());
//      core::throwUnrecognizedKeywordArgumentError((*frameP)->argArray()[argIdx]);
#if 0
	core::ActivationFrame_sp frame = (*frameP).as<core::ActivationFrame_O>();
	if ( argIdx >= frame->length() ) return;
	stringstream ss;
	for ( int ii = argIdx; ii < frame->length(); ii+=2 )
	{
	    core::T_sp& keyRef = frame->entryReference(ii);
	    if ( keyRef == kw::_sym_allow_other_keys ) continue;
	    ss << _rep_(keyRef) << " ";
	}
	SIMPLE_ERROR(BF("In %s extraneous keyword arguments: %s") % fnName % ss.str() );
#endif
}

extern void throwIfExcessArguments(core::T_sp *frameP, int argIdx) {
  _G();
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  ASSERT(argIdx >= 0);
  core::ActivationFrame_sp frame = gc::As<core::ActivationFrame_sp>((*frameP));
  if (argIdx < frame->length()) {
    stringstream serr;
    for (int i = argIdx; i < frame->length(); i++) {
      serr << _rep_(frame->entry(i)) << " ";
    }
    SIMPLE_ERROR(BF("extraneous arguments: %s") % serr.str());
  }
}
};

inline core::T_sp prependMultipleValues(core::T_mv *multipleValuesP) {
  _G();
  core::List_sp result = _Nil<core::T_O>();
  core::T_mv &mv = (*multipleValuesP);
  if (mv.number_of_values() > 0) {
    result = core::Cons_O::create(mv, result);
    for (int i = 1; i < mv.number_of_values(); i++) {
      result = core::Cons_O::create(mv.valueGet(i), result);
    }
  }
  return result;
}

extern "C" {
void sp_prependMultipleValues(core::T_sp *resultP, core::T_mv *multipleValuesP) {
  (*resultP) = prependMultipleValues(multipleValuesP);
  ASSERTNOTNULL(*resultP);
}
void mv_prependMultipleValues(core::T_mv *resultP, core::T_mv *multipleValuesP) {
  (*resultP) = Values(prependMultipleValues(multipleValuesP));
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {

/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
void sp_symbolFunctionRead(core::T_sp *resultP, const core::Symbol_sp *symP) {
  _G();
  ASSERT(resultP != NULL);
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  ASSERTF((*symP)->fboundp(), BF("There is no function bound to symbol[%s]") % _rep_((*symP)));
  (*resultP) = (*symP)->symbolFunction();
  ASSERTNOTNULL(*resultP);
}
void mv_symbolFunctionRead(core::T_mv *resultP, const core::Symbol_sp *symP) {
  _G();
  ASSERT(resultP != NULL);
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  ASSERTF((*symP)->fboundp(), BF("There is no function bound to symbol[%s]") % _rep_((*symP)));
  (*resultP) = Values((*symP)->symbolFunction());
  ASSERTNOTNULL(*resultP);
}

/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
extern void setfSymbolFunctionRead(core::T_sp *resultP, const core::Symbol_sp *symP) {
  _G();
  ASSERT(resultP != NULL);
  ASSERTF(symP != NULL, BF("passed symbol is NULL"));
  core::Function_sp setfFunc = (*symP)->getSetfFdefinition(); //_lisp->get_setfDefinition(*symP);
  ASSERTF(setfFunc, BF("There is no setf function bound to symbol[%s]") % _rep_((*symP)));
  (*resultP) = setfFunc;
  ASSERTNOTNULL(*resultP);
}
};

core::T_sp proto_lexicalFunctionRead(int depth, int index, core::ActivationFrame_sp *renvP) {
  ASSERT(renvP != NULL);
  LOG(BF("About to lexicalFunction depth[%d] index[%d]") % depth % index);
  LOG(BF("(*renvP) --> %s") % (*renvP)->__repr__());
  core::Function_sp res = core::Environment_O::clasp_lookupFunction((*renvP), depth, index);
  return res;
}

extern "C" {
void sp_lexicalFunctionRead(core::T_sp *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = proto_lexicalFunctionRead(depth, index, renvP);
  ASSERTNOTNULL(*resultP);
}
void mv_lexicalFunctionRead(core::T_mv *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = Values(proto_lexicalFunctionRead(depth, index, renvP));
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {
int activationFrameSize(core::ActivationFrame_sp *activationFrameP) {
  _G();
  ASSERT(activationFrameP != NULL);
  return (*activationFrameP)->length();
}

#if 0
/*! Return a pointer to the shared_ptr of the activation frames parent */
    extern core::ActivationFrame_sp* activationFrameParentRef(core::T_sp* frameP)
    {_G();
	ASSERT(frameP!=NULL);
	if ( (*frameP).nilp() ) return &(*frameP); // _Nil<core::ActivationFrame_O>();
	return &((*frameP)->parentFrameRef());
    }

/*! Return a pointer to the shared_ptr of the activation frames parent */
    extern core::ActivationFrame_sp* activationFrameNil()
    {_G();
	return &_lisp->activationFrameNil();
    }
#endif

void throwTooManyArgumentsException(const char *funcName, core::ActivationFrame_sp *afP, int givenNumberOfArguments, int requiredNumberOfArguments) {
  _G();
  core::throwTooManyArgumentsError(givenNumberOfArguments, requiredNumberOfArguments);
  //	SIMPLE_ERROR(BF("In %s too many arguments passed - got %d and expected %d - args: %s") % funcName % givenNumberOfArguments % requiredNumberOfArguments % _rep_((*(afP))) );
}

void throwNotEnoughArgumentsException(const char *funcName, core::ActivationFrame_sp *afP, int givenNumberOfArguments, int requiredNumberOfArguments) {
  _G();
  core::throwTooFewArgumentsError(givenNumberOfArguments, requiredNumberOfArguments);
  SIMPLE_ERROR(BF("In %s not enough arguments passed - got %d and expected %d - args: %s") % funcName % givenNumberOfArguments % requiredNumberOfArguments % _rep_((*(afP))));
}

void gdb() {
  _G();
  printf("%s:%d Set a breakpoint here to invoke gdb\n", __FILE__, __LINE__);
}

void debugInspectActivationFrame(core::ActivationFrame_sp *afP) {
  _G();
  core::ActivationFrame_sp af = (*afP);
  printf("debugInspectActivationFrame: %s\n", _rep_(af).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect af\n", __FILE__, __LINE__);
}

void debugInspectT_sp(core::T_sp *objP) {
  _G();
  core::T_sp obj = (*objP);
  printf("debugInspectObject@%p  obj.px_ref()=%p: %s\n", (void *)objP, (*objP).raw_(), _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
}

void debugInspectTPtr(core::T_O *tP) {
  core::T_sp obj = gctools::smart_ptr<core::T_O>((gc::Tagged)tP);
  printf("debugInspectTPtr@%p  obj.px_ref()=%p: %s\n", (void *)tP, obj.raw_(), _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
}

void debugInspectT_mv(core::T_mv *objP) {
  _G();
  printf("debugInspectT_mv@%p  #val=%d\n", (void *)objP, objP->number_of_values());
  printf("   debugInspectT_mv  obj= %s\n", _rep_(*objP).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
}

void debugMessage(const char *msg) {
  _G();
  printf("++++++ DEBUG-MESSAGE: %s \n", msg);
}

void debugPointer(const unsigned char *ptr) {
  _G();
  printf("++++++ debugPointer: %p \n", ptr);
}

void debugSymbolPointer(core::Symbol_sp *ptr) {
  _G();
  printf("++++++ debugSymbolPointer: %s\n", _rep_(*ptr).c_str());
}

void debugSymbolValue(core::Symbol_sp sym) {
  _G();
  printf("+++++ debugSymbolValue: %s\n", _rep_(core::af_symbolValue(sym)).c_str());
}

void debugPrintObject(const char *msg, core::T_sp *objP) {
  _G();
  std::cout << "++++++ JIT-PRINT-OBJECT: ";
  std::cout << msg << " --> ";
  if (objP == NULL) {
    std::cout << "objP is UNDEFINED";
  } else if (!(*objP).objectp()) {
    std::cout << "(*objP) is UNDEFINED";
  } else {
    std::cout << _rep_((*objP));
  }
  std::cout << std::endl;
}

void debugPrintI32(int i32) {
  _G();
  printf("+++DBG-I32[%d]\n", i32);
}

void debugPrint_size_t(size_t v) {
  _G();
  printf("+++DBG-size_t[%lu]\n", v);
}

void singleStepCallback() {
}

         
void throwCatchThrow(core::T_sp *tagP) {
  ASSERT(tagP != NULL);
  core::T_sp tag = *tagP;
  int frame = _lisp->exceptionStack().findKey(CatchFrame, tag);
  if (frame < 0) {
    CONTROL_ERROR();
  } else {
    core::CatchThrow catchThrow(frame);
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
      printf("%s:%d Throwing core::CatchThrow exception tag[%s] frame: %d\n", __FILE__, __LINE__, (*tagP)->__repr__().c_str(), frame);
      printf("   %s\n", _lisp->exceptionStack().summary().c_str());
    }
#endif
    throw catchThrow;
  }
  SIMPLE_ERROR(BF("This should never happen"));
}

void throwReturnFrom(core::Symbol_sp *blockSymbolP) {
  ASSERT(blockSymbolP != NULL);
  core::T_sp blockSymbol = *blockSymbolP;
  int frame = _lisp->exceptionStack().findKey(BlockFrame, blockSymbol);
  if (frame < 0) {
    CONTROL_ERROR();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Throwing core::ReturnFrom exception frame[%d]\n", frame);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  core::ReturnFrom returnFrom(frame);
  throw returnFrom;
}
};

core::T_mv proto_blockHandleReturnFrom(unsigned char *exceptionP, size_t frame) {
  core::ReturnFrom &returnFrom = (core::ReturnFrom &)*((core::ReturnFrom *)(exceptionP));
  if (returnFrom.getFrame() == frame) {
    return gctools::multiple_values<T_O>::createFromValues();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Re-throwing core::ReturnFrom exception frame[%d]\n", returnFrom.getFrame());
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  throw returnFrom;
}

extern "C" {

void sp_blockHandleReturnFrom(core::T_sp *resultP, unsigned char *exceptionP, size_t frame) {
  (*resultP) = proto_blockHandleReturnFrom(exceptionP, frame);
  ASSERTNOTNULL(*resultP);
}
void mv_blockHandleReturnFrom(core::T_mv *resultP, unsigned char *exceptionP, size_t frame) {
  (*resultP) = proto_blockHandleReturnFrom(exceptionP, frame);
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {

// Return 1 if exception depth is zero				- blockTestDepth0
// Set the result from the ReturnFrom exception return value   	- blockStoreResult
//

size_t pushCatchFrame(core::T_sp *tagP) {
  _G();
  ASSERT(tagP != NULL);
  size_t result = _lisp->exceptionStack().push(CatchFrame, *tagP);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Pushed CatchThrow frame[%zu]\n", result);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  return result;
}

size_t pushBlockFrame(core::Symbol_sp *tagP) {
  _G();
  ASSERT(tagP != NULL);
  core::T_sp osym = *tagP;
  size_t result = _lisp->exceptionStack().push(BlockFrame, osym);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Pushed Block frame[%zu]\n", result);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  return result;
}

size_t pushTagbodyFrame(core::ActivationFrame_sp *afP) {
  _G();
  ASSERT(afP != NULL);
  core::T_sp tagbodyId = (*afP);
  size_t result = _lisp->exceptionStack().push(TagbodyFrame, tagbodyId);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Pushed Tagbody frame[%zu]\n", result);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  return result;
}
};

core::T_mv proto_ifCatchFrameMatchesStoreResultElseRethrow(size_t catchFrame, unsigned char *exceptionP) {
  _G();
  core::CatchThrow *ctExceptionP = reinterpret_cast<core::CatchThrow *>(exceptionP);
  if (catchFrame == ctExceptionP->getFrame()) {
    return gctools::multiple_values<core::T_O>::createFromValues(); // ctExceptionP->getReturnedObject();
  }
// rethrow the exception
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Re-throwing CatchThrow frame[%d]\n", ctExceptionP->getFrame() );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  throw * ctExceptionP;
}

extern "C" {
void sp_ifCatchFrameMatchesStoreResultElseRethrow(core::T_sp *resultP, size_t catchFrame, unsigned char *exceptionP) {
  (*resultP) = proto_ifCatchFrameMatchesStoreResultElseRethrow(catchFrame, exceptionP);
  ASSERTNOTNULL(*resultP);
}
void mv_ifCatchFrameMatchesStoreResultElseRethrow(core::T_mv *resultP, size_t catchFrame, unsigned char *exceptionP) {
  (*resultP) = proto_ifCatchFrameMatchesStoreResultElseRethrow(catchFrame, exceptionP);
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {
void exceptionStackUnwind(size_t frame) {
  _G();
  _lisp->exceptionStack().unwind(frame);
}

void throwIllegalSwitchValue(size_t val, size_t max) {
  _G();
  SIMPLE_ERROR(BF("Illegal switch value %lu - max value is %lu") % val % max);
}

void throw_LexicalGo(int depth, int index) {
  _G();
  core::LexicalGo lgo(depth, index);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Throwing core::Go depth[%d] index[%d]\n", depth, index);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  throw lgo;
}

void throwDynamicGo(size_t depth, size_t index, core::ActivationFrame_sp *afP) {
  _G();
  T_sp tagbodyId = core::Environment_O::clasp_lookupTagbodyId((*afP), depth, index);
  int frame = _lisp->exceptionStack().findKey(TagbodyFrame, tagbodyId);
  if (frame < 0) {
    CONTROL_ERROR();
  }
  core::DynamicGo dgo((size_t)frame, index);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Throwing core::DynamicGo tagbodyIdP[%zu] index[%zu]\n", dgo.getFrame(), dgo.index());
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  throw dgo;
}

int tagbodyLexicalGoIndexElseRethrow(char *exceptionP) {
  IMPLEMENT_MEF(BF("Update me"));
#if 0
	core::LexicalGo* goExceptionP = (core::LexicalGo*)(exceptionP);
	if ( goExceptionP->depth() == 0 )
	{
	    return goExceptionP->index();
	}
	goExceptionP->decrementDepth();
#ifdef DEBUG_FLOW_CONTROL
	printf("Re-throwing core::Go depth[%d] index[%d]\n", goExceptionP->depth(), goExceptionP->index());
        printf("   %s\n", _lisp->exceptionStack().summary().c_str());
#endif
	throw *goExceptionP;
#endif
}

size_t tagbodyDynamicGoIndexElseRethrow(char *exceptionP, size_t frame) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d tagbodyDynamicGoIndexElseRethrow  frame: %lu\n", __FILE__, __LINE__, frame);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  core::DynamicGo *goExceptionP = reinterpret_cast<core::DynamicGo *>(exceptionP);
  if (goExceptionP->getFrame() == frame) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d Matched DynamicGo  frame: %lu  index: %lu\n", __FILE__, __LINE__, goExceptionP->getFrame(), goExceptionP->index());
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
    return goExceptionP->index();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("Re-throwing core::DynamicGo frame[%zu] index[%zu]\n", goExceptionP->getFrame(), goExceptionP->index());
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  throw * goExceptionP;
}

void getOrCreateLoadTimeValueArray(core::LoadTimeValues_O **ltvPP, const char *moduleName, int numberOfLoadTimeValues, int numberOfLoadTimeSymbols) {
  core::LoadTimeValues_sp loadTimeValues = _lisp->getOrCreateLoadTimeValues(moduleName, numberOfLoadTimeValues, numberOfLoadTimeSymbols);
  *ltvPP = &(*loadTimeValues); //reinterpret_cast<core::LoadTimeValues_O*>(loadTimeValues.pbase());
                               //	printf("%s:%d  getOrCreateLoadTimeValueArray ltvPP=%p  *ltvPP=%p\n", __FILE__, __LINE__, ltvPP, *ltvPP );
}

void dumpLoadTimeValues(core::LoadTimeValues_O *ltvP) {
  ltvP->dump();
}

void assignSourceFileInfoHandle(const char *moduleName, const char *sourceDebugPathname, size_t sourceDebugOffset, int useLineno, int *sourceFileInfoHandleP) {
  //	printf("%s:%d assignSourceFileInfoHandle %s\n", __FILE__, __LINE__, moduleName );
  core::Str_sp mname = core::Str_O::create(moduleName);
  core::Str_sp struename = core::Str_O::create(sourceDebugPathname);
  SourceFileInfo_mv sfi_mv = core::core_sourceFileInfo(mname, struename, sourceDebugOffset, useLineno ? true : false);
  int sfindex = unbox_fixnum(gc::As<core::Fixnum_sp>(sfi_mv.valueGet(1)));
#if 0
	if ( sfindex == 0 ) {
	    printf("%s:%d Could not get a SourceFileInfoHandle for %s\n", __FILE__, __LINE__, moduleName );
	} else {
	    printf("%s:%d Assigning SourceFileInfoHandle %d for %s  at sourceFileInfoHandleP@%p\n", __FILE__, __LINE__, sfindex, moduleName, sourceFileInfoHandleP );
	}
#endif
  *sourceFileInfoHandleP = sfindex;
}

void debugSourceFileInfoHandle(int *sourceFileInfoHandleP) {
  int sfindex = *sourceFileInfoHandleP;
  core::Fixnum_sp fn = core::make_fixnum(sfindex);
  SourceFileInfo_sp sfi = core::core_sourceFileInfo(fn);
  printf("%s:%d debugSourceFileInfoHandle[%d] --> %s\n", __FILE__, __LINE__, sfindex, _rep_(sfi).c_str());
}
};

inline core::T_sp proto_copyLoadTimeValue(core::LoadTimeValues_O **ltvPP, int index) {
  core::LoadTimeValues_O &ltv = **ltvPP;
  return ltv.data_element(index);
}
extern "C" {
void sp_copyLoadTimeValue(core::T_sp *resultP, core::LoadTimeValues_O **ltvPP, int index) {
  ASSERT(resultP != NULL);
  ASSERT(ltvPP != NULL);
  ASSERT((*ltvPP) != NULL);
  core::T_sp val = proto_copyLoadTimeValue(ltvPP, index);
#if defined(USE_MPS) && defined(DEBUG_LOAD_TIME_VALUES)
  if (core::_sym_STARdebugLoadTimeValuesSTAR && core::_sym_STARdebugLoadTimeValuesSTAR->symbolValue().notnilp()) {
    stringstream ss;
    ss << (BF("%s:%d sp_copyLoadTimeValue@%p  _Objects@%p  index[%d]  result client@%p  value: %s  cl::_sym_destructuring_bind@%p") % __FILE__ % __LINE__ % *ltvPP % (*ltvPP)->_Objects._Vector._Contents % index % val.pbase() % _rep_(val).c_str() % cl::_sym_destructuring_bind.pbase()).str();
    printf("%s\n", ss.str().c_str());
  }
#endif
  (*resultP) = val;
  ASSERTNOTNULL(*resultP);
}
void mv_copyLoadTimeValue(core::T_mv *resultP, core::LoadTimeValues_O **ltvPP, int index) {
  ASSERT(resultP != NULL);
  ASSERT(ltvPP != NULL);
  ASSERT((*ltvPP) != NULL);
  (*resultP) = Values(proto_copyLoadTimeValue(ltvPP, index));
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d mv_copyTimeValue@%p  index[%d]  result client@%p  value: %s\n", __FILE__, __LINE__, *ltvPP, index, (*resultP).pbase(), _rep_(*resultP).c_str());
#endif
  ASSERTNOTNULL(*resultP);
}
};

inline core::T_sp proto_getLoadTimeValue(core::LoadTimeValues_O **ltvPP, int index) {
  return (*ltvPP)->data_element(index);
}

extern "C" {
void sp_getLoadTimeValue(core::T_sp *resultP, core::LoadTimeValues_O **ltvPP, int index) {
  (*resultP) = proto_getLoadTimeValue(ltvPP, index);
  ASSERTNOTNULL(*resultP);
}
void mv_getLoadTimeValue(core::T_mv *resultP, core::LoadTimeValues_O **ltvPP, int index) {
  (*resultP) = Values(proto_getLoadTimeValue(ltvPP, index));
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {

void ltv_findBuiltInClass(core::T_sp *resultP, core::T_sp *symbolP) {
  ASSERT(resultP != NULL);
  *resultP = core::cl_findClass(*symbolP, true, _Nil<core::T_O>());
  ASSERTNOTNULL(*resultP);
}

void ltv_makeCons(core::T_sp *resultP) {
  ASSERT(resultP != NULL);
  (*resultP) = core::Cons_O::create();
  ASSERTNOTNULL(*resultP);
}
#if 0
    void ltv_makeSourceCodeCons(core::T_sp* resultP, const char* sourceFileNameP, int lineNo, int column)
    {_G();
	ASSERT(resultP!=NULL);
	(*resultP) = core::Cons_O::create(lineNo,column,core::SourceFileInfo_O::getOrCreate(sourceFileNameP),_lisp);
	ASSERTNOTNULL(*resultP);
    }
#endif
void ltv_makeArrayObjects(core::T_sp *resultP, core::T_sp *elementTypeP, int rank, int dimensions[]) {
  ASSERT(resultP != NULL);
  ASSERT(elementTypeP != NULL);
  if (rank == 1) // vector
  {
    *resultP = core::VectorObjects_O::create(_Nil<core::T_O>(), dimensions[0], *elementTypeP);
  } else {
    core::List_sp dims = _Nil<core::T_O>();
    for (int i = rank - 1; i >= 0; i--) {
      dims = core::Cons_O::create(core::make_fixnum(dimensions[i]), dims);
    }
    *resultP = core::ArrayObjects_O::make(dims, cl::_sym_T_O, *elementTypeP,_lisp->_true());
  }
  ASSERTNOTNULL(*resultP);
}

void ltv_makeHashTable(core::T_sp *resultP, core::T_sp *testP) {
  ASSERT(resultP != NULL);
  (*resultP) = core::HashTable_O::create(*testP);
  ASSERTNOTNULL(*resultP);
}

void rplaca(core::T_sp *resultP, core::T_sp *carP) {
  ASSERT(resultP != NULL);
  gc::As<core::Cons_sp>((*resultP))->setCar(*carP);
}

void rplacd(core::T_sp *resultP, core::T_sp *carP) {
  ASSERT(resultP != NULL);
  gc::As<core::Cons_sp>((*resultP))->setCdr(*carP);
}

void ltv_initializeArrayObjectsRowMajorArefOrder(core::T_sp *arrayP, core::LoadTimeValues_O **ltvPP, int *indices) {
  core::Array_sp array = gc::As<core::Array_sp>((*arrayP));
  int arrayTotalSize = array->arrayTotalSize();
  for (int i = 0; i < arrayTotalSize; i++) {
    array->rowMajorAset(i, (*ltvPP)->data_element(indices[i]));
  }
}

void ltv_initializeHashTable(core::T_sp *hashTableP, int numEntries, core::LoadTimeValues_O **ltvPP, int *indices) {
  core::HashTable_sp hashTable = gc::As<core::HashTable_sp>((*hashTableP));
  int j = 0;
  for (int i = 0; i < numEntries; i++) {
    int ikey = indices[j];
    int ival = indices[j + 1];
    core::T_sp key = (*ltvPP)->data_element(ikey);
    core::T_sp val = (*ltvPP)->data_element(ival);
    hashTable->hash_table_setf_gethash(key, val);
    j += 2;
  }
}

void saveToMultipleValue0(core::T_mv *mvP) {
  MultipleValues &mv = lisp_multipleValues();
  mv.valueSet(0, *mvP);
}

void sp_restoreFromMultipleValue0(core::T_sp *resultP) {
  MultipleValues &mv = lisp_multipleValues();
  (*resultP).setRaw_(reinterpret_cast<gc::Tagged>(mv[0]));
}

void mv_restoreFromMultipleValue0(core::T_mv *resultP) {
  MultipleValues &mv = lisp_multipleValues();
  (*resultP) = core::T_mv(mv[0], mv.getSize());
}

/*! Copy the current MultipleValues in _lisp->values() into a VectorObjects */
extern void saveValues(core::T_sp *resultP, core::T_mv *mvP) {
  _G();
  ASSERT(resultP != NULL);
  ASSERT(mvP != NULL);
  int numValues = (*mvP).number_of_values();
  core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(), numValues, core::T_O::___staticClass);
  //	printf("intrinsics.cc saveValues numValues = %d\n", numValues );
  if (numValues > 0) {
    vo->setf_elt(0, (*mvP));
  }
  for (int i(1); i < (*mvP).number_of_values(); ++i) {
    vo->setf_elt(i, (*mvP).valueGet(i));
  }
  (*resultP) = vo;
  ASSERTNOTNULL(*resultP);
}

/*! Copy the current MultipleValues in _lisp->values() into a VectorObjects */
extern void loadValues(core::T_mv *resultP, core::T_sp *vectorObjectsP) {
  _G();
  ASSERT(resultP != NULL);
  ASSERT(vectorObjectsP != NULL);
  if (!(*vectorObjectsP)) {
    // If there was a non-local exit then *vectorObjectP will be NULL
    // check for that here and if so set the result to gctools::multiple_values<core::T_O>()
    (*resultP) = gctools::multiple_values<core::T_O>();
    return;
  }
  ASSERTF(*vectorObjectsP, BF("*vectorObjectsP is UNDEFINED"));
  core::VectorObjects_sp vo = gc::As<core::VectorObjects_sp>((*vectorObjectsP));
  //	printf("intrinsics.cc loadValues vo->length() = %d\n", vo->length() );
  if (vo->length() == 0) {
    (*resultP) = gctools::multiple_values<core::T_O>();
    return;
  }
  (*resultP) = gctools::multiple_values<core::T_O>(vo->elt(0), vo->length());
  for (int i(1); i < vo->length(); ++i) {
    (*resultP).valueSet(i, vo->elt(i));
  }
}

/*! If saw_aok > 0 then return that.
      Otherwise check the following argument - if true then return 2 --> :a-o-k t
      Otherwise return 1 --> :a-o-k nil
    */
int kw_allowOtherKeywords(int saw_aok, core::ActivationFrame_sp *afP, int argIdx) {
  if (saw_aok)
    return saw_aok;
  ASSERTNOTNULL(*afP);
  core::ValueFrame_sp valueFrame = gc::As<core::ValueFrame_sp>((*afP));
  bool aokTrue = valueFrame->entryReference(argIdx + 1).isTrue();
  return aokTrue ? 2 : 1;
}

void kw_ifNotKeywordException(core::T_O **objP) {
  ASSERT(objP != NULL);
  if (!af_keywordP((gc::smart_ptr<core::T_O>((gc::Tagged) * objP)))) {
    SIMPLE_ERROR(BF("Not keyword %s") % _rep_(gctools::smart_ptr<core::T_O>((gc::Tagged) * objP)));
    //            core::throwUnrecognizedKeywordArgumentError(*objP);
  }
}

size_t kw_trackFirstUnexpectedKeyword(size_t badKwIdx, size_t newBadKwIdx) {
  // 65536 is the magic number for badKwIdx has not been assigned yet
  if (badKwIdx != 65536)
    return badKwIdx;
  return newBadKwIdx;
}
};

extern "C" {

void progvSaveSpecials(void **saveSpecialsP, core::T_sp *symbolsP, core::T_sp *valuesP) {
  _G();
  core::DynamicScopeManager *managerP = new core::DynamicScopeManager();
  (*saveSpecialsP) = (void *)managerP;
  core::List_sp symbols = (*symbolsP);
  core::List_sp values = (*valuesP);
  for (; symbols.notnilp(); symbols = oCdr(symbols), values = oCdr(values)) {
    core::Symbol_sp symbol = gc::As<Symbol_sp>(oCar(symbols));
    core::T_sp value = oCar(values);
    managerP->pushSpecialVariableAndSet(symbol, value);
  }
}

void progvRestoreSpecials(void **saveSpecialsP) {
  _G();
  core::DynamicScopeManager *managerP = (core::DynamicScopeManager *)(*saveSpecialsP);
  delete (managerP);
}
};

extern "C" {

void pushDynamicBinding(core::Symbol_sp *symbolP) {
  core::Symbol_sp sym = *symbolP;
  _lisp->bindings().push(sym);
  //	printf("%s:%d - pushDynamicBinding symbol: %s  value: %s\n", __FILE__, __LINE__, sym->__repr__().c_str(), sym->symbolValueOrUnbound()->__repr__().c_str() );
}

void popDynamicBinding(core::Symbol_sp *symbolP) {
  core::Symbol_sp sym = *symbolP;
  core::Symbol_sp top = _lisp->bindings().topSymbol();
  if (sym != _lisp->bindings().topSymbol()) {
    stringstream ss;
    ss << __FILE__ << ":" << __LINE__;
    ss << " popDynamicBinding of " << _rep_(*symbolP) << std::endl;
    ss << "  mismatch with top of dynamic binding stack: " << _rep_(top) << std::endl;
    ss << "  dumping stack: " << std::endl;
    core::core_dynamicBindingStackDump(ss);
    SIMPLE_ERROR(BF("Mismatch in popDynamicBinding:\n%s") % ss.str());
  }
  _lisp->bindings().pop();
  //	printf("%s:%d - popDynamicBinding symbol: %s  restored value: %s\n", __FILE__, __LINE__, sym->__repr__().c_str(), sym->symbolValueOrUnbound()->__repr__().c_str() );
}
};

extern "C" {

void trace_setLineNumberColumnForIHSTop(char *sourceFileName, int *sourceFileInfoHandleP, size_t fileOffset, int ln, int col) {
  if (comp::_sym_STARlowLevelTracePrintSTAR->symbolValue().isTrue()) {
    if (*sourceFileInfoHandleP == 0) {
      printf("%s:%d trace_setLineNumberColumnForIHSTop has *sourceFileInfoHandleP@%p == 0 soureFileName: %s\n", __FILE__, __LINE__, sourceFileInfoHandleP, sourceFileName);
    }
  }
  _lisp->invocationHistoryStack().setSourcePosForTop(*sourceFileInfoHandleP, fileOffset, ln, col);
}

void trace_setActivationFrameForIHSTop(core::T_sp *afP) {
  _G();
  _lisp->invocationHistoryStack().setActivationFrameForTop(*afP);
}

extern int matchKeywordOnce(core::T_sp *xP, core::T_O **yP, unsigned char *sawKeyAlreadyP) {
  if ((*xP).raw_() != (*yP))
    return 0;
  if (*sawKeyAlreadyP)
    return 2;
  return 1;
}
};

// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
//
//  Intrinsics for Cleavir

extern "C" {

//#define DEBUG_CC

void cc_setTmvToNil(core::T_mv *sharedP) {
  *sharedP = Values(_Nil<core::T_O>());
}


core::T_O *cc_makeCell() {
  _G();
  core::Cons_sp res = core::Cons_O::create();
#ifdef DEBUG_CC
  printf("%s:%d makeCell res.px[%p]\n", __FILE__, __LINE__, res.px);
#endif
  return res.raw_();
}

void cc_writeCell(core::T_O *cell, core::T_O *val) {
  //	core::Cons_sp c = gctools::smart_ptr<core::Cons_O>(reinterpret_cast<core::Cons_O*>(cell));
  core::Cons_sp c = gctools::smart_ptr<core::Cons_O>((gc::Tagged)cell);
#ifdef DEBUG_CC
  printf("%s:%d writeCell cell[%p]  val[%p]\n", __FILE__, __LINE__, cell, val);
#endif
  c->setCar(gctools::smart_ptr<core::T_O>((gc::Tagged)val));
}

core::T_O *cc_readCell(core::T_O *cell) {
  core::Cons_sp c = gctools::smart_ptr<core::Cons_O>((gc::Tagged)cell);
  core::T_sp val = c->ocar();
#ifdef DEBUG_CC
  printf("%s:%d readCell cell[%p] --> value[%p]\n", __FILE__, __LINE__, cell, val.px);
#endif
  return val.raw_();
}

core::T_O *cc_fetch(core::T_O *array, std::size_t idx) {
  //	core::ValueFrame_sp a = gctools::smart_ptr<core::ValueFrame_O>(reinterpret_cast<core::ValueFrame_O*>(array));
  core::ValueFrame_sp a = gctools::smart_ptr<core::ValueFrame_O>((gc::Tagged)array);
#ifdef DEBUG_CC
  printf("%s:%d fetch array@%p idx[%zu] -->cell[%p]\n", __FILE__, __LINE__, array, idx, (*a)[idx].raw_());
#endif
  ASSERT(a.notnilp());
  return (*a)[idx].raw_();
}

T_O *cc_fdefinition(core::T_O *sym) {
  //	core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>(reinterpret_cast<core::Symbol_O*>(sym));
  core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>((gc::Tagged)sym);
  if (!s->fboundp())
    SIMPLE_ERROR(BF("Could not find function %s") % _rep_(s));
  core::Function_sp fn = core::af_symbolFunction(s);
  return fn.raw_();// &(*fn);
}

T_O *cc_getSetfFdefinition(core::T_O *sym) {
  //	core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>(reinterpret_cast<core::Symbol_O*>(sym));
  core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>((gc::Tagged)sym);
  if (!s->setf_fboundp())
    SIMPLE_ERROR(BF("Could not find function %s") % _rep_(s));
  core::Function_sp fn = s->getSetfFdefinition(); //_lisp->get_setfDefinition(s);
  return fn.raw_(); //&(*fn);
}

T_O *cc_symbolValue(core::T_O *sym) {
  //	core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>(reinterpret_cast<core::Symbol_O*>(sym));
  core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>((gc::Tagged)sym);
  core::T_sp v = core::af_symbolValue(s);
  return v.raw_(); // &(*v);
}

#define PROTO_cc_setSymbolValue "void (t* t*)"
#define CATCH_cc_setSymbolValue false
void cc_setSymbolValue(core::T_O *sym, core::T_O *val) {
  //	core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>(reinterpret_cast<core::Symbol_O*>(sym));
  core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>((gc::Tagged)sym);
  s->setf_symbolValue(gctools::smart_ptr<core::T_O>((gc::Tagged)val));
}

void cc_call(core::T_mv *result, core::T_O *tfunc, LCC_ARGS_BASE) {
  //	core::Function_O* func = gctools::DynamicCast<core::Function_O*,core::T_O*>::castOrNULL(tfunc);
  core::Function_O *tagged_func = gc::TaggedCast<core::Function_O *, core::T_O *>::castOrNULL(tfunc);
  ASSERT(tagged_func != NULL);
  auto closure = gc::untag_general<core::Function_O *>(tagged_func)->closure.as<core::Closure>();
  closure->invoke(result, LCC_PASS_ARGS);
}

void cc_invoke(core::T_mv *result, core::T_O *tfunc, LCC_ARGS_BASE) {
  //	core::Function_O* func = gctools::DynamicCast<core::Function_O*,core::T_O*>::castOrNULL(tfunc);
  core::Function_O *tagged_func = gc::TaggedCast<core::Function_O *, core::T_O *>::castOrNULL(tfunc);
  ASSERT(tagged_func != NULL);
  auto closure = gc::untag_general<core::Function_O *>(tagged_func)->closure;
  closure->invoke(result, LCC_PASS_ARGS);
}


core::T_O *cc_enclose(core::T_O *lambdaName, fnLispCallingConvention llvm_func, std::size_t numCells, ...) {
  core::T_sp tlambdaName = gctools::smart_ptr<core::T_O>((gc::Tagged)lambdaName);
  core::ValueFrame_sp vo = core::ValueFrame_O::create(numCells, _Nil<core::T_O>());
  core::T_O *p;
  va_list argp;
  va_start(argp, numCells);
  int idx = 0;
#ifdef DEBUG_CC
  printf("%s:%d Starting enclose\n", __FILE__, __LINE__);
#endif
  for (; numCells; --numCells) {
    p = va_arg(argp, core::T_O *);
#ifdef DEBUG_CC
    printf("%s:%d enclose environment@%p[%d] p[%p]\n", __FILE__, __LINE__, vo.raw_(), idx, p);
#endif
    (*vo)[idx] = gctools::smart_ptr<core::T_O>((gc::Tagged)p);
    ++idx;
  }
  va_end(argp);
  //
  // Holy hell - I have to spoof stuff to allocate a CompiledClosure - I need to make all this info
  // available to the enclose instruction
  //
  llvmo::CompiledClosure *functoid = gctools::ClassAllocator<llvmo::CompiledClosure>::allocateClass(tlambdaName // functionName - make this something useful!
                                                                                                    ,
                                                                                                    _Nil<core::T_O>() // SourcePosInfo
                                                                                                    ,
                                                                                                    kw::_sym_function // fn-type
                                                                                                    ,
                                                                                                    llvm_func //(llvmo::CompiledClosure::fptr_type)NULL   // ptr - will be set when Module is compiled
                                                                                                    ,
                                                                                                    _Nil<core::T_O>() // llvm-func
                                                                                                    ,
                                                                                                    vo // renv
                                                                                                    ,
                                                                                                    _Nil<T_O>() // assocFuncs
                                                                                                    ,
                                                                                                    _Nil<T_O>() // lambdaList
                                                                                                    );
  core::CompiledFunction_sp cf = core::CompiledFunction_O::make(functoid);
  core::T_sp res = cf;
  return res.raw_();
}

/*! Take the multiple-value inputs from the thread local MultipleValues and call tfunc with them.
     This function looks exactly like the cc_invoke_multipleValueOneFormCall intrinsic but
    in cmpintrinsics.lsp it is set not to require a landing pad */
//    void cc_call_multipleValueOneFormCall(core::T_mv* result, core::T_O* tfunc )
void cc_call_multipleValueOneFormCall(core::T_mv *result, core::T_O *tfunc) {
  core::MultipleValues &mvThreadLocal = core::lisp_multipleValues();
  core::Function_O *tagged_func = gctools::TaggedCast<core::Function_O *, core::T_O *>::castOrNULL(tfunc);
  ASSERT(tagged_func != NULL);
  auto closure = gc::untag_general<core::Function_O *>(tagged_func)->closure;
  closure->invoke(result, result->number_of_values(), result->raw_(), mvThreadLocal[1], mvThreadLocal[2], mvThreadLocal[3], mvThreadLocal[4]);
}

/*! Take the multiple-value inputs from the thread local MultipleValues and invoke tfunc with them
	  This function looks exactly like the cc_call_multipleValueOneFormCall intrinsic but
	  in cmpintrinsics.lsp it is set to require a landing pad */
void cc_invoke_multipleValueOneFormCall(core::T_mv *result, core::T_O *tfunc) {
  core::MultipleValues &mvThreadLocal = core::lisp_multipleValues();
  core::Function_O *tagged_func = gctools::TaggedCast<core::Function_O *, core::T_O *>::castOrNULL(tfunc);
  ASSERT(tagged_func != NULL);
  auto closure = gc::untag_general<core::Function_O *>(tagged_func)->closure;
  closure->invoke(result, result->number_of_values(), result->raw_(), mvThreadLocal[1], mvThreadLocal[2], mvThreadLocal[3], mvThreadLocal[4]);
}

void cc_saveThreadLocalMultipleValues(core::T_mv *result, core::MultipleValues *mv) {
  core::MultipleValues &mvThread = core::lisp_multipleValues();
  (*mv)._Size = result->number_of_values();
  (*mv)[0] = (*result).raw_();
  for (size_t i = 1; i < (*mv)._Size; ++i) {
    (*mv)[i] = mvThread[i];
  }
}

void cc_loadThreadLocalMultipleValues(core::T_mv *result, core::MultipleValues *mv) {
  core::MultipleValues &mvThread = core::lisp_multipleValues();
  *result = gctools::multiple_values<core::T_O>(gctools::smart_ptr<core::T_O>((gc::Tagged)(*mv)[0]), (*mv)._Size);
  for (size_t i = 1; i < (*mv)._Size; ++i) {
    mvThread[i] = (*mv)[i];
  }
}


__attribute__((visibility("default"))) core::T_O *cc_gatherRestArguments(std::size_t nargs, core::T_O **argArray, std::size_t startRest, char *fnName) {
  _G();
  core::List_sp result = _Nil<core::T_O>();
  int inargs = nargs;
  int istartRest = startRest;
  for (int i = inargs - 1; i >= istartRest; i--) {
    result = core::Cons_O::create(core::T_sp((gc::Tagged)argArray[i]), result);
  }
  return (core::T_O *)result.raw_(); //(&(*result));
}

std::size_t cc_allowOtherKeywords(std::size_t saw_aok, std::size_t nargs, core::T_O **argArray, std::size_t argIdx) {
  if (saw_aok)
    return saw_aok;
  bool aokTrue = !gctools::tagged_nilp(argArray[argIdx + 1]);
  return aokTrue ? 2 : 1;
}

void cc_ifBadKeywordArgumentException(size_t allowOtherKeys, std::size_t badKwIdx, std::size_t nargs, core::T_O **argArray) {
  if (allowOtherKeys == 2)
    return;
  if (badKwIdx != 65536) {
    SIMPLE_ERROR(BF("Bad keyword argument %s") % _rep_(core::T_sp(argArray[badKwIdx])));
  }
}

size_t cc_matchKeywordOnce(core::T_O *xP, core::T_O *yP, core::T_O *sawKeyAlreadyP) {
  if (xP != yP)
    return 0;
  if (!gctools::tagged_nilp(sawKeyAlreadyP))
    return 2;
  return 1;
}

void cc_ifNotKeywordException(core::T_O *obj) {
  T_sp vobj((gc::Tagged)obj);
  if (!af_keywordP(vobj) ) {
    SIMPLE_ERROR(BF("Not keyword %s") % _rep_(vobj));
  }
}

T_O **cc_multipleValuesArrayAddress() {
  return &lisp_multipleValues().callingArgsStart()[0];
}

void cc_unwind(T_O *targetFrame, size_t index) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In cc_unwind targetFrame: %ld  index: %lu\n", __FILE__, __LINE__, gc::untag_fixnum(targetFrame), index);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  core::Unwind unwind(targetFrame, index);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("- - - - in cc_unwind throwing unwind to reach targetFrame: %ld   unwind=@%p\n", gc::untag_fixnum(targetFrame), &unwind);
  }
#endif
  try {
    throw unwind;
  } catch (core::Unwind& uw) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("- - - -  unwind event = %p\n", &uw);
  }
#endif
    throw;
  }
}


void cc_funwind_protect(core::T_mv* resultP, T_O* protected_fn, T_O* cleanup_fn)
{
  try {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In funwind_protect try\n", __FILE__, __LINE__ );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
    core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(protected_fn);
    ASSERT(func!=NULL);
    auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
    closure->invoke(resultP,LCC_PASS_ARGS0());
  } catch (...) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In funwind_protect catch(...) just caught\n", __FILE__, __LINE__ );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
    // Save any return value that may be in the multiple value return array
    gctools::Vec0<T_sp> savemv;
    T_mv tresult;
    tresult.readFromMultipleValue0();
    tresult.saveToVec0(savemv);
    {
      core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(cleanup_fn);
      ASSERT(func!=NULL);
      auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
      closure->invoke(&tresult,LCC_PASS_ARGS0());
    }
    tresult.loadFromVec0(savemv);
    tresult.saveToMultipleValue0();
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In funwind_protect catch(...)    about to rethrow\n", __FILE__, __LINE__ );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
    throw;
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In funwind_protect  normal exit\n", __FILE__, __LINE__ );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  gctools::Vec0<T_sp> savemv;
  resultP->saveToVec0(savemv);
  {
    T_mv tresult;
    core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(cleanup_fn);
    ASSERT(func!=NULL);
    auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
    closure->invoke(&tresult,LCC_PASS_ARGS0());
  }
  resultP->loadFromVec0(savemv);
};



void cc_catch(core::T_mv* resultP, T_O* tag, T_O* thunk)
{
  int frame = _lisp->exceptionStack().push(CatchFrame, LCC_TO_SMART_PTR(tag));
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In cc_catch tag@%p thisFrame: %d\n", __FILE__, __LINE__, tag, frame);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  try {
    core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(thunk);
    ASSERT(func!=NULL);
    auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
    closure->invoke(resultP,LCC_PASS_ARGS0());
  } catch (CatchThrow &catchThrow) {
    if (catchThrow.getFrame() != frame) {
#ifdef DEBUG_FLOW_CONTROL
      if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
        printf("- - - - - Rethrowing CatchThrow targetFrame[%d] (thisFrame is: %d)\n", catchThrow.getFrame(), frame);
      }
#endif
      throw catchThrow;
    }
    *resultP = gctools::multiple_values<T_O>::createFromValues();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("- - - - - Matched CatchThrow (thisFrame is: %d)\n", frame);
    printf("- - - - - Unwinding to thisFrame: %d\n", frame );
  }
#endif
  _lisp->exceptionStack().unwind(frame);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d  After cc_catch unwind\n", __FILE__, __LINE__ );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
}


void cc_throw(T_O* tag, T_O* result_func)
{
  int frame = _lisp->exceptionStack().findKey(CatchFrame, LCC_TO_SMART_PTR(tag));
  if (frame < 0) {
    CONTROL_ERROR();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In cc_throw     throwing CatchThrow to reach targetFrame[%d]\n", __FILE__, __LINE__, frame);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  T_mv result;
  core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(result_func);
  ASSERT(func!=NULL);
  auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
  closure->invoke(&result,LCC_PASS_ARGS0());
  result.saveToMultipleValue0();
  throw CatchThrow(frame);
}






/*! Use "invoke" to invoke this version of "throw"
      I'm using this to figure out why when I JIT code with LLVM
      I have to "invoke" functions that throw exceptions rather than "call" them.
    */
#if 0
void cc_throw(T_O *tag) {
  _G();
  MAY_BE_DEPRECIATED();
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In cc_tag tag@%p\n\n", __FILE__, __LINE__, tag);
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  int frame = _lisp->exceptionStack().findKey(CatchFrame, LCC_TO_SMART_PTR(tag));
  if (frame < 0) {
    CONTROL_ERROR();
  }
  core::CatchThrow catchThrow(frame);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d in cc_throw throwing CatchThrow to reach frame: %d   core::CatchThrow typeinfo@%p\n", __FILE__, __LINE__, frame, &typeid(core::CatchThrow));
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  throw catchThrow;
}
#endif

void cc_saveMultipleValue0(core::T_mv *result) {
  _G();
  (*result).saveToMultipleValue0();
}

void cc_restoreMultipleValue0(core::T_mv *result) {
  _G();
  (*result).readFromMultipleValue0();
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d In cc_restoreMultipleValue0\n", __FILE__, __LINE__);
  }
#endif
}

T_O *cc_pushLandingPadFrame() {
  _G();
  core::T_sp ptr = _Nil<core::T_O>();
#define DEBUG_UNWIND 1
#ifdef DEBUG_UNWIND
  ptr = core::Pointer_O::create((void *)&typeid(core::Unwind));
#endif
  size_t index = _lisp->exceptionStack().push(LandingPadFrame, ptr);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d pushLandingPadFrame pushed frame: %lu  core::Unwind typeinfo@%p\n", __FILE__, __LINE__, index, (void*)&typeid(core::Unwind));
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  return gctools::tag_fixnum<core::T_O *>(index);
}

void cc_popLandingPadFrame(T_O *frameFixnum) {
  ASSERT(gctools::tagged_fixnump(frameFixnum));
  size_t frameIndex = gctools::untag_fixnum(frameFixnum);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d  popLandingPadFrame   About to unwind exceptionStack to frame: %lu\n", __FILE__, __LINE__, frameIndex );
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  _lisp->exceptionStack().unwind(frameIndex);
}

size_t cc_landingpadUnwindMatchFrameElseRethrow(char *exceptionP, core::T_O *thisFrame) {
  ASSERT(gctools::tagged_fixnump(thisFrame));
  core::Unwind *unwindP = reinterpret_cast<core::Unwind *>(exceptionP);
#ifdef DEBUG_FLOW_CONTROL
  size_t frameIndex = gctools::untag_fixnum(thisFrame);
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("%s:%d landingpadUnwindMatchFrameElseRethrow targetFrame: %lu thisFrame: %lu  unwindP=%p\n",
           __FILE__, __LINE__, gctools::untag_fixnum(unwindP->getFrame()), frameIndex, unwindP);
    if ( unwindP->getFrame() > thisFrame ) {
      printf("- - - - - THERE IS A SERIOUS PROBLEM - THE TARGET FRAME HAS BEEN BYPASSED\n");
    }
    printf("   %s\n", _lisp->exceptionStack().summary().c_str());
  }
#endif
  if (unwindP->getFrame() == thisFrame) {
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
      printf("- - - - - Matched Unwind to targetFrame: %lu  index: %lu\n", gc::untag_fixnum(unwindP->getFrame()), unwindP->index());
    }
#endif
    return unwindP->index();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp() ) {
    printf("- - - - - Rethrowing core::Unwind targetFrame[%lu] index[%zu] (thisFrame is: %lu)\n", gc::untag_fixnum(unwindP->getFrame()), unwindP->index(), frameIndex);
  }
#endif
  throw * unwindP;
}


#if 0 // Bring this back online when we convert to new calling convention
T_mv cc_multiple_value_funcall(core::T_mv* result, T_O* funcDesignator, std::size_t numFuns, ...) {
  MultipleValues mvAccumulate;
  mvAccumulate._Size = 0;
  size_t idx = 0;
  va_list argp;
  va_start(argp,numFuns);
  for (; numFuns; --numFuns) {
    core::T_O* tfunc = va_arg(argp,core::T_O*);
    core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(tfunc);
    ASSERT(func!=NULL);
    auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
    T_mv result;
    closure->invoke(&result,LCC_PASS_ARGS0());
    mvAccumulate[idx] = result.raw_();
    ++idx;
    for (size_t i = 1, iEnd(result.number_of_values()); i < iEnd; ++i) {
      mvAccumulate[idx] = result.valueGet(i).raw_();
      ++idx;
    }
  }
  va_end(argp);
  ASSERT(idx < MultipleValues::MultipleValuesLimit);
  mvAccumulate._Size = idx;
  core::Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(funcDesignator);
  ASSERT(func!=NULL);
  auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
  core::T_O** a = &mvAccumulate[0];
  MultipleValues &mvThreadLocal = lisp_multipleValues();
  for (size_t i = LCC_FIXED_ARGS, iEnd(mvAccumulate._Size); i < iEnd; ++i) {
    mvThreadLocal[i] = mvAccumulate[i];
  }
  closure->invoke(result, mvAccumulate._Size, mvAccumulate[0], mvAccumulate[1], mvAccumulate[2], mvAccumulate[3], mvAccumulate[4]);
}

T_mv cc_multiple_value_prog1_function(core::T_mv* result, core::T_O* tfunc1, core::T_O* tfunc2) {
  MultipleValues mvFunc1;
  core::Function_O* func1 = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(tfunc1);
  ASSERT(func1!=NULL);
  auto closure1 = gc::untag_general<core::Function_O *>(func1)->closure.as<core::Closure>();
  closure1->invoke(result,LCC_PASS_ARGS0());
  mvFunc1._Size = result->number_of_values();
  mvFunc1[0] = result->raw_();
  MultipleValues &mvThreadLocal = lisp_multipleValues();
  for (size_t i(1), iEnd(mvFunc1._Size); i < iEnd; ++i) mvFunc1[i] = mvThreadLocal[i];
  T_mv resultTemp;
  core::Function_O* func2 = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(tfunc2);
  ASSERT(func2!=NULL);
  auto closure2 = gc::untag_general<core::Function_O *>(func2)->closure.as<core::Closure>();
  closure2->invoke(&resultTemp,LCC_PASS_ARGS0());
  for (size_t i(1), iEnd(mvFunc1._Size); i < iEnd; ++i) mvThreadLocal[i] = mvFunc1[i];
}
#endif





void clasp_terminate(const char *file, int line, int column, const char *func) {
  printf("Terminating file: %s  func: %s\n", file, func);
  exit(1);
}
};


#if 0
core::T_mv ccmv_invoke(core::T_O *tfunc, LCC_ARGS_BASE) {
  //	core::Function_O* func = gctools::DynamicCast<core::Function_O*,core::T_O*>::castOrNULL(tfunc);
  core::Function_O *tagged_func = gc::TaggedCast<core::Function_O *, core::T_O *>::castOrNULL(tfunc);
  ASSERT(tagged_func != NULL);
  auto closure = gc::untag_general<core::Function_O *>(tagged_func)->closure;
  closure->invoke(result, LCC_PASS_ARGS);
}
#endif

#pragma GCC visibility pop

namespace llvmo {
// We must link one symbol to the executable or none of this file will be inserted

#define PRIMITIVE(name, prototype) primitive(##name, PROTOTYPE_ #name, CATCH_ #name)

void initialize_link_intrinsics() {
//	PRIMITIVE(cc_setSymbolValue);
//printf("%s:%d  Initializing intrinsics.cc\n", __FILE__, __LINE__ );
#if 0
	T_mv foo = testTwoReturns();
	printf("Called testTwoReturns  foo.raw_() = %p   foo.two = %d\n", foo.raw_(), foo.number_of_values() );
#endif
}
};
