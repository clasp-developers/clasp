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

ALWAYS_INLINE core::T_sp *loadTimeValueReference(core::LoadTimeValues_O **ltvPP, int index) {
  core::LoadTimeValues_O &ltv = **ltvPP;
  core::T_sp &result = ltv.data_element(index);
  return &result;
}

ALWAYS_INLINE core::Symbol_sp *loadTimeSymbolReference(core::LoadTimeValues_O **ltvPP, int index) {
  core::Symbol_sp &result = (*ltvPP)->symbols_element(index);
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d loadTimeSymbolReference@%p  index[%d]  result client@%p  value: %s\n", __FILE__, __LINE__, (*ltvPP), index, result.pbase(), _rep_(result).c_str());
#endif
  return &result;
}


ALWAYS_INLINE void newTsp(core::T_sp *sharedP) {
  ASSERT(sharedP != NULL);
  new (sharedP) core::T_sp();
}


ALWAYS_INLINE void newTmv(core::T_mv *sharedP) {
  new (sharedP) core::T_mv();
}


ALWAYS_INLINE extern int compareTspTptr(core::T_sp *xP, core::T_O **yP) {
  return ((*xP).raw_() == (*yP)) ? 1 : 0;
}

ALWAYS_INLINE extern void sp_copyTsp(core::T_sp *destP, core::T_sp *sourceP) {
  //	ASSERT(sourceP!=NULL);
  //	ASSERT(destP!=NULL);
  *destP = *sourceP;
}

ALWAYS_INLINE extern void mv_copyTsp(core::T_mv *destP, core::T_sp *sourceP) {
  ASSERT(sourceP != NULL);
  ASSERT(destP != NULL);
  *destP = Values(*sourceP);
}

ALWAYS_INLINE extern void sp_copyTspTptr(core::T_sp *destP, core::T_O **sourceP) {
  *destP = gc::smart_ptr<core::T_O>((gc::Tagged) * sourceP);
}

ALWAYS_INLINE extern void mv_copyTspTptr(core::T_mv *destP, core::T_O **sourceP) {
  ASSERT(sourceP != NULL);
  ASSERT(destP != NULL);
  *destP = Values(gc::smart_ptr<core::T_O>((gc::Tagged) * sourceP));
}

/*! This copies a T_mv from source to dest */
ALWAYS_INLINE void mv_copyTmvOrSlice(core::T_mv *destP, core::T_mv *sourceP) {
  //	printf("intrinsics.cc mv_copyTmvOrSlice copying %d values\n", (*sourceP).number_of_values());
  (*destP) = (*sourceP);
}

/*! This slices a T_mv in source down to a T_sp in dest */
ALWAYS_INLINE void sp_copyTmvOrSlice(core::T_sp *destP, core::T_mv *sourceP) {
  if ((*sourceP).number_of_values() == 0) {
    (*destP) = _Nil<T_O>();
  } else
    (*destP) = (*sourceP);
}

ALWAYS_INLINE void sp_makeNil(core::T_sp *result) {
  (*result) = _Nil<core::T_O>();
}

ALWAYS_INLINE void mv_makeNil(core::T_mv *result) {
  (*result) = Values(_Nil<core::T_O>());
}

ALWAYS_INLINE void makeT(core::T_sp *result) {
  (*result) = _lisp->_true();
}

ALWAYS_INLINE void makeCons(core::T_sp *resultConsP, core::T_sp *carP, core::T_sp *cdrP) {
  (*resultConsP) = core::Cons_O::create(*carP, *cdrP);
}


ALWAYS_INLINE void cc_call_with_variable_bound(core::T_mv* result, core::T_O* symbol, core::T_O* value, core::T_O* thunk) {
  T_sp tsymbol((gc::Tagged)symbol);
  T_sp tvalue((gc::Tagged)value);
  core::DynamicScopeManager scope(tsymbol,tvalue);
  Function_O* func = gc::TaggedCast<core::Function_O*,core::T_O*>::castOrNULL(thunk);
  ASSERT(func!=NULL);
  auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
  closure->invoke(result,LCC_PASS_ARGS0());
};

};

extern "C" {

ALWAYS_INLINE T_O *cc_precalcSymbol(core::LoadTimeValues_O **tarray, size_t idx) {
  LoadTimeValues_O *array = *tarray;
#ifdef DEBUG_CC
  printf("%s:%d precalcSymbol idx[%zu] symbol = %p\n", __FILE__, __LINE__, idx, (*array).symbols_element(idx).px);
#endif
  T_O *res = (*array).symbols_element(idx).raw_();
  ASSERT(res != NULL);
  return res;
}

ALWAYS_INLINE T_O *cc_precalcValue(core::LoadTimeValues_O **tarray, size_t idx) {
  LoadTimeValues_O *array = *tarray;
#ifdef DEBUG_CC
  printf("%s:%d precalcValue idx[%zu] value = %p\n", __FILE__, __LINE__, idx, (*array).data_element(idx).px);
#endif
  T_O *res = (*array).data_element(idx).raw_();
  return res;
}

ALWAYS_INLINE core::T_O **cc_loadTimeValueReference(core::LoadTimeValues_O **ltvPP, size_t index) {
  ASSERT(ltvPP != NULL);
  ASSERT(*ltvPP != NULL);
  core::LoadTimeValues_O &ltv = **ltvPP;
  core::T_sp &result = ltv.data_element(index);
  return &result.rawRef_();
}

ALWAYS_INLINE void cc_copy_va_list(size_t nargs, T_O**mvPtr, va_list va_args )
{
  for ( int i = LCC_FIXED_ARGS; i<nargs; ++i ) {
    mvPtr[i] = va_arg(va_args, core::T_O *);
  }
  va_end(va_args);
}

};


namespace llvmo {
void initialize_intrinsics() {
  // Do nothing
}

};
#pragma GCC visibility pop
