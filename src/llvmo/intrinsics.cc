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

#include <cctype>
#include <cstdint>
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
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/intrinsics.h>

#define DEBUG_FLOW_CONTROL 1

using namespace core;

#pragma GCC visibility push(default)

extern "C" {

ALWAYS_INLINE core::T_sp *symbolValueReference(core::Symbol_sp *symbolP) {
  return ((*symbolP)->valueReference());
}

ALWAYS_INLINE core::T_sp *lexicalValueReference(int depth, int index, core::ActivationFrame_sp *frameP) {
  core::ActivationFrame_sp af = gctools::reinterpret_cast_smart_ptr<core::ActivationFrame_O>(*frameP);
  return const_cast<core::T_sp *>(&core::value_frame_lookup_reference(af, depth, index));
}

ALWAYS_INLINE void sp_lexicalValueRead(core::T_sp *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = core::value_frame_lookup_reference(*renvP,depth,index);
}
ALWAYS_INLINE void mv_lexicalValueRead(core::T_mv *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = core::value_frame_lookup_reference(*renvP,depth,index);
}


ALWAYS_INLINE void makeFunctionFrame(core::ActivationFrame_sp *resultP, int numargs, core::ActivationFrame_sp *parentP)
// was ActivationFrame_sp
{
  ASSERT(resultP != NULL);
  ASSERT(parentP != NULL);
  (*resultP) = core::FunctionFrame_sp(core::FunctionFrame_O::create(numargs, (*parentP)));
  ASSERTNOTNULL(*resultP);
}

ALWAYS_INLINE core::T_sp *functionFrameReference(core::ActivationFrame_sp *frameP, int idx) {
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  core::FunctionFrame_sp frame = gctools::reinterpret_cast_smart_ptr<core::FunctionFrame_O>((*frameP));
#ifdef DEBUG_ASSERTS
  if (idx < 0 || idx >= frame->length()) {
    intrinsic_error(llvmo::invalidIndexForFunctionFrame, clasp_make_fixnum(idx), clasp_make_fixnum(frame->length()));
  }
#endif
  core::T_sp *pos_gc_safe = const_cast<core::T_sp *>(&frame->entryReference(idx));
  return pos_gc_safe;
}





ALWAYS_INLINE void sp_lexicalFunctionRead(core::T_sp *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = core::function_frame_lookup(*renvP,depth,index);
}

ALWAYS_INLINE void mv_lexicalFunctionRead(core::T_mv *resultP, int depth, int index, core::ActivationFrame_sp *renvP) {
  (*resultP) = core::function_frame_lookup(*renvP,depth,index);
}


ALWAYS_INLINE core::T_sp *loadTimeValueReference(core::LoadTimeValues_O **ltvPP, int index) {
  core::LoadTimeValues_O *tagged_ltvP = *ltvPP;
  core::LoadTimeValues_O *ltvP = gctools::untag_general<core::LoadTimeValues_O *>(tagged_ltvP);
  core::T_sp &result = ltvP->data_element(index);
  return &result;
}

ALWAYS_INLINE core::Symbol_sp *loadTimeSymbolReference(core::LoadTimeValues_O **ltvPP, int index) {
  core::LoadTimeValues_O *tagged_ltvP = *ltvPP;
  core::LoadTimeValues_O *ltvP = gctools::untag_general<core::LoadTimeValues_O *>(tagged_ltvP);
  core::Symbol_sp &result = ltvP->symbols_element(index);
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

ALWAYS_INLINE extern int compareTspTptr(core::T_sp *xP, core::T_O *yP) {
  return ((*xP).raw_() == (yP)) ? 1 : 0;
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

ALWAYS_INLINE extern void sp_copyTspTptr(core::T_sp *destP, core::T_O *source) {
  *destP = gc::smart_ptr<core::T_O>((gc::Tagged)source);
}

ALWAYS_INLINE extern void mv_copyTspTptr(core::T_mv *destP, core::T_O *source) {
  ASSERT(destP != NULL);
  *destP = Values(gc::smart_ptr<core::T_O>((gc::Tagged)source));
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

ALWAYS_INLINE void sp_symbolValueRead(core::T_sp *resultP, const core::Symbol_sp *symP) {
  T_sp sv = (*symP)->_Value;
  if (sv.unboundp()) {
    SIMPLE_ERROR(BF("Unbound symbol-value for %s") % (*symP)->_Name->c_str());
  }
  *resultP = sv;
}
ALWAYS_INLINE void mv_symbolValueRead(core::T_mv *resultP, const core::Symbol_sp *symP) {
  T_sp sv = (*symP)->_Value;
  if (sv.unboundp()) {
    SIMPLE_ERROR(BF("Unbound symbol-value for %s") % (*symP)->_Name->c_str());
  }
  *resultP = sv;
}

ALWAYS_INLINE T_O *va_symbolFunction(core::Symbol_sp *symP) {
  if (!(*symP)->fboundp())
    intrinsic_error(llvmo::noFunctionBoundToSymbol, *symP);
  core::Function_sp func((gc::Tagged)(*symP)->_Function.theObject);
  return func.raw_();
}
};

extern "C" {

ALWAYS_INLINE T_O *cc_precalcSymbol(core::LoadTimeValues_O **tarray, size_t idx) {
  core::LoadTimeValues_O *tagged_ltvP = *tarray;
  core::LoadTimeValues_O *array = gctools::untag_general<core::LoadTimeValues_O *>(tagged_ltvP);
#ifdef DEBUG_CC
  printf("%s:%d precalcSymbol idx[%zu] symbol = %p\n", __FILE__, __LINE__, idx, (*array).symbols_element(idx).px);
#endif
  T_O *res = array->symbols_element(idx).raw_();
  ASSERT(res != NULL);
  return res;
}

ALWAYS_INLINE T_O *cc_precalcValue(core::LoadTimeValues_O **tarray, size_t idx) {
  core::LoadTimeValues_O *tagged_ltvP = *tarray;
  core::LoadTimeValues_O *array = gctools::untag_general<core::LoadTimeValues_O *>(tagged_ltvP);
#ifdef DEBUG_CC
  printf("%s:%d precalcValue idx[%zu] value = %p\n", __FILE__, __LINE__, idx, (*array).data_element(idx).px);
#endif
  T_O *res = array->data_element(idx).raw_();
  return res;
}

ALWAYS_INLINE core::T_O **cc_loadTimeValueReference(core::LoadTimeValues_O **ltvPP, size_t index) {
  ASSERT(ltvPP != NULL);
  ASSERT(*ltvPP != NULL);
  core::LoadTimeValues_O *tagged_ltvP = *ltvPP;
  core::LoadTimeValues_O *ltvP = gctools::untag_general<core::LoadTimeValues_O *>(tagged_ltvP);
  core::T_sp &result = ltvP->data_element(index);
  return &result.rawRef_();
}

ALWAYS_INLINE core::T_O *cc_va_arg(VaList_S *valist) {
  VaList_S *vl = reinterpret_cast<VaList_S *>(gc::untag_valist((void *)valist));
  return va_arg(vl->_Args, core::T_O *);
}

ALWAYS_INLINE void cc_copy_va_list(size_t nargs, T_O **mvPtr, VaList_S *va_args) {
  VaList_S *vl = reinterpret_cast<VaList_S *>(gc::untag_valist((void *)va_args));
  for (int i = LCC_FIXED_ARGS; i < nargs; ++i) {
    mvPtr[i] = va_arg(vl->_Args, core::T_O *);
  }
  va_end(vl->_Args);
}

ALWAYS_INLINE T_O *cc_unsafe_symbol_value(core::T_O *sym) {
  core::Symbol_O *symP = reinterpret_cast<core::Symbol_O *>(gctools::untag_general<core::T_O *>(sym));
  return symP->symbolValueRef().raw_();
}

ALWAYS_INLINE T_O *cc_safe_symbol_value(core::T_O *sym) {
  core::Symbol_O *symP = reinterpret_cast<core::Symbol_O *>(gctools::untag_general<core::T_O *>(sym));
  T_O *sv = symP->symbolValueRef().raw_();
  if (sv == gctools::global_tagged_Symbol_OP_unbound) {
    intrinsic_error(llvmo::unboundSymbolValue, gc::smart_ptr<core::Symbol_O>((gc::Tagged)sym));
  }
  return sv;
}

ALWAYS_INLINE T_O *cc_unsafe_fdefinition(core::T_O *sym) {
  core::Symbol_O *symP = reinterpret_cast<core::Symbol_O *>(gctools::untag_general<core::T_O *>(sym));
  return symP->_Function.raw_();
}

ALWAYS_INLINE T_O *cc_safe_fdefinition(core::T_O *sym) {
  core::Symbol_O *symP = reinterpret_cast<core::Symbol_O *>(gctools::untag_general<core::T_O *>(sym));
  T_O *sv = symP->_Function.raw_();
  if (sv == gctools::global_tagged_Symbol_OP_unbound) {
    intrinsic_error(llvmo::unboundSymbolFunction, gc::smart_ptr<core::Symbol_O>((gc::Tagged)sym));
  }
  return symP->_Function.raw_();
}

ALWAYS_INLINE T_O *cc_unsafe_setfdefinition(core::T_O *sym) {
  core::Symbol_O *symP = reinterpret_cast<core::Symbol_O *>(gctools::untag_general<core::T_O *>(sym));
  return symP->_SetfFunction.raw_();
}

ALWAYS_INLINE T_O *cc_safe_setfdefinition(core::T_O *sym) {
  core::Symbol_O *symP = reinterpret_cast<core::Symbol_O *>(gctools::untag_general<core::T_O *>(sym));
  T_O *sv = symP->_SetfFunction.raw_();
  if (sv == gctools::global_tagged_Symbol_OP_unbound) {
    intrinsic_error(llvmo::unboundSymbolSetfFunction, gc::smart_ptr<core::Symbol_O>((gc::Tagged)sym));
  }
  return symP->_SetfFunction.raw_();
}

ALWAYS_INLINE gc::return_type cc_call(LCC_ARGS_CC_CALL_ELLIPSIS) {
  //	core::Function_O* func = gctools::DynamicCast<core::NamedFunction_O*,core::T_O*>::castOrNULL(tfunc);
  core::Closure_O *tagged_closure = reinterpret_cast<core::Closure_O *>(lcc_closure);
  core::Closure_O* closure = gc::untag_general<core::Closure_O *>(tagged_closure);
  VaList_S lcc_arglist_s;
  va_start(lcc_arglist_s._Args, LCC_VA_START_ARG);
  LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_arglist_s);
  core::T_O *lcc_arglist = lcc_arglist_s.asTaggedPtr();
  return closure->invoke_va_list(LCC_PASS_ARGS);
}

ALWAYS_INLINE void makeValueFrame(core::T_sp *resultActivationFrameP, size_t numargs)
{
  core::ValueFrame_sp valueFrame(core::ValueFrame_O::create(numargs, _Nil<core::T_O>()));
//  valueFrame->setEnvironmentId(id);   // I don't use id anymore
  (*resultActivationFrameP) = valueFrame;
}

ALWAYS_INLINE void makeTagbodyFrame(core::ActivationFrame_sp *resultP)
{
  core::TagbodyFrame_sp tagbodyFrame(core::TagbodyFrame_O::create(_Nil<core::T_O>()));
  (*resultP) = tagbodyFrame;
}

ALWAYS_INLINE core::T_sp *valueFrameReference(core::ActivationFrame_sp *frameP, int idx) {
  ASSERT(frameP != NULL);
  ASSERT((*frameP));
  ASSERTF(idx >= 0 && idx < ((*frameP)->length()), BF("Illegal value of idx[%d] must be in range [0<=idx<%d]") % idx % (*frameP)->length());
  core::ValueFrame_sp frame = gctools::As_unsafe<core::ValueFrame_sp>(*frameP);
  core::T_sp *pos_gc_safe = const_cast<core::T_sp *>(&frame->entryReference(idx));
  return pos_gc_safe;
}

ALWAYS_INLINE core::T_O *cc_gatherVaRestArguments(std::size_t nargs, VaList_S *tagged_vargs, std::size_t startRest, VaList_S* untagged_vargs_rest) {
  ASSERT(nargs >= startRest);
  VaList_S* untagged_vargs = reinterpret_cast<VaList_S*>(gc::untag_valist(tagged_vargs));
  untagged_vargs_rest->set_from_other_VaList_S(untagged_vargs);
  T_O* result = untagged_vargs_rest->asTaggedPtr();
  return result;
//  T_sp varest((gctools::Tagged)vargs);
//  printf("%s:%d in gatherVaRestArguments --> %s\n", __FILE__, __LINE__, _rep_(vargs).c_str());
  //VaList_S *args = reinterpret_cast<VaList_S *>(gc::untag_valist((void *)vargs));
  //return args->asTaggedPtr();
}

ALWAYS_INLINE core::T_O *cc_makeCell() {
  core::Cons_sp res = core::Cons_O::create(_Nil<core::T_O>(),_Nil<core::T_O>());
#ifdef DEBUG_CC
  printf("%s:%d makeCell res.px[%p]\n", __FILE__, __LINE__, res.px);
#endif
  return res.raw_();
}

ALWAYS_INLINE void cc_writeCell(core::T_O *cell, core::T_O *val) {
  //	core::Cons_sp c = gctools::smart_ptr<core::Cons_O>(reinterpret_cast<core::Cons_O*>(cell));
  ASSERT(gctools::tagged_consp(cell));
  core::Cons_O* cp = reinterpret_cast<core::Cons_O*>(gctools::untag_cons(cell));
//  core::Cons_sp c = gctools::smart_ptr<core::Cons_O>((gc::Tagged)cell);
#ifdef DEBUG_CC
  printf("%s:%d writeCell cell[%p]  val[%p]\n", __FILE__, __LINE__, cell, val);
#endif
  cp->setCar(gctools::smart_ptr<core::T_O>((gc::Tagged)val));
}

ALWAYS_INLINE core::T_O *cc_readCell(core::T_O *cell) {
  core::Cons_O* cp = reinterpret_cast<core::Cons_O*>(gctools::untag_cons(cell));
  core::T_sp val = cp->ocar();
#ifdef DEBUG_CC
  printf("%s:%d readCell cell[%p] --> value[%p]\n", __FILE__, __LINE__, cell, val.px);
#endif
  return val.raw_();
}


core::T_O *cc_fetch(core::T_O *tagged_closure, std::size_t idx) {
  //	core::ValueFrame_sp a = gctools::smart_ptr<core::ValueFrame_O>(reinterpret_cast<core::ValueFrame_O*>(array));
  gctools::smart_ptr<core::ClosureWithSlots_O> c = gctools::smart_ptr<core::ClosureWithSlots_O>((gc::Tagged)tagged_closure);
#ifdef DEBUG_CC
  printf("%s:%d fetch array@%p idx[%zu] -->cell[%p]\n", __FILE__, __LINE__, array, idx, (*c)[idx].raw_());
#endif
  ASSERT(c.notnilp());
  return (*c)[idx].raw_();
}


ALWAYS_INLINE char *cc_getPointer(core::T_O *pointer_object) {
  core::Pointer_O* po = reinterpret_cast<core::Pointer_O*>(gctools::untag_general(pointer_object));
  char* ptr = reinterpret_cast<char*>(po->ptr());
  return ptr;
}

};

extern "C" {

ALWAYS_INLINE void setParentOfActivationFrameFromClosure(core::T_sp *resultP, core::T_O *closureRaw) {
  Closure_sp closure = Closure_sp((gctools::Tagged)closureRaw);
  T_sp activationFrame = closure->closedEnvironment();
  core::T_O* parentP =  activationFrame.raw_();
  ASSERT((*resultP).isA<ActivationFrame_O>());
  ActivationFrame_sp af = gc::reinterpret_cast_smart_ptr<ActivationFrame_O, T_O>((*resultP));
  af->setParentFrame(parentP);
}

ALWAYS_INLINE void setParentOfActivationFrame(core::T_sp *resultP, core::T_sp *parentsp) {
  T_O *parentP = parentsp->raw_();
  ASSERT((*resultP).isA<ActivationFrame_O>());
  ActivationFrame_sp af = gc::reinterpret_cast_smart_ptr<ActivationFrame_O, T_O>((*resultP));
  af->setParentFrame(parentP);
  return;
}


ALWAYS_INLINE core::T_O *cc_stack_enclose(void* closure_address,
                            core::T_O *lambdaName, fnLispCallingConvention llvm_func,
                            int *sourceFileInfoHandleP,
                            size_t filePos, size_t lineno, size_t column,
                            std::size_t numCells, ...) {
  core::T_sp tlambdaName = gctools::smart_ptr<core::T_O>((gc::Tagged)lambdaName);
  gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(closure_address);
  const gctools::GCKindEnum closure_kind = gctools::GCKind<core::ClosureWithSlots_O>::Kind;
  size_t size = gctools::sizeof_container_with_header<core::ClosureWithSlots_O>(numCells);

//  gctools::global_stack_closure_bytes_allocated += size;

#ifdef DEBUG_GUARD
  new (header) gctools::GCHeader<core::ClosureWithSlots_O>::HeaderType(closure_kind,size,0,size);
#else
  new (header) gctools::GCHeader<core::ClosureWithSlots_O>::HeaderType(closure_kind);
#endif
  auto obj = gctools::BasePtrToMostDerivedPtr<typename gctools::smart_ptr<core::ClosureWithSlots_O>::Type>(closure_address);
  new (obj) (typename gctools::smart_ptr<core::ClosureWithSlots_O>::Type)(numCells,
                                                                          tlambdaName,
                                                                          kw::_sym_function,
                                                                          llvm_func,
                                                                          _Nil<T_O>(),
                                                                          _Nil<T_O>(),
                                                                          _Nil<T_O>(),
                                                                          *sourceFileInfoHandleP, filePos, lineno, column);

  gctools::smart_ptr<core::ClosureWithSlots_O> functoid = gctools::smart_ptr<core::ClosureWithSlots_O>(obj);
  core::T_O *p;
  va_list argp;
  va_start(argp, numCells);
  int idx = 0;
  for (; numCells; --numCells) {
    p = va_arg(argp, core::T_O *);
    (*functoid)[idx] = gctools::smart_ptr<core::T_O>((gc::Tagged)p);
    ++idx;
  }
  va_end(argp);
//  printf("%s:%d  Allocating closure on stack at %p\n", __FILE__, __LINE__, functoid.raw_());
  return functoid.raw_();
}

}; // extern "C"

namespace llvmo {

// === CORE TRANSLATORS FROM OBJECT / TO OBJECT ===

template< typename T >
ALWAYS_INLINE core::T_O* from_object_raw(core::T_O* obj)
{
  core::T_sp ptr =  gctools::smart_ptr< core::T_O>( (gctools::Tagged) obj );

  T x = translate::from_object< T >( ptr )._v;
  core::T_O *result = reinterpret_cast< core::T_O * >( x );
  return result;
}

template< typename T >
ALWAYS_INLINE core::T_O* to_object_raw(core::T_O* obj)
{
  T x = static_cast< T >(reinterpret_cast<cl_intptr_t>( (gctools::Tagged) obj));
  return translate::to_object< T >::convert(x).raw_();
}

}; // namespace llvmo

extern "C" {

#define DEF_RAW_TRANSLATOR(TYPE,NAME) \
static const int source_line_from_object_##NAME = __LINE__; \
ALWAYS_INLINE core::T_O* from_object_##NAME(core::T_O* obj) \
{ \
  return llvmo::from_object_raw< TYPE >( obj ); \
} \
\
static const int source_line_to_object_##NAME = __LINE__; \
ALWAYS_INLINE core::T_O* to_object_##NAME(core::T_O* obj) \
{ \
  return llvmo::to_object_raw< TYPE >( obj ); \
}

DEF_RAW_TRANSLATOR(short,short);
DEF_RAW_TRANSLATOR(unsigned short,ushort);
DEF_RAW_TRANSLATOR(int,int);
DEF_RAW_TRANSLATOR(unsigned int,uint);
DEF_RAW_TRANSLATOR(long,long);
DEF_RAW_TRANSLATOR(unsigned long,ulong);
DEF_RAW_TRANSLATOR(long long,longlong);
DEF_RAW_TRANSLATOR(unsigned long long,ulonglong);
DEF_RAW_TRANSLATOR(int8_t,int8);
DEF_RAW_TRANSLATOR(uint8_t,uint8);
DEF_RAW_TRANSLATOR(int16_t,int16);
DEF_RAW_TRANSLATOR(uint16_t,uint16);
DEF_RAW_TRANSLATOR(int32_t,int32);
DEF_RAW_TRANSLATOR(uint32_t,uint32);
DEF_RAW_TRANSLATOR(int64_t,int64);
DEF_RAW_TRANSLATOR(uint64_t,uint64);
DEF_RAW_TRANSLATOR(size_t,size);
DEF_RAW_TRANSLATOR(ssize_t,ssize);
DEF_RAW_TRANSLATOR(ptrdiff_t,ptrdiff);
DEF_RAW_TRANSLATOR(time_t,time);
DEF_RAW_TRANSLATOR(char,char);

// Special Case: Pointers
// DEF_RAW_TRANSLATOR(void *,pointer);
// drmeister says:
// 17:06 <drmeister> I think it's best to static cast from unsigned long to
//                   uintptr_t and then reinterpret_cast to void*
// 17:06 <drmeister> reinterpret_cast<void*>(static_cast<uintptr_t>(unsigned
//                   long))
// 17:07 <drmeister> Good 'ole C++, it's trying to save you from yourself.

static const int source_line_from_object_pointer = __LINE__;
ALWAYS_INLINE core::T_O* from_object_pointer(core::T_O* obj)
{
  core::T_sp ptr =  gctools::smart_ptr< core::T_O>( (gctools::Tagged) obj );

  void * x = translate::from_object< void * >( ptr )._v;

  core::T_O *result = reinterpret_cast< core::T_O * >( x );
  return result;
}

static const int source_line_to_object_pointer = __LINE__;
ALWAYS_INLINE core::T_O* to_object_pointer( core::T_O* obj )
{
  void * x = reinterpret_cast< void * >(reinterpret_cast<cl_intptr_t>( (gctools::Tagged) obj));
  return translate::to_object< void * >::convert( x ).raw_();
}

// === END OF CORE TRANSLATORS ===

}; // eytern "C"

namespace llvmo
{

void initialize_raw_translators( void )
{
  // // TRANSLATOR WRAPPERS

  // // -- SHORT --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<SHORT>", &from_object_short,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_short );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<SHORT>", &to_object_short,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_short );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<USHORT>", &from_object_ushort,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_ushort );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<INT>", &to_object_ushort,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_ushort );

  // // -- INT --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<INT>", &from_object_int,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_int );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<INT>", &to_object_int,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_int );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<UINT>", &from_object_uint,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_uint );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<UINT>", &to_object_uint,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_uint );

  // // -- LONG --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<LONG>", &from_object_long,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_long );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<LONG>", &to_object_long,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_long );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<ULONG>", &from_object_ulong,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_ulong );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<ULONG>", &to_object_ulong,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_ulong );

  // // -- INT8 --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<INT8>", &from_object_int8,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_int8 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<INT8>", &to_object_int8,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_int8 );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<UINT8>", &from_object_uint8,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_uint8 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<UINT8>", &to_object_uint8,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_uint8 );

  // // -- INT16 --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<INT16>", &from_object_int16,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_int16 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<INT16>", &to_object_int16,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_int16 );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<UINT16>", &from_object_uint16,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_uint16 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<UINT16>", &to_object_uint16,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_uint16 );

  // // -- INT32 --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<INT32>", &from_object_int32,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_int32 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<INT32>", &to_object_int32,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_int32 );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<UINT32>", &from_object_uint32,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_uint32 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<UINT32>", &to_object_uint32,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_uint32 );

  // // -- INT64 --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<INT64>", &from_object_int64,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_int64 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<INT64>", &to_object_int64,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_int64 );

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<UINT64>", &from_object_uint64,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_uint64 );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<UINT64>", &to_object_uint64,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_uint64 );

  // // -- PTRDIFF --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<PTRDIFF>", &from_object_ptrdiff,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_ptrdiff );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<PTRDIFF>", &to_object_ptrdiff,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_ptrdiff );

  // // -- TIME --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<TIME>", &from_object_time,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_time );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<TIME>", &to_object_time,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_time );

  // // -- SIZE --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<SIZE>", &from_object_size,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_size );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<SIZE>", &to_object_size,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_size );

  // // -- SSIZE --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<SSIZE>", &from_object_ssize,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_ssize );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<SSIZE>", &to_object_ssize,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_ssize );

  // // -- CHAR --

  // wrap_translator( "CLASP-FFI","FROM-OBJECT<CHAR>", &from_object_char,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_from_object_size );

  // wrap_translator( "CLASP-FFI","TO-OBJECT<CHAR>", &to_object_char,
  //                  "", // arguments
  //                  "", // declares
  //                  "", // docstring,
  //                  __FILE__,
  //                  source_line_to_object_char );

  // // END OF TRANSLATOR WRAPPERS

  return;

} // initialize_raw_translators

}; // namespace llvmo

namespace llvmo {
void initialize_intrinsics() {
  // Do nothing
}
};

#pragma GCC visibility pop
