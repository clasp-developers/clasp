/*
    File: builtins.cc
    Small functions used by the runtime that should always be inlined.
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

#include <clasp/core/core.h>
#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/derivableCxxObject.h>
#include <clasp/gctools/gcStack.h>
#include <clasp/llvmo/intrinsics.h>

#if 0 // DEBUGGING
#define NO_UNWIND_BEGIN_BUILTINS() NO_UNWIND_BEGIN()
#define NO_UNWIND_END_BUILTINS() NO_UNWIND_END()
#define ENSURE_VALID_OBJECT_BUILTINS(x) ENSURE_VALID_OBJECT(x)
#else
#define NO_UNWIND_BEGIN_BUILTINS()
#define NO_UNWIND_END_BUILTINS()
#define ENSURE_VALID_OBJECT_BUILTINS(x) x
#endif

#define BUILTIN_ATTRIBUTES __attribute__((always_inline))



extern "C" {

BUILTIN_ATTRIBUTES void cc_rewind_va_list(va_list va_args, void** register_save_areaP)
{NO_UNWIND_BEGIN_BUILTINS();
  LCC_REWIND_VA_LIST(va_args,register_save_areaP);
  NO_UNWIND_END_BUILTINS();
}


/* cc_setup_vaslist

   Builds a vaslist rewound to the first required argument from a va_list.
   Return the tagged vaslist.
*/
BUILTIN_ATTRIBUTES core::T_O* cc_setup_vaslist(core::Vaslist* vaslist, va_list va_args, size_t nargs)
{
  new (vaslist) core::Vaslist(nargs,va_args);
  LCC_REWIND_VA_LIST_KEEP_REGISTER_SAVE_AREA(vaslist->_Args);
  return gctools::tag_vaslist<core::T_O*>(vaslist);
}

/* Setup the vaslist and rewind it using the va_list inside of the vaslist.
   Return the tagged vaslist. */
BUILTIN_ATTRIBUTES core::T_O* cc_setup_vaslist_internal(core::Vaslist* vaslist, size_t nargs)
{
  LCC_REWIND_VA_LIST_KEEP_REGISTER_SAVE_AREA(vaslist->_Args);
  vaslist->remaining_nargs() = nargs;
  return gctools::tag_vaslist<core::T_O*>(vaslist);
}

/* Remove one item from the vaslist and return it.
   Do not call this unless you are sure there are elements to pop without DEBUG_BUILD */
BUILTIN_ATTRIBUTES core::T_O* cx_vaslist_pop(core::T_O *preVaslist)
{NO_UNWIND_BEGIN(); // check_remaining_nargs doesn't actually throw.
  core::VaList_sp vaslist((gctools::Tagged)preVaslist);
  return vaslist->next_arg_raw();
  NO_UNWIND_END();
}
};


#include <clasp/llvmo/read-stamp.cc>

extern "C" {
// mostly duplicates fastgf.cc, ew.
// Idea here is we're moving away from even having fastgf.cc.
BUILTIN_ATTRIBUTES core::T_O* cx_read_stamp(core::T_O* obj)
{
  return llvmo::template_read_stamp<core::T_O>(obj);
#if 0
  uintptr_t tag = reinterpret_cast<uintptr_t>(obj)&gctools::tag_mask;
  int64_t stamp;
  switch (tag) {
  case FIXNUM0_TAG:
      return core::make_fixnum(gctools::STAMP_FIXNUM).raw_();
  case GENERAL_TAG: {
  // do more stuff to get the stamp
    core::General_O* client_ptr = reinterpret_cast<core::General_O*>(gctools::untag_general<core::T_O*>(obj));
    const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client_ptr));
    uint64_t stamp = header.stamp();
    if (stamp == gctools::STAMP_core__Instance_O ||
        stamp == gctools::STAMP_core__FuncallableInstance_O ||
        stamp == global_TheClassRep_stamp ) {
      core::Instance_O* instance_ptr = reinterpret_cast<core::Instance_O*>(client_ptr);
      core::SimpleVector_O* rack = reinterpret_cast<core::SimpleVector_O*>(gctools::untag_general<core::T_O*>(instance_ptr->_Rack.raw_()));
      return core::make_fixnum((*rack)[0].unsafe_fixnum()).raw_();
    } else if ( stamp == gctools::STAMP_core__WrappedPointer_O ) {
      core::WrappedPointer_O* wrapped_ptr = reinterpret_cast<core::WrappedPointer_O*>(client_ptr);
      return core::make_fixnum(wrapped_ptr->Stamp_).raw_();
    } else if ( stamp == gctools::STAMP_core__DerivableCxxObject_O ) {
      core::DerivableCxxObject_O* derivable_cxx_object_ptr = reinterpret_cast<core::DerivableCxxObject_O*>(client_ptr);
      return core::make_fixnum(derivable_cxx_object_ptr->get_stamp_()).raw_();
    } else {
      return core::make_fixnum(stamp).raw_();
    }
  }
  case CHARACTER_TAG:
      return core::make_fixnum(gctools::STAMP_CHARACTER).raw_();
  case CONS_TAG:
      return core::make_fixnum(gctools::STAMP_CONS).raw_();
  case FIXNUM1_TAG:
      return core::make_fixnum(gctools::STAMP_FIXNUM).raw_();
  case VASLIST_TAG:
      return core::make_fixnum(gctools::STAMP_VA_LIST_S).raw_();
  case SINGLE_FLOAT_TAG:
      return core::make_fixnum(gctools::STAMP_SINGLE_FLOAT).raw_();
  }
  return core::make_fixnum(123456).raw_();
#endif
}


BUILTIN_ATTRIBUTES
core::T_O *cc_symbol_function(core::T_O *symP) {
  core::Symbol_sp sym((gctools::Tagged)symP);
  core::Function_sp func((gc::Tagged)(sym)->_Function.theObject);
  return func.raw_();
}

/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
BUILTIN_ATTRIBUTES core::T_O* setfSymbolFunctionRead(const core::T_O *tsymP)
{NO_UNWIND_BEGIN();
  const core::Symbol_sp sym((gctools::Tagged)tsymP);
  core::Function_sp setfFunc = sym->getSetfFdefinition(); //_lisp->get_setfDefinition(*symP);
  return setfFunc.raw_();
  NO_UNWIND_END();
}

BUILTIN_ATTRIBUTES core::T_O** lexicalValueReference(size_t depth, size_t index, core::ActivationFrame_O *frameP)
{
  core::ActivationFrame_sp af((gctools::Tagged)frameP);
  core::T_sp& value_ref = core::value_frame_lookup_reference(af, depth, index);
  return &value_ref.rawRef_();
}

BUILTIN_ATTRIBUTES core::T_O** registerReference(core::T_O** register_)
{
  return register_;
}

// The following two are only valid for non-simple arrays. Be careful!
BUILTIN_ATTRIBUTES core::T_O* cc_realArrayDisplacement(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->realDisplacedTo().raw_();
}
BUILTIN_ATTRIBUTES size_t cc_realArrayDisplacedIndexOffset(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->displacedIndexOffset();
}

BUILTIN_ATTRIBUTES size_t cc_arrayTotalSize(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->arrayTotalSize();
}

BUILTIN_ATTRIBUTES size_t cc_arrayRank(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->rank();
}

BUILTIN_ATTRIBUTES size_t cc_arrayDimension(core::T_O* tarray, size_t axis) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->arrayDimension(axis);
}

BUILTIN_ATTRIBUTES uint cc_simpleBitVectorAref(core::T_O* tarray, size_t index) {
  core::SimpleBitVector_O* array = reinterpret_cast<core::SimpleBitVector_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->testBit(index);
}

BUILTIN_ATTRIBUTES void cc_simpleBitVectorAset(core::T_O* tarray, size_t index, uint v) {
  core::SimpleBitVector_O* array = reinterpret_cast<core::SimpleBitVector_O*>(gctools::untag_general<core::T_O*>(tarray));
  array->setBit(index, v);
}

BUILTIN_ATTRIBUTES core::T_O* invisible_makeValueFrameSetParent(core::T_O* parent) {
  return parent;
}

BUILTIN_ATTRIBUTES core::T_O* invisible_makeBlockFrameSetParent(core::T_O* parent) {
  return parent;
}

BUILTIN_ATTRIBUTES core::T_O* invisible_makeTagbodyFrameSetParent(core::T_O* parent) {
  return parent;
}

#if 0
BUILTIN_ATTRIBUTES core::T_O* invisible_makeValueFrameSetParentFromClosure(core::T_O* closureRaw) {
  if (closureRaw!=NULL) {
    core::Closure_O* closureP = reinterpret_cast<core::Closure_O*>(gc::untag_general<core::T_O*>(closureRaw));
    core::T_sp activationFrame = closureP->closedEnvironment();
    return activationFrame.raw_(); // >rawRef_() = closureRaw; //  = activationFrame;
  } else {
    return _Nil<core::T_O>().raw_();
  }
}
#endif


/*! Return i32 1 if (valP) is != unbound 0 if it is */
BUILTIN_ATTRIBUTES int isBound(core::T_O *valP)
{
  return gctools::tagged_unboundp<core::T_O*>(valP) ? 0 : 1;
}

/*! Return i32 1 if (valP) is != nil 0 if it is */
BUILTIN_ATTRIBUTES int isTrue(core::T_O* valP)
{
  return gctools::tagged_nilp<core::T_O*>(valP) ? 0 : 1;
}

/*! Return i32 1 if (valP) is != nil 0 if it is */
BUILTIN_ATTRIBUTES core::T_O* valueOrNilIfZero(gctools::return_type val) {
  return val.nvals ? val.ret0[0] : _Nil<core::T_O>().raw_();
}

BUILTIN_ATTRIBUTES core::T_O** activationFrameReferenceFromClosure(core::T_O* closureRaw)
{
  ASSERT(closureRaw);
  if (closureRaw!=NULL) {
    core::ClosureWithSlots_sp closure = core::ClosureWithSlots_sp((gctools::Tagged)closureRaw);
    return &closure->closedEnvironment_rawRef();
  }
  return NULL;
}

BUILTIN_ATTRIBUTES void* cc_vaslist_va_list_address(core::T_O* vaslist)
{
  return &(gctools::untag_vaslist(vaslist)->_Args);
};

BUILTIN_ATTRIBUTES size_t* cc_vaslist_remaining_nargs_address(core::Vaslist* vaslist)
{
  return &(gctools::untag_vaslist(vaslist)->_remaining_nargs);
};


BUILTIN_ATTRIBUTES core::T_O *cc_fetch(core::T_O *tagged_closure, std::size_t idx)
{
  gctools::smart_ptr<core::ClosureWithSlots_O> c = gctools::smart_ptr<core::ClosureWithSlots_O>((gc::Tagged)tagged_closure);
  return (*c)[idx].raw_();
}

BUILTIN_ATTRIBUTES core::T_O *cc_readCell(core::T_O *cell)
{
  core::Cons_sp cp((gctools::Tagged)cell);
  return CONS_CAR(cp).raw_();
}

BUILTIN_ATTRIBUTES core::T_O* cc_builtin_nil()
{
  return _Nil<core::T_O>().raw_();
};

BUILTIN_ATTRIBUTES core::T_O* bc_function_from_function_designator(core::T_O* function_designator)
{
  core::T_sp tfunction_designator((gctools::Tagged)function_designator);
  if (gc::IsA<core::Function_sp>(tfunction_designator)) {
    return function_designator;
  } else if (gc::IsA<core::Symbol_sp>(tfunction_designator)) {
    core::Symbol_sp sym = gc::As_unsafe<core::Symbol_sp>(tfunction_designator);
    core::Function_sp func((gc::Tagged)(sym)->_Function.theObject);
    return func.raw_();
  }
  llvmo::not_function_designator_error(tfunction_designator);
};


};


extern "C" {
BUILTIN_ATTRIBUTES core::T_O* ignore_initializeBlockClosure( core::T_O** dummy)
{
// Do nothing but return NULL which will never match a handle
  return NULL;
}

BUILTIN_ATTRIBUTES core::T_O* ignore_initializeTagbodyClosure(core::T_O** dummy)
{
// Do nothing but return NULL which will never match a handle
  return NULL;
}


BUILTIN_ATTRIBUTES gctools::return_type ignore_blockHandleReturnFrom(unsigned char *exceptionP, core::T_O* handle) {
#if 1
  core::ReturnFrom &returnFrom = (core::ReturnFrom &)*((core::ReturnFrom *)(exceptionP));
  if (returnFrom.getHandle() == handle) {
    printf("%s:%d There is a serious problem - an ignore_blockHandleReturnFrom expecting frame %p recieved a returnFrom with frame %p - no such returnFrom should be sent - the block/return-from optimization is broken\n", __FILE__, __LINE__, handle, returnFrom.getHandle());
    abort();
  }
#endif
  throw;
}


BUILTIN_ATTRIBUTES void debugBreak() {
  asm("int $03");
}

BUILTIN_ATTRIBUTES void ignore_exceptionStackUnwind()
{
// Do nothing
}
};
