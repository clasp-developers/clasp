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
BUILTIN_ATTRIBUTES uint64_t cx_read_stamp(core::T_O* obj, uint64_t stamp)
{
  uint64_t old_stamp = (uint64_t)llvmo::template_read_stamp<core::T_O>(obj);
  cc_match((core::T_O*)old_stamp,(core::T_O*)stamp);
  return old_stamp;
}

#if 0
BUILTIN_ATTRIBUTES core::T_O** registerReference(core::T_O** register_)
{
  return register_;
}
#endif

BUILTIN_ATTRIBUTES uint cc_simpleBitVectorAref(core::T_O* tarray, size_t index) {
  core::SimpleBitVector_O* array = reinterpret_cast<core::SimpleBitVector_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->testBit(index);
}

BUILTIN_ATTRIBUTES void cc_simpleBitVectorAset(core::T_O* tarray, size_t index, uint v) {
  core::SimpleBitVector_O* array = reinterpret_cast<core::SimpleBitVector_O*>(gctools::untag_general<core::T_O*>(tarray));
  array->setBit(index, v);
}

/*! Return i32 1 if (valP) is != nil 0 if it is */
BUILTIN_ATTRIBUTES int isTrue(core::T_O* valP)
{
  return gctools::tagged_nilp<core::T_O*>(valP) ? 0 : 1;
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


