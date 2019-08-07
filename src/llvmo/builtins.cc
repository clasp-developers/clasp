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

gctools::return_type cm_check_index(void* index, void* max, void* axis )
{
  if (((intptr_t)(index)<0) || (((intptr_t)(index)>=(intptr_t)max))) invalid_index_error(index,max,axis);
  gctools::return_type result(_Nil<core::T_O>().raw_(),0);
  return result;
}


gctools::return_type cm_vset(void* vector, void* idx, void* value)
{
  core::AbstractSimpleVector_sp asv((gctools::Tagged)vector);
  core::T_sp tvalue((gctools::Tagged)value);
  asv->vset(untag_fixnum((core::T_O*)idx),tvalue);
  gctools::return_type result(value,1);
  return result;
};


gctools::return_type cm_vref(void* vector, void* idx)
{
  core::AbstractSimpleVector_sp asv((gctools::Tagged)vector);
  gctools::return_type result(asv->vref(untag_fixnum((core::T_O*)idx)).raw_(),1);
  return result;
};


};
