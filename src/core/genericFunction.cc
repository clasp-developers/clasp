/*
    File: genericFunction.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/bformat.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/predicates.h>
#include <clasp/core/cache.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/wrappers.h>
#include <clasp/llvmo/intrinsics.h>

#define CACHE_METHOD_LOOKUP

namespace core {

SYMBOL_EXPORT_SC_(ClPkg, compute_applicable_methods);
SYMBOL_SC_(ClosPkg, compute_applicable_methods_using_classes);
SYMBOL_SC_(ClosPkg, compute_effective_method_function);

/*! Reproduces functionality in FEnot_funcallable_vararg */
LCC_RETURN not_funcallable_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  SIMPLE_ERROR(BF("Not a funcallable instance %s") % _rep_(gf));
}



CL_DEFUN void core__print_object_address(T_sp obj)
{
  printf("%s:%d print_object %s @%p\n", __FILE__, __LINE__, _rep_(obj).c_str(), (void*)obj.raw_());
}

CL_DEFUN T_sp core__object_address(T_sp obj)
{
  return Pointer_O::create(&*obj);
}

};
