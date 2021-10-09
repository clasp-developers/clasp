/*
    File: singleDispatchGenericFunction.h
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
#ifndef _SINGLEDISPATCHGENERICFUNCTION_H_
#define _SINGLEDISPATCHGENERICFUNCTION_H_

#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/singleDispatchGenericFunction.fwd.h>
#include <clasp/core/singleDispatchMethod.fwd.h>
#include <atomic>

namespace core {
FORWARD(SingleDispatchMethod);
class SingleDispatchGenericFunction_O : public Closure_O {
  LISP_CLASS(core,CorePkg,SingleDispatchGenericFunction_O,"SingleDispatchGenericFunction",Closure_O);
public:
  SingleDispatchGenericFunction_O(GlobalEntryPoint_sp ep) : Base(ep) {};
public:
  typedef enum {
      REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY = 0,
      REF_SINGLE_DISPATCH_SPECIALIZER_LAMBDA_LIST_HANDLER = 1,
      REF_SINGLE_DISPATCH_SPECIALIZER_DISPATCH_ARGUMENT_INDEX = 2,
      REF_SINGLE_DISPATCH_SPECIALIZER_METHODS = 3,
      REF_SINGLE_DISPATCH_SPECIALIZER_SLOTS = 4
  } SingleDispatchSlots;
public:
  static SingleDispatchGenericFunction_sp create_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, size_t singleDispatchArgumentIndex);
  static LCC_RETURN single_dispatch_funcallable_entry_point(LCC_ARGS_ELLIPSIS);
public:
  std::atomic<T_sp> callHistory;
  LambdaListHandler_sp lambdaListHandler;
  Fixnum_sp argumentIndex;
  std::atomic<T_sp> methods;
};

};


namespace core {
SingleDispatchGenericFunction_sp core__ensure_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, bool autoExport, size_t singleDispatchArgumentIndex);
// void core__satiateSingleDispatchGenericFunctions();

};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
