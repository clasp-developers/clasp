/*
    File: stacks.h
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
#ifndef _core_stacks_H_
#define _core_stacks_H_

#include <clasp/core/foundation.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/sourceFileInfo.fwd.h>
#include <clasp/core/stacks.fwd.h>
#include <clasp/core/array.fwd.h>

namespace core {

/*! Put this macro anywhere in the C++ code and it will update the
  current source line number in the InvocationHistoryStack
  to the current __LINE__ in the current C++ source - this is done within the _G() macro */
//#define	_LINE() {::core::_threadIHS.top()->setLineNumberColumnForCxxFunction(__LINE__,0,__FUNCTION__);}

class InvocationHistoryStack;

#pragma GCC visibility push(default)
class InvocationHistoryFrame //: public gctools::StackRoot
{
 public:
  static const int NoLine = -1;
  static const int NoColumn = -1;

 public:
  const InvocationHistoryFrame *_Previous;
  mutable va_list   _args;
  size_t _remaining_nargs;
  size_t _Bds;
 public:
 InvocationHistoryFrame(va_list rawArgList, size_t remaining_nargs)
   : _Previous(my_thread->_InvocationHistoryStack),
    _Bds(my_thread->bindings().size())
    {
      va_copy(this->_args,rawArgList);
      this->_remaining_nargs = remaining_nargs;
    }
  //Closure_sp fc, core::T_O *valist_args, T_sp env = _Nil<T_O>());

  //	InvocationHistoryFrame(int sourceFileInfoHandle, int lineno, int column, ActivationFrame_sp env=_Nil<ActivationFrame_O>());
//  VaList_sp valist_sp() const { return VaList_sp((gc::Tagged)this->_RawArgList); };
  const InvocationHistoryFrame *previous() const { return this->_Previous; };
  SimpleVector_sp arguments() const;
  string argumentsAsString(int maxWidth) const;
  void dump(int index) const;
  string asString(int index) const;
  string asStringLowLevel(Closure_sp closure,int index) const;
  int bds() const { return this->_Bds; };
  Function_sp function() const;
  void* register_save_area() const;
};

#pragma GCC visibility pop

};


namespace core {
  size_t backtrace_size();
  string backtrace_as_string();
};

namespace core {
  struct SafeUpdateInvocationHistoryStack {
    SafeUpdateInvocationHistoryStack(InvocationHistoryFrame& ihf) {
      my_thread->_InvocationHistoryStack = &ihf;
    }
    ~SafeUpdateInvocationHistoryStack() {
      my_thread->_InvocationHistoryStack = my_thread->_InvocationHistoryStack->_Previous;
    }
  };
};

#ifdef USE_EXPENSIVE_BACKTRACE
#define INVOCATION_HISTORY_FRAME() \
  core::InvocationHistoryFrame zzzFrame(lcc_arglist_s._Args,lcc_arglist_s.remaining_nargs()); \
  core::SafeUpdateInvocationHistoryStack zzzStackUpdate(zzzFrame);
#else
#define INVOCATION_HISTORY_FRAME()
#endif

#endif /* _core_stacks_H_ */
