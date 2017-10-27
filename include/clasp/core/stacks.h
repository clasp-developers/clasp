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
  mutable const InvocationHistoryFrame *_Previous;
  mutable va_list   _args;
  size_t _remaining_nargs;
//  size_t _Bds;
 public:
 inline InvocationHistoryFrame(va_list rawArgList, size_t remaining_nargs)
   : _Previous(NULL)
//    , _Bds(my_thread->bindings().size())
    {
      va_copy(this->_args,rawArgList);
      this->_remaining_nargs = remaining_nargs;
#ifdef DEBUG_IHS
      this->validate();
#endif
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
//  int bds() const { return this->_Bds; };
  T_sp function() const;
  void* register_save_area() const;
  void validate() const;
};

#pragma GCC visibility pop

};


namespace core {
  size_t backtrace_size();
  string backtrace_as_string();
};

#ifdef DEBUG_IHS
  extern int global_debug_ihs;
#endif

namespace core {

  void invocation_history_stack_dump(const InvocationHistoryFrame* frame, const char* msg, size_t num_frames);
  size_t invocation_history_stack_depth(const InvocationHistoryFrame* frame);
  
  void validate_InvocationHistoryStack(int pushPop, const InvocationHistoryFrame* frame, const InvocationHistoryFrame* stackTop);
  void error_InvocationHistoryStack(const InvocationHistoryFrame* frame, const InvocationHistoryFrame* stackTop);

#if 0
  inline void push_InvocationHistoryStack(const InvocationHistoryFrame* frame);
  inline void pop_InvocationHistoryStack(const InvocationHistoryFrame* frame);
#else
  inline void push_InvocationHistoryStack(const InvocationHistoryFrame* frame) {
#ifdef DEBUG_IHS
  if (global_debug_ihs) validate_InvocationHistoryStack(1,frame,my_thread->_InvocationHistoryStackTop);
  void* frame_function_ptr = reinterpret_cast<void*>(gc::As_unsafe<Function_sp>(frame->function())->entry);
  global_debug_ihs_shadow_stack.push_back(frame_function_ptr);
  // Keep track of the last closure before things go haywire
  backtrace(my_thread->_IHSBacktrace,IHS_BACKTRACE_SIZE);
#endif
  frame->_Previous = my_thread->_InvocationHistoryStackTop;
  my_thread->_InvocationHistoryStackTop = frame;
}

inline void pop_InvocationHistoryStack(const InvocationHistoryFrame* frame) {
#ifdef DEBUG_IHS
  if (global_debug_ihs) validate_InvocationHistoryStack(0,frame,my_thread->_InvocationHistoryStackTop);
  global_debug_ihs_shadow_stack.pop_back();
#endif
  unlikely_if (frame != my_thread->_InvocationHistoryStackTop) error_InvocationHistoryStack(frame,my_thread->_InvocationHistoryStackTop);
  my_thread->_InvocationHistoryStackTop = my_thread->_InvocationHistoryStackTop->_Previous;
#ifdef DEBUG_IHS
  backtrace(my_thread->_IHSBacktrace,IHS_BACKTRACE_SIZE);
#endif
};
#endif
  
  struct SafeUpdateInvocationHistoryStack {
    const InvocationHistoryFrame* frame;
  SafeUpdateInvocationHistoryStack(const InvocationHistoryFrame* ihf) : frame(ihf) {
      push_InvocationHistoryStack(ihf);
    }
    ~SafeUpdateInvocationHistoryStack() {
      pop_InvocationHistoryStack(this->frame);
    }
  };
};

#define ALWAYS_INVOCATION_HISTORY_FRAME() \
  core::InvocationHistoryFrame zzzFrame(lcc_arglist_s._Args,lcc_arglist_s.remaining_nargs()); \
  core::SafeUpdateInvocationHistoryStack zzzStackUpdate(&zzzFrame);

#define INVOCATION_HISTORY_FRAME()

#endif /* _core_stacks_H_ */
