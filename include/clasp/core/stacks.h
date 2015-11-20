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
#include <clasp/core/lispVector.fwd.h>

namespace core {

/*! Put this macro anywhere in the C++ code and it will update the
  current source line number in the InvocationHistoryStack
  to the current __LINE__ in the current C++ source - this is done within the _G() macro */
//#define	_LINE() {::core::_threadIHS.top()->setLineNumberColumnForCxxFunction(__LINE__,0,__FUNCTION__);}

class InvocationHistoryStack;

#pragma GCC visibility push(default)
class InvocationHistoryFrame //: public gctools::StackRoot
    {
  friend class InvocationHistoryStack;

public:
  static const int NoLine = -1;
  static const int NoColumn = -1;

public:
  uint _Index;
  InvocationHistoryStack *_Stack;
  InvocationHistoryFrame *_Previous;
  int _Bds;
  gc::tagged_pointer<Closure> closure;
  T_sp environment;
  size_t _NumberOfArguments;
  core::T_O **_RegisterArguments;
  core::T_O **_StackArguments;

public:
  InvocationHistoryFrame(gctools::tagged_pointer<Closure> fc, core::T_O *valist_args, T_sp env = _Nil<T_O>());
  //	InvocationHistoryFrame(int sourceFileInfoHandle, int lineno, int column, ActivationFrame_sp env=_Nil<ActivationFrame_O>());
  ATTR_WEAK virtual ~InvocationHistoryFrame();
  InvocationHistoryFrame *previous() { return this->_Previous; };
  uint index() { return this->_Index; };
  VectorObjects_sp arguments() const;
  string argumentsAsString(int maxWidth) const;
  void dump() const;
  virtual void setActivationFrame(T_sp af) { this->environment = af; };
  virtual string asString() const;
  string asStringLowLevel(gctools::tagged_pointer<Closure> closure) const;
  virtual T_sp activationFrame() const { return this->environment; };
  virtual int bds() const { return this->_Bds; };

public:
};

#pragma GCC visibility pop

class InvocationHistoryStack {
private:
  InvocationHistoryFrame *_Top;

public:
  InvocationHistoryStack() : _Top(NULL){};

  InvocationHistoryFrame *top() const { return this->_Top; };
  inline void push(InvocationHistoryFrame *frame) {
    this->_Top = frame;
  }

  inline void pop() {
    GCTOOLS_ASSERT(this->_Top != NULL);
    this->_Top = this->_Top->previous();
  }

  uint size() {
    uint count = 0;
    InvocationHistoryFrame *cur = this->_Top;
    while (cur) {
      ++count;
      cur = cur->previous();
    }
    return count;
  }

  vector<InvocationHistoryFrame *> asVectorFrames();

  string asString() const;
};

class DynamicBinding {
public:
  Symbol_sp _Var;
  T_sp _Val;
  DynamicBinding(Symbol_sp sym, T_sp val) : _Var(sym), _Val(val){};
};

#pragma GCC visibility push(default)
class DynamicBindingStack {
public:
  gctools::Vec0<DynamicBinding> _Bindings;

public:
  inline int top() const { return this->_Bindings.size() - 1; }

  Symbol_sp topSymbol() const { return this->_Bindings.back()._Var; };

  Symbol_sp var(int i) const { return this->_Bindings[i]._Var; };
  T_sp val(int i) const { return this->_Bindings[i]._Val; };

  ATTR_WEAK void push(Symbol_sp var);
  ATTR_WEAK void pop();

  void reserve(int x) { this->_Bindings.reserve(x); };

  int size() const { return this->_Bindings.size(); };
};
#pragma GCC visibility pop
}

namespace core {

/*! Exception stack information */

typedef enum { NullFrame,
               CatchFrame,
               BlockFrame,
               TagbodyFrame,
               LandingPadFrame } FrameKind;
/*! Store the information for the exception 
      For CatchThrow:   _Obj1
    */
class ExceptionEntry {
public:
  ExceptionEntry() : _FrameKind(NullFrame), _Key(_Nil<T_O>()){};
  ExceptionEntry(FrameKind k, T_sp key) : _FrameKind(k), _Key(key){};
  FrameKind _FrameKind;
  T_sp _Key;
};

class ExceptionStack {
  FRIEND_GC_SCANNER(ExceptionStack);

public:
  gctools::Vec0<ExceptionEntry> _Stack;

public:
  ExceptionEntry &operator[](int i) { return this->_Stack[i]; };
  size_t size() const { return this->_Stack.size(); };
  string summary() {
    stringstream ss;
    ss << "ExceptionStackSummary: depth[" << this->size() << "] ";
    for (int idx = this->size() - 1; idx >= 0; --idx) {
      FrameKind fk = this->_Stack[idx]._FrameKind;
      char frameChar;
      switch (fk) {
      case NullFrame:
        frameChar = 'N';
        break;
      case CatchFrame:
        frameChar = 'C';
        break;
      case BlockFrame:
        frameChar = 'B';
        break;
      case TagbodyFrame:
        frameChar = 'T';
        break;
      case LandingPadFrame:
        frameChar = 'L';
        break;
      default:
        frameChar = 'u';
        break;
      }
      ss << frameChar << idx;
      if (this->_Stack[idx]._Key.notnilp()) {
        ss << "{@" << (void *)this->_Stack[idx]._Key.raw_() << "}";
      }
      ss << " ";
    };
    return ss.str();
  };

  size_t push(FrameKind kind, T_sp key) {
    size_t frame = this->_Stack.size();
    this->_Stack.emplace_back(kind, key);
    return frame;
  }
  void pop() {
    this->_Stack.pop_back();
  };
  /*! Return the index of the stack entry with the matching key.
          If return -1 then the key wasn't found */
  int findKey(FrameKind kind, T_sp key);
  T_sp backKey() const { return this->_Stack.back()._Key; };
  void unwind(size_t newTop) { this->_Stack.resize(newTop); };
  Vector_sp backtrace();
};
};

#define INVOCATION_HISTORY_FRAME() core::InvocationHistoryFrame zzzFrame(gctools::tagged_pointer<core::Closure>(this), lcc_arglist);

namespace core {
void initialize_stacks();
};

#endif /* _core_stacks_H_ */
