/*
    File: debugger.h
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
#ifndef debugger_H
#define debugger_H

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/stacks.h>

namespace core {
/*! This class controls the single-step state of the Lisp interpreter
  in an exception safe way.
  When you want to force a form to execute in single step mode
  you declare a LispDebugger(lisp,true) in the scope where you will
  evaluate the form and then when the form finishes it will restore
  the single step state to what it was.
*/

void core__low_level_backtrace();
void core__clib_backtrace(int depth = 999999999);

FORWARD(InvocationHistoryFrameIterator);

class LispDebugger {
private:
  bool _CanContinue;
  T_sp _Condition;

public:
  /* Immediatly returns if we are not single stepping or if the step stack level
	   is less than the current stack level.
	   Otherwise print the next instruction to be evaluated and wait for
	   the user to indicate what they want to do. */
  static void step();

public:
  /*! Print the current expression */
  void printExpression();

  /*! Invoke the debugger,
	  If the user is allowed to resume and opts to resume then return the resume object 
	*/
  T_sp invoke();

  InvocationHistoryFrameIterator_sp currentFrame() const;

  LispDebugger(T_sp condition);
  LispDebugger();

  virtual ~LispDebugger() {
    _G();
    _lisp->decrementDebuggerLevel();
  };
};

void af_backtrace();


void dbg_lowLevelDescribe(T_sp obj);
void dbg_describe_tagged_T_Optr(T_O *p);

 bool check_for_frame(uintptr_t);
 void frame_check(uintptr_t);

extern "C" {
void af_gotoIhsTop();
void af_gotoIhsNext();
void af_gotoIhsPrev();
void af_printCurrentIhsFrame();
void af_evalPrint(const string &expr);

// Generate a backtrace with JIT symbols resolved 
void c_bt();
void c_btcl();
};

int safe_backtrace(void**& return_buffer);

bool lookup_stack_map_entry(uintptr_t functionPointer, int& frameOffset, int& frameSize);
void register_jitted_object(const std::string& name, uintptr_t address, int size);

void push_one_llvm_stackmap(bool jit, uintptr_t& startAddress );

void register_llvm_stackmaps(uintptr_t startAddress, uintptr_t endAddress, size_t numberStackmaps);

 bool if_dynamic_library_loaded_remove(const std::string& libraryName);

 void add_dynamic_library_using_handle(const std::string& libraryName, void* handle);
 void add_dynamic_library_using_origin(bool is_executable, const std::string& libraryName, uintptr_t origin);
 
 void startup_register_loaded_objects();

bool lookup_address(uintptr_t address, const char*& symbol, uintptr_t& start, uintptr_t& end, char& type );

 typedef enum {undefined,symbolicated,lispFrame,cFrame} BacktraceFrameEnum ;
struct BacktraceEntry {
  BacktraceEntry() : _Stage(undefined),_ReturnAddress(0),_FunctionStart(0),_FunctionEnd(~0),_BasePointer(0),_InstructionOffset(0),_FrameSize(0),_FrameOffset(0), _FunctionDescription(0), _InvocationHistoryFrameAddress(0) {};
  BacktraceFrameEnum   _Stage;
  uintptr_t            _ReturnAddress;
  uintptr_t            _FunctionStart;
  uintptr_t            _FunctionEnd;
  uintptr_t            _BasePointer;
  int                  _InstructionOffset;
  int                  _FrameSize;
  int                  _FrameOffset;
  std::string          _SymbolName;
  uintptr_t            _FunctionDescription;
  uintptr_t            _InvocationHistoryFrameAddress;
};

};

extern "C" {
std::string dbg_safe_repr(uintptr_t raw);
void dbg_safe_print(uintptr_t raw);
void dbg_safe_println(uintptr_t raw);
void dbg_safe_backtrace();
void dbg_safe_backtrace_stderr();
};
#endif
