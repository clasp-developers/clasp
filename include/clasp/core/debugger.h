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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/stacks.h>
#include <clasp/core/conditions.h>

namespace core {
/*! This class controls the single-step state of the Lisp interpreter
  in an exception safe way.
  When you want to force a form to execute in single step mode
  you declare a LispDebugger(lisp,true) in the scope where you will
  evaluate the form and then when the form finishes it will restore
  the single step state to what it was.
*/

void core_lowLevelBacktrace();
void core_clibBacktrace(int depth = 999999999);

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

void initialize_debugging();

extern "C" {
void af_gotoIhsTop();
void af_gotoIhsNext();
void af_gotoIhsPrev();
void af_printCurrentIhsFrame();
void af_evalPrint(const string &expr);
};
};
#endif
