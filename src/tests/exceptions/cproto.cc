/*
    File: cproto.cc
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

#include <stdio.h>

#include "../../core/foundation.h"
#include "../../core/common.h"
#include "../../core/symbolTable.h"

typedef void (*afFunctionPtrType)(core::T_sp *resultP, core::ActivationFrame_sp *afP);

extern "C" {
extern core::ActivationFrame_sp *activationFrameNil();
extern void invokeLlvmFunction(core::T_sp *resultP, afFunctionPtrType fn, core::ActivationFrame_sp *afP);
extern void makeNil(core::T_sp *resultP);
extern void throwCatchThrow(core::T_sp *tag, core::T_sp *val);
extern void debugPrintI32(int x) throw();

struct CatchThrowStruct {
  core::T_sp *_tagP;
  core::T_sp *_resultP;
};

void cproto_catchThrow(core::T_sp *resultP, core::ActivationFrame_sp *afP) {
  printf("In repl\n");
  //    core::T_sp val;
  //    makeNil(&val);
  makeNil(resultP);
  try {
    throwCatchThrow(resultP, resultP);
  } catch (core::CatchThrow &thrownException) {
    debugPrintI32(2001);
    printf("Caught tag: %s\n", thrownException.getThrowObject()->__repr__().c_str());
  } catch (...) {
    debugPrintI32(2002);
    // Do cleanup here
    throw; // rethrow exception
  }
}

void cproto_unwind(core::T_sp *resultP, core::ActivationFrame_sp *afP) {
  printf("In repl\n");
  //    core::T_sp val;
  //    makeNil(&val);
  makeNil(resultP);
  try {
    throwCatchThrow(resultP, resultP);
  } catch (...) {
    debugPrintI32(1234001);
    throw;
  }
}

void ___main___(core::T_sp *resultP, core::ActivationFrame_sp *afP) {
  printf("Running c-function ___main___\n");
  core::ActivationFrame_sp *afNilP = activationFrameNil();
  invokeLlvmFunction(resultP, &cproto_unwind, afNilP);
  makeNil(resultP);
}
};
