/*
    File: mpPackage.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/symbol.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>
#include <clasp/core/evaluator.h>



namespace mp {


SYMBOL_SC_(MpPkg, aSingleMpSymbol);
SYMBOL_EXPORT_SC_(MpPkg, roo);


void start_thread(Process_sp process, core::T_sp function) {
  void* stack_base;
  core::ThreadLocalState my_thread_local_state(&stack_base);
  printf("%s:%d entering start_thread  &my_thread -> %p \n", __FILE__, __LINE__, (void*)&my_thread);
  my_thread = &my_thread_local_state;
  my_thread->initialize_thread();
  gctools::register_thread(process,stack_base);
  core::eval::funcall(function);
  gctools::unregister_thread(process);
  printf("%s:%d leaving start_thread\n", __FILE__, __LINE__);
}


Process_O::Process_O(core::T_sp function) : _Function(function), _Thread(start_thread,this->asSmartPtr(),function) {};
  
#if 0
void initialize_mpPackage() {
  list<string> lnicknames;
  list<string> luse = {"COMMON-LISP"};
  _lisp->makePackage("MP", lnicknames, luse);
  // We don't have to create the MP symbols here - it's done in bootStrapCoreSymbolMap
}
#endif
};
