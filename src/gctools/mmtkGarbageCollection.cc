/*
    File: boehmGarbageCollection.cc
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
// #include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gctoolsPackage.h>
#ifdef USE_MMTK
#include <clasp/gctools/mmtkGarbageCollection.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/debugger.h>
#include <clasp/core/compiler.h>
#include <clasp/gctools/snapshotSaveLoad.h>

THREAD_LOCAL MMTk_Mutator my_mutator;

namespace gctools {
__attribute__((noinline)) int initializeMmtk(int argc, char* argv[], bool mpiEnabled, int mpiRank, int mpiSize) {
  gc_init((size_t)(1024 * 1024 * 1024) * (size_t)4);

  void* topOfStack;
  my_mutator = bind_mutator(topOfStack);

  // ctor sets up my_thread
  gctools::ThreadLocalStateLowLevel thread_local_state_low_level(&topOfStack);
  core::ThreadLocalState thread_local_state(false); // special ctor that does not require _Nil be defined
  my_thread_low_level = &thread_local_state_low_level;
  my_thread = &thread_local_state;

#if 0
  // I'm not sure if this needs to be done for the main thread
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#endif

#ifndef SCRAPING
#define ALL_PREGCSTARTUPS_CALLS
#include PRE_GC_STARTUP_INC_H
#undef ALL_PREGCSTARTUPS_CALLS

  //
  // Set up the _lisp and symbols memory as roots
  //
  gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*)&_lisp;
  GC_add_roots((void*)lispRoot, (void*)((char*)lispRoot + sizeof(void*)));
  gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*)&global_core_symbols[0];
  GC_add_roots((void*)coreSymbolRoots, (void*)((char*)coreSymbolRoots + sizeof(void*) * NUMBER_OF_CORE_SYMBOLS));
  gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*)&global_symbols[0];
  GC_add_roots((void*)symbolRoots, (void*)((char*)symbolRoots + sizeof(void*) * global_symbol_count));

#endif
  int exitCode;
  try {
    exitCode = startupFn(argc, argv, mpiEnabled, mpiRank, mpiSize);
  } catch (core::SaveLispAndDie& ee) {
#ifdef USE_PRECISE_GC
    snapshotSaveLoad::snapshot_save(ee);
#endif
    exitCode = 0;
  }
  return exitCode;
}

};     // namespace gctools
#endif // whole file #ifdef USE_MMTK
