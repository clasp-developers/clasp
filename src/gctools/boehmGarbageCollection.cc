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
#include <clasp/gctools/stw.h>
#include <clasp/core/object.h>
#include <clasp/core/lispStream.h>
// #include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gctoolsPackage.h>
#ifdef USE_BOEHM // whole file #ifdef USE_BOEHM
#include <clasp/gctools/boehm_config.h>
// boehm_config defines a PACKAGE macro and that will mess with the scraper - so undef it.
#undef PACKAGE
#include <clasp/gctools/boehmGarbageCollection.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/debugger.h>
#include <clasp/core/function.h>
#include <clasp/core/compiler.h>
#include <clasp/gctools/snapshotSaveLoad.h>

#ifdef USE_PRECISE_GC
#include "src/bdwgc/include/gc_mark.h"
#endif // USE_PRECISE_GC

extern "C" {
void boehm_park() {
  GC_disable();
};

void boehm_release() {
  GC_enable();
}
};

// ------------

namespace gctools {

void collect_garbage() { GC_gcollect(); }

void clasp_warn_proc(char* msg, GC_word arg) {
#if 0
  // This warning is annoying and comes up a lot but doesn't cause problems except for when building ironclad
  printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__);
  printf(msg, arg);
#endif
}

void invoke_finalizers() {
  GC_invoke_finalizers();
}

}; // namespace gctools

namespace gctools {
__attribute__((noinline)) void startupBoehm(gctools::ClaspInfo* claspInfo) {
  GC_set_handle_fork(1);
  GC_INIT();
  GC_allow_register_threads();
  GC_set_java_finalization(1);
#ifdef ALL_INTERIOR_POINTERS
  GC_set_all_interior_pointers(1); // tagged pointers require this
                                   // printf("%s:%d Turning on interior pointers\n",__FILE__,__LINE__);
#else
    printf("%s:%d:%s ALL_INTERIOR_POINTERS is 0 - so interior pointers should not be detected\n",
           __FILE__, __LINE__, __FUNCTION__ );
    if (REGISTER_TAG_DISPLACEMENTS) {
      printf("%s:%d:%s Calling GC_register_displacement for GENERAL_TAG and CONS_TAG\n",
             __FILE__, __LINE__, __FUNCTION__ );
      GC_register_displacement(GENERAL_TAG);
      GC_register_displacement(CONS_TAG);
    } else {
      printf("%s:%d:%s NOT calling GC_register_displacement for GENERAL_TAG and CONS_TAG - I WANT Clasp to crash to see if displacements work\n",
             __FILE__, __LINE__, __FUNCTION__ );
    }
#endif
  GC_set_warn_proc(clasp_warn_proc);
  //  GC_enable_incremental();
  GC_init();

  my_thread =
    (core::ThreadLocalState*)ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(core::ThreadLocalState));
  new (my_thread) core::ThreadLocalState(false);
  my_thread_low_level = &my_thread->_LowLevel;

#if 1
  // I'm not sure if this needs to be done for the main thread
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#endif

  //
  // Set up the _lisp and symbols memory as roots
  //
  gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*)&_lisp;
  GC_add_roots((void*)lispRoot, (void*)((char*)lispRoot + sizeof(void*)));
  //  gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*)&global_core_symbols[0];
  //  GC_add_roots((void*)coreSymbolRoots,(void*)((char*)coreSymbolRoots+sizeof(void*)*NUMBER_OF_CORE_SYMBOLS));
  gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*)&global_symbols[0];
  GC_add_roots((void*)symbolRoots, (void*)((char*)symbolRoots + sizeof(void*) * global_symbol_count));

  //  void* dummy = GC_generic_malloc_uncollectable(592,4);
}

void shutdownBoehm() {
  GC_FREE(my_thread);
  delete my_thread_low_level;
}

bool heap_ptr_p(const void* p) {
  return GC_is_heap_ptr(p);
}

size_t heap_size() { return GC_get_heap_size(); }
size_t free_bytes() { return GC_get_free_bytes(); }
size_t bytes_since_gc() { return GC_get_bytes_since_gc(); }

CL_DEFUN size_t core__dynamic_space_size() { return GC_get_total_bytes(); }

CL_DEFUN size_t core__dynamic_usage() { return GC_get_heap_size(); }

void clasp_gc_registerRoots(void* rootsStart, size_t numberOfRoots) {
#ifdef CLASP_APPLE_SILICON
  //
  // This is experimental for Apple Silicon M1 chip
  //
  // It doesn't allow RWX memory - so we use GC_add_roots to add roots to boehm for code.
  //   This runs into the hard limit of 8,192 root sets very quickly
  //
  void* rootsEnd = (void*)((uintptr_t)rootsStart + numberOfRoots * sizeof(void*));
  if (rootsEnd != rootsStart) {
#if 0
    // debugging
    void* base = GC_base(rootsStart);
    printf("%s:%d:%s GC_add_roots %p to %p\n", __FILE__, __LINE__, __FUNCTION__, rootsStart, rootsEnd);
    if (base) {
      printf("    ---- the roots are INSIDE GC managed memory - GC_add_roots is not needed\n");
    } else {
      printf("    ---- the roots are OUTSIDE GC managed memory - GC_add_roots is NEEDED\n");
    }
#endif
    GC_add_roots(rootsStart, rootsEnd);
  }
#endif
}

void clasp_gc_deregisterRoots(void* rootsStart, size_t numberOfRoots) {
#ifdef CLASP_APPLE_SILICON
  //
  // This is experimental for Apple Silicon M1 chip
  //
  // It doesn't allow RWX memory - so we use GC_add_roots to add roots to boehm for code.
  //   This runs into the hard limit of 8,192 root sets very quickly
  //
  void* rootsEnd = (void*)((uintptr_t)rootsStart + numberOfRoots * sizeof(void*));
  if (rootsEnd != rootsStart) {
    printf("%s:%d:%s GC_add_roots %p to %p\n", __FILE__, __LINE__, __FUNCTION__, rootsStart, rootsEnd);
    GC_remove_roots(rootsStart, rootsEnd);
  }
#endif
}

};     // namespace gctools
#endif // whole file #ifdef USE_BOEHM
