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

void clasp_warn_proc(char* msg, GC_word arg) {
#if 0
  // This warning is annoying and comes up a lot but doesn't cause problems except for when building ironclad
  printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__);
  printf(msg, arg);
#endif
}

void run_finalizers(core::T_sp obj, void* data) {
  // The _sym_STARfinalizersSTAR weak-key-hash-table will be useless at this point
  // because Boehm wipes out the entry before the finalizer is called - but we have
  // a backup plan.  The data pointer may be NULL but otherwise points to a tagged pointer in the
  // GC UNCOLLECTABLE memory that contains the list of finalizers that we would otherwise have
  // looked up in the *finalizers* weak-key-hash-table
  // if data is NULL then there are no finalizers to run
  //
  // The GCInfo NeedsFinalization = true will call in here and if data is NULL
  // then we want to immediately return.
  if (data == NULL)
    return;
  gctools::Tagged finalizers_tagged = *reinterpret_cast<gctools::Tagged*>(data);
  core::List_sp finalizers = core::T_sp(finalizers_tagged);
  for (auto cur : finalizers)
    core::eval::funcall(oCar(cur), obj);
  // Now release the memory pointed to by data
  GC_FREE(data);
}

void boehm_general_finalizer_from_BoehmFinalizer(void* client, void* data) {
  core::T_sp obj((gctools::Tagged)tag_general((core::T_O*)client));
  if (data != NULL) {
    run_finalizers(obj, data);
  }
}

void boehm_general_finalizer(void* base, void* data) {
  void* client = HeaderPtrToGeneralPtr<core::T_O>(base);
  if ((uintptr_t)client & gctools::ptag_mask) {
    printf("%s:%d The client pointer %p must NOT BE TAGGED at this point\n", __FILE__, __LINE__, client);
    abort();
  }
  core::T_sp obj((gctools::Tagged)tag_general((core::T_O*)client));
  if (data != NULL) {
    run_finalizers(obj, data);
  }
}

void boehm_cons_finalizer(void* base, void* data) {
  ASSERT(CONS_HEADER_SIZE == 0);
  if (data != NULL) {
    void* client = gctools::HeaderPtrToConsPtr(base);
    core::Cons_sp obj((core::Cons_O*)client);
    run_finalizers(obj, data);
  }
}

/*! Register a list of finalizers to run when object is finalized.
    This function can be called multiple times - it replaces any previous
    list of finalizers with this new one.
*/
void set_finalizer_list(core::T_sp object, core::List_sp finalizers) {
  // First check if there is already a finalizer and data.
  // The data may be NULL  - that means we need to allocate a pointer in the UNCOLLECTABLE memory to keep a
  // linked list of Common Lisp finalizers alive.
  //    The finalizers tagged pointer is written into the UNCOLLECTABLE memory.
  // There may be a non-NULL old_finalizer - that means the gcalloc.h code installed a finalizer that
  // will invoke the objects C++ destructor.  We can continue to use the gcalloc.h installed finalizer
  // because it will invoke Common Lisp finalizers before invoking the destructor.
  // If we do need to install a new finalizer - we need to install either the boehm_general_finalizer
  // or the boehm_cons_finalizer depending on the type of object.
  if (object.generalp()) {
    GC_finalization_proc orig_finalizer, dummy_finalizer;
    void* data;
    void* dummy_data;
    void* base = SmartPtrToBasePtr(object);
    // Check current finalizers. There is no way to do this in Boehm other than
    // calling this to install new finalizers.
    GC_register_finalizer_no_order(base, (GC_finalization_proc)0, 0, &orig_finalizer, &data);
    if (data == NULL) {
      // no list exists - create an uncollectable pointer to one.
      // this pointer needs to be scannable by the GC since it's to a regular
      // old GC'd list.
      gctools::Tagged* new_data = reinterpret_cast<gctools::Tagged*>(ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(core::Cons_O*)));
      data = (void*)new_data;
    }
    // Write the finalizers list into the UNCOLLECTABLE space
    *(reinterpret_cast<gctools::Tagged*>(data)) = finalizers.tagged_();
    // If there was no finalizer, make it boehm_general_finalizer which calls
    // the elements of the list. If there was one, it's BoehmFinalizer<CLASS>
    // which will call the list and then the destructor.
    if (!orig_finalizer) orig_finalizer = boehm_general_finalizer;
    GC_register_finalizer_no_order(base, orig_finalizer, data, &dummy_finalizer, &dummy_data);
  } else if (object.consp()) {
    GC_finalization_proc orig_finalizer, dummy_finalizer;
    void* data;
    void* dummy_data;
    void* base = (void*)gctools::ConsPtrToHeaderPtr(object.untag_object());
    GC_register_finalizer_no_order(base, (GC_finalization_proc)0, 0, &orig_finalizer, &data);
    if (data == NULL) {
      gctools::Tagged* new_data = reinterpret_cast<gctools::Tagged*>(ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(core::Cons_O*)));
      data = (void*)new_data;
    }
    // Write the finalizers list into the UNCOLLECTABLE space
    *(reinterpret_cast<gctools::Tagged*>(data)) = finalizers.tagged_();
    ASSERT(!orig_finalizer || (orig_finalizer == boehm_cons_finalizer));
    GC_register_finalizer_no_order(base, boehm_cons_finalizer, data, &dummy_finalizer, &dummy_data);
  }
}

/*! Clear the list of finalizers associated with object.
 */
void clear_finalizer_list(core::T_sp object) {
  // With the Boehm GC we keep track of the finalizer list in the data pointer that is passed to the finalizer
  // we need to now check if that data pointer is defined and if so - free the UNCOLLECTABLE memory
  // We will keep the old finalizer function because it might be the one that gcalloc.h installed which
  // needs to call the object destructor.  The non-destructor finalizer does nothing when data == NULL
  if (object.generalp()) {
    BoehmFinalizerFn orig_finalizer;
    void* data;
    void* base = SmartPtrToBasePtr(object);
    GC_register_finalizer_no_order(base, NULL, NULL, &orig_finalizer, &data);
    GC_register_finalizer_no_order(base, orig_finalizer, NULL, NULL, NULL);
    if (data != NULL) GC_free(data);
  } else if (object.consp()) {
    BoehmFinalizerFn orig_finalizer;
    void* data;
    void* base = (void*)gctools::ConsPtrToHeaderPtr(&*object);
    GC_register_finalizer_no_order(base, NULL, NULL, &orig_finalizer, &data);
    GC_register_finalizer_no_order(base, orig_finalizer, NULL, NULL, NULL);
    if (data != NULL) GC_free(data);
  }
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

  gctools::ThreadLocalStateLowLevel* thread_local_state_low_level = new gctools::ThreadLocalStateLowLevel(claspInfo);
  my_thread_low_level = thread_local_state_low_level;

  // ctor sets up my_thread
  my_thread =
    (core::ThreadLocalState*)ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(core::ThreadLocalState));
  new (my_thread) core::ThreadLocalState(false);

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

size_t heap_size() { return GC_get_heap_size(); }
size_t free_bytes() { return GC_get_free_bytes(); }
size_t bytes_since_gc() { return GC_get_bytes_since_gc(); }

void* call_with_stopped_world(void* (*f)(void*), void* data) {
  return GC_call_with_alloc_lock(f, data);
}

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
