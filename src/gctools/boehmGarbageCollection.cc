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
//#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gctoolsPackage.h>
#ifdef USE_BOEHM // whole file #ifdef USE_BOEHM
#include <clasp/gctools/boehmGarbageCollection.h>
#include <clasp/core/debugger.h>

namespace gctools {



void clasp_warn_proc(char *msg, GC_word arg) {
  printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__);
  printf(msg, arg);
}



void run_finalizers(core::T_sp obj, void* data)
{
  // The _sym_STARfinalizersSTAR weak-key-hash-table will be useless at this point
  // because Boehm wipes out the entry before the finalizer is called - but we have
  // a backup plan.  The data pointer may be NULL but otherwise points to a tagged pointer in the
  // GC UNCOLLECTABLE memory that contains the list of finalizers that we would otherwise have
  // looked up in the *finalizers* weak-key-hash-table
  // if data is NULL then there are no finalizers to run
  if (data == NULL) {
//    printf("%s:%d There were no finalizers to run for %p\n", __FILE__, __LINE__, (void*)obj.tagged_());
    return;
  }
  gctools::Tagged finalizers_tagged = *reinterpret_cast<gctools::Tagged*>(data);
  core::List_sp finalizers = core::T_sp(finalizers_tagged);
//  printf("%s:%d  looked up finalizer list for %p length -> %d  list head -> %p\n", __FILE__, __LINE__, (void*)obj.tagged_(), core::cl__length(finalizers), (void*)finalizers.tagged_());
  for ( auto cur : finalizers ) {
    core::T_sp func = oCar(cur);
    core::eval::funcall(func,obj);
  }
  // Now release the memory pointed to by data
  GC_FREE(data);
}

void boehm_general_finalizer(void* base, void* data)
{
//  printf("%s:%d general_finalizer for %p\n", __FILE__, __LINE__, (void*)base);
  void* object = BasePtrToMostDerivedPtr<core::T_O>(base);
  core::T_sp obj((core::T_O*)object);
//  printf("%s:%d cons_finalizer for tagged General -> %p\n", __FILE__, __LINE__, (void*)obj.tagged_());
  run_finalizers(obj,data);
}

void boehm_cons_finalizer(void* base, void* data)
{
  void* object = base; // no header on Cons_O
  core::Cons_sp obj((core::Cons_O*)object);
//  printf("%s:%d cons_finalizer for tagged Cons -> %p\n", __FILE__, __LINE__, (void*)obj.tagged_());
  run_finalizers(obj,data);
}


/*! Register a list of finalizers to run when object is finalized.
    This function can be called multiple times - it replaces any previous
    list of finalizers with this new one.
*/
void boehm_set_finalizer_list(gctools::Tagged object_tagged, gctools::Tagged finalizers_tagged )
{
  core::T_sp object(object_tagged);
  core::List_sp finalizers = core::T_sp(finalizers_tagged);
  // First check if there is already a finalizer and data.
  // The data may be NULL  - that means we need to allocate a pointer in the UNCOLLECTABLE memory to keep a
  // linked list of Common Lisp finalizers alive (the weak-key-hash-table above won't keep anything
  // alive because Boehm wipes out the weak-key-hash-table entry before the finalizers get called).
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
    GC_register_finalizer_no_order(base,(GC_finalization_proc)0, 0,&orig_finalizer,&data);
//    printf("%s:%d base = %p orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, base, (void*)orig_finalizer, (void*)data);
    if (data==NULL) {
      gctools::Tagged* new_data = reinterpret_cast<gctools::Tagged*>(GC_MALLOC_UNCOLLECTABLE(sizeof(core::Cons_O*)));
      data = (void*)new_data;
//      printf("%s:%d allocated uncollectable data=%p\n", __FILE__, __LINE__, (void*)data);
    }
    // Write the finalizers list into the UNCOLLECTABLE space
    *(reinterpret_cast<gctools::Tagged*>(data)) = finalizers.tagged_();
    if (orig_finalizer) {
        // There was already a finalizer - restore it
      GC_register_finalizer_no_order(base,orig_finalizer,data,&dummy_finalizer,&dummy_data);
//      printf("%s:%d base = %p set orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, base, (void*)orig_finalizer, (void*)data);
    } else {
      //There was no finalizer - give it the boehm_general_finalizer
      GC_register_finalizer_no_order(base,boehm_general_finalizer,data,&dummy_finalizer, &dummy_data);
//      printf("%s:%d base = %p set boehm_general_finalizer=%p  data=%p\n", __FILE__, __LINE__, base, (void*)boehm_general_finalizer, (void*)data);
    }
  } else if (object.consp()) {
    GC_finalization_proc orig_finalizer, dummy_finalizer;
    void* data;
    void* dummy_data;
    void* base = (void*)&*object;
    GC_register_finalizer_no_order(base,(GC_finalization_proc)0, 0,&orig_finalizer,&data);
//    printf("%s:%d base = %p orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, base, (void*)orig_finalizer, (void*)data);
    if (data==NULL) {
      gctools::Tagged* new_data = reinterpret_cast<gctools::Tagged*>(GC_MALLOC_UNCOLLECTABLE(sizeof(core::Cons_O*)));
      data = (void*)new_data;
//      printf("%s:%d allocated uncollectable data=%p\n", __FILE__, __LINE__, (void*)data);
    }
    // Write the finalizers list into the UNCOLLECTABLE space
    *(reinterpret_cast<gctools::Tagged*>(data)) = finalizers.tagged_();
    GC_register_finalizer_no_order(base,boehm_cons_finalizer,data,&dummy_finalizer,&dummy_data);
//    printf("%s:%d base = %p set boehm_cons_finalizer=%p  data=%p  dummy_finalizer=%p  dummy_data = %p\n", __FILE__, __LINE__, base, (void*)boehm_cons_finalizer, (void*)data, dummy_finalizer, dummy_data);
  }
}


/*! Clear the list of finalizers associated with object.
*/
void boehm_clear_finalizer_list(gctools::Tagged object_tagged)
{
  core::T_sp object(object_tagged);
    // With the Boehm GC we keep track of the finalizer list in the data pointer that is passed to the finalizer
  // we need to now check if that data pointer is defined and if so - free the UNCOLLECTABLE memory
  // We will keep the old finalizer function because it might be the one that gcalloc.h installed which
  // needs to call the object destructor.  The non-destructor finalizer does nothing when data == NULL
  if ( object.generalp() ) {
    BoehmFinalizerFn orig_finalizer;
    void* data;
    void* base = SmartPtrToBasePtr(object);
    GC_register_finalizer_no_order(base,NULL,NULL,&orig_finalizer,&data);
//    printf("%s:%d orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, (void*)orig_finalizer, (void*)data);
    if ( data != NULL ) {
      gctools::Tagged list_tagged = *reinterpret_cast<gctools::Tagged*>(data);
//      printf("%s:%d definalize - wiping out the finalizer list in the UNCOLLECTABLE memory -> %p\n", __FILE__, __LINE__, (void*)list_tagged);
      GC_free(data);
      data = NULL;
    }
    GC_register_finalizer_no_order(base,orig_finalizer,NULL,NULL,NULL);
  } else if (object.consp()) {
    BoehmFinalizerFn orig_finalizer;
    void* data;
    void* base = (void*)(&*object);
    GC_register_finalizer_no_order(base,NULL,NULL,&orig_finalizer,&data);
//    printf("%s:%d orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, (void*)orig_finalizer, (void*)data);
    if ( data != NULL ) {
      gctools::Tagged list_tagged = *reinterpret_cast<gctools::Tagged*>(data);
//      printf("%s:%d definalize - wiping out the finalizer list in the UNCOLLECTABLE memory -> %p\n", __FILE__, __LINE__, (void*)list_tagged);
      GC_free(data);
      data = NULL;
    }
    GC_register_finalizer_no_order(base,orig_finalizer,NULL,NULL,NULL);
  }
}












#ifdef USE_BOEHM_MEMORY_MARKER
  int globalBoehmMarker = 0;
#endif

};

namespace gctools  {
#if 0
// This doesn't work because Boehm has an arbitrary 2048 limit on the number
// of root sets
void boehm_register_roots(void* root_address, size_t num_roots)
{
  void* high_address_plus_1 = reinterpret_cast<void*>(reinterpret_cast<char*>(root_address)+num_roots*sizeof(core::T_sp) + 1);
  GC_add_roots(root_address,high_address_plus_1);
}
#endif

void* boehm_create_shadow_table(size_t nargs)
{
  // Boehm uses a shadow table in the UNCOLLECTABLE space
  // Allocate the table and fill it with UNBOUND
  core::T_sp* shadow_mem = reinterpret_cast<core::T_sp*>(GC_MALLOC_UNCOLLECTABLE(nargs*sizeof(core::T_sp)));
  for (size_t ii(0); ii<nargs; ++ii) {
    shadow_mem[ii] = _Unbound<core::T_O>();
  }
  return reinterpret_cast<void*>(shadow_mem);
};

};

namespace gctools {
__attribute__((noinline))
int initializeBoehm(MainFunctionType startupFn, int argc, char *argv[], bool mpiEnabled, int mpiRank, int mpiSize) {
  GC_INIT();
  GC_allow_register_threads();
  GC_set_java_finalization(1);
//  GC_allow_register_threads();
  GC_set_all_interior_pointers(1); // tagged pointers require this
                                   //printf("%s:%d Turning on interior pointers\n",__FILE__,__LINE__);
  GC_set_warn_proc(clasp_warn_proc);
  //  GC_enable_incremental();
  GC_init();
  void* topOfStack;
  // ctor sets up my_thread
  core::ThreadLocalState thread_local_state(&topOfStack);
  my_thread = &thread_local_state;
#if 0
  // I'm not sure if this needs to be done for the main thread
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#endif

#ifndef SCRAPING
#define ALL_PREGCSTARTUPS_CALLS
#include PREGCSTARTUP_INC_H
#undef ALL_PREGCSTARTUPS_CALLS
#endif
  
  int exitCode = startupFn(argc, argv, mpiEnabled, mpiRank, mpiSize);
#if 0
  GC_unregister_my_thread();
#endif
  return exitCode;
}



};
#endif // whole file #ifdef USE_BOEHM
