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
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/debugger.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/compiler.h>
#include <clasp/gctools/snapshotSaveLoad.h>

#ifdef USE_PRECISE_GC
#include <gc/gc_mark.h>
#endif // USE_PRECISE_GC


extern "C" {
void boehm_park() {
//  printf("%s:%s:%d Do something to turn off the boehm gc\n", __FILE__, __FUNCTION__, __LINE__ );
  GC_disable();
};

void boehm_release() {
//  printf("%s:%s:%d Do something to turn on the boehm gc\n", __FILE__, __FUNCTION__, __LINE__ );
  GC_enable();
}
};


static gctools::ReachableClassMap *static_ReachableClassKinds;
static size_t invalidHeaderTotalSize = 0;

extern "C" {
void callback_reachable_object( gctools::Header_s* ptr, void *client_data) {
  gctools::Header_s *h = reinterpret_cast<gctools::Header_s *>(ptr);
  gctools::GCStampEnum stamp = h->_stamp_wtag_mtag.stamp_();
  if (!valid_stamp(stamp)) {
    printf("%s:%d:%s Invalid stamp\n", __FILE__, __LINE__, __FUNCTION__ );
#if 0
    if (h->_stamp_wtag_mtag.consP()) {
      stamp = (gctools::GCStampEnum)(gctools::STAMPWTAG_core__Cons_O>>gctools::Header_s::wtag_width);
//      printf("%s:%d cons stamp address: %p sz: %lu stamp: %lu\n", __FILE__, __LINE__, (void*)h, sz, stamp);
    } else {
      stamp = (gctools::GCStampEnum)0; // unknown uses 0
    }
#endif
  }
  size_t sz = objectSize(h);
  gctools::ReachableClassMap::iterator it = static_ReachableClassKinds->find(stamp);
  if (it == static_ReachableClassKinds->end()) {
    gctools::ReachableClass reachableClass(stamp);
    reachableClass.update(sz);
    (*static_ReachableClassKinds)[stamp] = reachableClass;
  } else {
    it->second.update(sz);
  }
#if 0
  if (stamp==(gctools::GCStampEnum)(gctools::STAMPWTAG_core__Symbol_O>>gctools::Header_s::wtag_width)) {
    core::Symbol_O* sym = (core::Symbol_O*)ptr;
    printf("%s:%d symbol %s\n", __FILE__, __LINE__, sym->formattedName(true).c_str());
  }
#endif
}

void boehm_callback_reachable_object_find_stamps(void *ptr, size_t sz, void *client_data) {
  gctools::FindStamp* findStamp = (gctools::FindStamp*)client_data;
  gctools::Header_s *h = reinterpret_cast<gctools::Header_s *>(ptr);
  gctools::GCStampEnum stamp = h->_stamp_wtag_mtag.stamp_();
  if (!valid_stamp(stamp)) {
    if (sz==32) {
      stamp = (gctools::GCStampEnum)(gctools::STAMPWTAG_core__Cons_O>>gctools::Header_s::wtag_width);
//      printf("%s:%d cons stamp address: %p sz: %lu stamp: %lu\n", __FILE__, __LINE__, (void*)h, sz, stamp);
    } else {
      stamp = (gctools::GCStampEnum)0; // unknown uses 0
    }
  }
  if (stamp == findStamp->_stamp) {
    findStamp->_addresses.push_back(ptr);
  }
}



void boehm_callback_reachable_object_find_owners(void *ptr, size_t sz, void *client_data) {
  gctools::FindOwner* findOwner = (gctools::FindOwner*)client_data;
  for ( void** cur = (void**)ptr ; cur < (void**)((void**)ptr+(sz/8)); cur += 1 ) {
    void* tp = *cur;
    uintptr_t tag = (uintptr_t)tp&0xf;
    if (GC_is_heap_ptr(tp) && (tag == GENERAL_TAG || tag == CONS_TAG)) {
      void* obj = gctools::untag_object(tp);
      uintptr_t addr = (uintptr_t)obj;
      void* base = gctools::GeneralPtrToHeaderPtr(obj);
#if 0
      printf("%s:%d Looking at cur->%p\n", __FILE__, __LINE__, cur);
      printf("%s:%d             tp->%p\n", __FILE__, __LINE__, tp);
      printf("%s:%d           base->%p\n", __FILE__, __LINE__, base);
      printf("%s:%d        pointer->%p\n", __FILE__, __LINE__, findOwner->_pointer);
#endif
      if (base == findOwner->_pointer ) {
        uintptr_t* uptr = (uintptr_t*)ptr;
#ifdef USE_BOEHM
        printf("%p  %s\n", ptr, obj_name((*uptr)>>4));
#endif
        findOwner->_addresses.push_back((void*)uptr);
        }
      }
    }
  }
}


template <typename T>
size_t dumpResults(const std::string &name, const std::string &shortName, T *data) {
  typedef typename T::value_type::second_type value_type;
  vector<value_type> values;
  for (auto it : *data) {
    values.push_back(it.second);
  }
  size_t totalSize(0);
  sort(values.begin(), values.end(), [](const value_type &x, const value_type &y) {
                                       return (x.totalSize > y.totalSize);
                                     });
  size_t idx = 0;
  size_t totalCons = 0;
  size_t numCons = 0;
  for (auto it : values) {
    // Does that print? If so should go to the OutputStream
    size_t sz = it.print(shortName);
    totalSize += sz;
    if (sz < 1) break;
    idx += 1;
#if 0
    if ( idx % 100 == 0 ) {
      gctools::poll_signals();
    }
#endif
  }
  return totalSize;
}



void* walk_garbage_collected_objects_with_alloc_lock(void* client_data) {
  gctools::GatherObjects gather;
  gatherAllObjects( gather );
  for ( auto header : gather._Marked ) {
    callback_reachable_object( header, client_data );
  }
  return NULL;
}


// ------------






namespace gctools {



void clasp_warn_proc(char *msg, GC_word arg) {
#if 0
  // This warning is annoying and comes up a lot but doesn't cause problems except for when building ironclad
  printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__);
  printf(msg, arg);
#endif
}



void run_finalizers(core::T_sp obj, void* data)
{
  // The _sym_STARfinalizersSTAR weak-key-hash-table will be useless at this point
  // because Boehm wipes out the entry before the finalizer is called - but we have
  // a backup plan.  The data pointer may be NULL but otherwise points to a tagged pointer in the
  // GC UNCOLLECTABLE memory that contains the list of finalizers that we would otherwise have
  // looked up in the *finalizers* weak-key-hash-table
  // if data is NULL then there are no finalizers to run
  //
  // The GCInfo NeedsFinalization = true will call in here and if data is NULL
  // then we want to immediately return.
  if (data == NULL) return;
  gctools::Tagged finalizers_tagged = *reinterpret_cast<gctools::Tagged*>(data);
  core::List_sp finalizers = core::T_sp(finalizers_tagged);
//  printf("%s:%d  looked up finalizer list for %p length -> %lu  list head -> %p\n", __FILE__, __LINE__, (void*)obj.tagged_(), core::cl__length(finalizers), (void*)finalizers.tagged_());
  for ( auto cur : finalizers ) {
    core::T_sp func = oCar(cur);
    if (((uintptr_t)obj.raw_() & ptag_mask) == 0) {
      printf("%s:%d   The obj %p must be tagged\n", __FILE__, __LINE__, obj.raw_());
      abort();
    }
//    printf("%s:%d     calling finalizer %p with obj %p\n", __FILE__, __LINE__, func.raw_(), obj.raw_());
    core::eval::funcall(func,obj);
  }
  // Now release the memory pointed to by data
  GC_FREE(data);
}

void boehm_general_finalizer_from_BoehmFinalizer(void* client, void* data)
{
  gctools::Header_s* header = (gctools::Header_s*)((char*)client - sizeof(gctools::Header_s));
//  printf("%s:%d:%s for client: %p stamp: %lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)client, header->_stamp_wtag_mtag.stamp());
  if ((uintptr_t)client&gctools::ptag_mask) {
    printf("%s:%d The client pointer %p must NOT BE TAGGED at this point\n", __FILE__, __LINE__, client);
    abort();
  }
  core::T_sp obj((gctools::Tagged)tag_general((core::T_O*)client));
  if (data!=NULL) {
    run_finalizers(obj,data);
  }
}

void boehm_general_finalizer(void* base, void* data)
{
//  printf("%s:%d:%s general_finalizer for %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)base);
  void* client = HeaderPtrToGeneralPtr<core::T_O>(base);
  if ((uintptr_t)client&gctools::ptag_mask) {
    printf("%s:%d The client pointer %p must NOT BE TAGGED at this point\n", __FILE__, __LINE__, client);
    abort();
  }
  core::T_sp obj((gctools::Tagged)tag_general((core::T_O*)client));
  if (data!=NULL) {
    run_finalizers(obj,data);
  } else {
//    printf("%s:%d:%s  data was NULL and so could not run finalizers\n", __FILE__, __LINE__, __FUNCTION__ );
  }
}

void boehm_cons_finalizer(void* base, void* data)
{
  ASSERT(CONS_HEADER_SIZE==0);
  if (data!=NULL) {
    void* client = gctools::HeaderPtrToConsPtr(base);
    core::Cons_sp obj((core::Cons_O*)client);
//    printf("%s:%d boehm_cons_finalizer for tagged Cons -> %p  client -> %p\n", __FILE__, __LINE__, (void*)obj.tagged_(), client );
    run_finalizers(obj,data);
  } else {
    printf("%s:%d:%s  data was NULL and so could not run finalizers\n", __FILE__, __LINE__, __FUNCTION__ );
  }
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
    GC_register_finalizer_no_order(base,(GC_finalization_proc)0, 0,&orig_finalizer,&data);
//    printf("%s:%d base = %p orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, base, (void*)orig_finalizer, (void*)data);
    if (data==NULL) {
      gctools::Tagged* new_data = reinterpret_cast<gctools::Tagged*>(ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(core::Cons_O*)));
//      printf("%s:%d:%s  finalizers1 allocate: %p\n", __FILE__, __LINE__, __FUNCTION__, new_data );
      data = (void*)new_data;
//      printf("%s:%d allocated uncollectable data=%p\n", __FILE__, __LINE__, (void*)data);
    }
    // Write the finalizers list into the UNCOLLECTABLE space
    *(reinterpret_cast<gctools::Tagged*>(data)) = finalizers.tagged_();
    if (orig_finalizer) {
        // If there is already a finalizer function then it will be the BoehmFinalizer<CLASS>
        // This will call the boehm_general_finalizer and then call the dtor for the class
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
    ASSERT(CONS_HEADER_SIZE==0);
    void* base = (void*)gctools::ConsPtrToHeaderPtr(&*object); 
    GC_register_finalizer_no_order(base,(GC_finalization_proc)0, 0,&orig_finalizer,&data);
//    printf("%s:%d object -> %p base = %p orig_finalizer=%p  data=%p\n", __FILE__, __LINE__, object.raw_(), base, (void*)orig_finalizer, (void*)data);
    if (data==NULL) {
      gctools::Tagged* new_data = reinterpret_cast<gctools::Tagged*>(ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(core::Cons_O*)));
//      printf("%s:%d:%s  finalizers2 allocate: %p\n", __FILE__, __LINE__, __FUNCTION__, new_data );
      data = (void*)new_data;
//      printf("%s:%d allocated uncollectable data=%p\n", __FILE__, __LINE__, (void*)data);
    }
    // Write the finalizers list into the UNCOLLECTABLE space
    *(reinterpret_cast<gctools::Tagged*>(data)) = finalizers.tagged_();
    ASSERT(!orig_finalizer||(orig_finalizer==boehm_cons_finalizer));
    GC_register_finalizer_no_order(base,boehm_cons_finalizer,data,&dummy_finalizer,&dummy_data);
//    printf("%s:%d base = %p set boehm_cons_finalizer=%p  data=%p  dummy_finalizer=%p  dummy_data = %p\n", __FILE__, __LINE__, base, (void*)boehm_cons_finalizer, (void*)data, dummy_finalizer, dummy_data);
  }
}


/*! Clear the list of finalizers associated with object.
*/
void boehm_clear_finalizer_list(gctools::Tagged object_tagged)
{
//  printf("%s:%d:%s  obj -> %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)object_tagged);
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
    if ( data != NULL ) {
      gctools::Tagged list_tagged = *reinterpret_cast<gctools::Tagged*>(data);
      GC_free(data);
      data = NULL;
    }
    GC_register_finalizer_no_order(base,orig_finalizer,NULL,NULL,NULL);
  } else if (object.consp()) {
    BoehmFinalizerFn orig_finalizer;
    void* data;
    void* base = (void*)gctools::ConsPtrToHeaderPtr(&*object);
    GC_register_finalizer_no_order(base,NULL,NULL,&orig_finalizer,&data);
    if ( data != NULL ) {
      gctools::Tagged list_tagged = *reinterpret_cast<gctools::Tagged*>(data);
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

namespace gctools {
__attribute__((noinline))
int initializeBoehm(MainFunctionType startupFn, int argc, char *argv[], bool mpiEnabled, int mpiRank, int mpiSize) {
  GC_set_handle_fork(1);
  GC_INIT();
  GC_allow_register_threads();
  GC_set_java_finalization(1);
  GC_set_all_interior_pointers(1); // tagged pointers require this
                                   //printf("%s:%d Turning on interior pointers\n",__FILE__,__LINE__);
  GC_set_warn_proc(clasp_warn_proc);
  //  GC_enable_incremental();
  GC_init();
  void* topOfStack;
  // ctor sets up my_thread
  gctools::ThreadLocalStateLowLevel thread_local_state_low_level(&topOfStack);
  core::ThreadLocalState thread_local_state(false); // special ctor that does not require _Nil be defined
  my_thread_low_level = &thread_local_state_low_level;
  my_thread = &thread_local_state;
  core::transfer_StartupInfo_to_my_thread();
#if 1
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


  //
  // Set up the _lisp and symbols memory as roots
  //
  gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*)&_lisp;
  GC_add_roots((void*)lispRoot,(void*)((char*)lispRoot+sizeof(void*)));
//  gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*)&global_core_symbols[0];
//  GC_add_roots((void*)coreSymbolRoots,(void*)((char*)coreSymbolRoots+sizeof(void*)*NUMBER_OF_CORE_SYMBOLS));
  gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*)&global_symbols[0];
  GC_add_roots((void*)symbolRoots,(void*)((char*)symbolRoots+sizeof(void*)*global_symbol_count));

//  void* dummy = GC_generic_malloc_uncollectable(592,4);
  
  int exitCode;
  try {
    exitCode = startupFn(argc, argv, mpiEnabled, mpiRank, mpiSize);
  } catch (core::SaveLispAndDie& ee) {
#ifdef USE_PRECISE_GC
    snapshotSaveLoad::snapshot_save(ee);
#endif
    exitCode = 0;
  }
    
#if 0
  GC_unregister_my_thread();
#endif
  return exitCode;
}


/* Walk all of the roots, passing the address of each root and what it represents */
void walkRoots( RootWalkCallback callback, void* data ) {
  callback( (Tagged*)&_lisp, LispRoot, 0, data);
#if 0
  for ( size_t ii=0; ii<NUMBER_OF_CORE_SYMBOLS; ++ii ) {
    callback( (Tagged*)&global_core_symbols[ii], CoreSymbolRoot, ii, data );
  }
#endif
  for ( size_t jj=0; jj<global_symbol_count; ++jj ) {
    callback( (Tagged*)&global_symbols[jj], SymbolRoot, jj, data );
  }
};



size_t ReachableClass::print(const std::string& shortName) {
  Fixnum k = this->_Kind;
  stringstream className;
  if (k <= gctools::STAMPWTAG_max) {
//      printf("%s:%d searching for name for this->_Kind: %u\n", __FILE__, __LINE__, this->_Kind);
    const char* nm = obj_name(k);
    if (!nm) {
      className << "NULL-NAME";
    } else {
      className << nm;
    }
    clasp_write_string((BF("%s: total_size: %10d count: %8d avg.sz: %8d kind: %s/%d\n")
                        % shortName % this->totalSize % this->instances
                        % (this->totalSize / this->instances) % className.str().c_str() % k).str()
                       , cl::_sym_STARstandard_outputSTAR->symbolValue());
    core::clasp_finish_output_t();
    return this->totalSize;
  } else {
    clasp_write_string((BF("%s: total_size: %10d count: %8d avg.sz: %8d kind: %s/%d\n")
                        % shortName % this->totalSize % this->instances
                        % (this->totalSize / this->instances) % "UNKNOWN" % k).str()
                       ,cl::_sym_STARstandard_outputSTAR->symbolValue());
    core::clasp_finish_output_t();
    return this->totalSize;
  }
}

void clasp_gc_room(std::ostringstream& OutputStream) {
  static_ReachableClassKinds = new (ReachableClassMap);
  invalidHeaderTotalSize = 0;
  GC_call_with_alloc_lock( walk_garbage_collected_objects_with_alloc_lock, NULL );
  OutputStream << "Walked LispKinds\n" ;
  size_t totalSize(0);
  OutputStream << "-------------------- Reachable ClassKinds -------------------\n"; 
  totalSize += dumpResults("Reachable ClassKinds", "class", static_ReachableClassKinds);
  OutputStream << "Done walk of memory  " << static_cast<uintptr_t>(static_ReachableClassKinds->size()) << " ClassKinds\n";
  OutputStream << "Total invalidHeaderTotalSize = " << std::setw(12) << invalidHeaderTotalSize << '\n';
  OutputStream << "Total memory usage (bytes):    " << std::setw(12) << totalSize << '\n';
  OutputStream << "Total GC_get_heap_size()       " << std::setw(12) << GC_get_heap_size() << '\n';
  OutputStream << "Total GC_get_free_bytes()      " << std::setw(12) << GC_get_free_bytes() << '\n';
  OutputStream << "Total GC_get_bytes_since_gc()  " <<  std::setw(12) << GC_get_bytes_since_gc() << '\n';
  OutputStream << "Total GC_get_total_bytes()     " <<  std::setw(12) << GC_get_total_bytes() << '\n';

  delete static_ReachableClassKinds;
}


};
#endif // whole file #ifdef USE_BOEHM
