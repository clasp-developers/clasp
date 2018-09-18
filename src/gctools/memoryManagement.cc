/*
    File: memoryManagement.cc
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
//

#include <clasp/core/foundation.h>
#include <clasp/gctools/gcalloc.h>
#include <clasp/core/object.h>
#include <clasp/core/bformat.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/debugger.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/mpPackage.h>
//#include "main/allHeaders.cc"

#ifdef _TARGET_OS_LINUX
#include <signal.h>
#endif
#ifndef SCRAPING
#define ALL_PREGCSTARTUPS_EXTERN
#include PREGCSTARTUP_INC_H
#undef ALL_PREGCSTARTUPS_EXTERN
#endif

extern "C" {
__attribute__((noinline)) void HitAllocationSizeThreshold() {
  my_thread_low_level->_Allocations._HitAllocationSizeCounter++;
}

__attribute__((noinline)) void HitAllocationNumberThreshold() {
  my_thread_low_level->_Allocations._HitAllocationNumberCounter++;
}
}




#include <clasp/core/scrape.h>


////////////////////////////////////////////////////////////
//
// GC_MANAGED_TYPE
//
// Objects that are managed by the GC and need a stamp
//   but are not directly accessible to Common Lisp
GC_MANAGED_TYPE(core::Lisp_O);
GC_MANAGED_TYPE(clbind::detail::class_map);
GC_MANAGED_TYPE(gctools::GCArray_moveable<double>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<float>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<int>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<long>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<short>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<signed char>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<unsigned char>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<unsigned int>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<unsigned long>);
GC_MANAGED_TYPE(gctools::GCArray_moveable<unsigned short>);
GC_MANAGED_TYPE(gctools::GCBitUnitArray_moveable<1,unsigned int,int>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::AuxArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::CacheRecord>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::DynamicBinding>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::ExceptionEntry>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::KeywordArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::OptionalArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::RequiredArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::SymbolClassPair>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::SymbolStorage>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::T_O *>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<double>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<float>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<int>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>);


namespace gctools {
void lisp_increment_recursive_allocation_counter(core::ThreadLocalState* thread)
{
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  int x = thread->_RecursiveAllocationCounter+1;
  thread->_RecursiveAllocationCounter = x;
  if (x!=1) {
    printf("%s:%d A recursive allocation took place - these are illegal!!!!\n", __FILE__, __LINE__ );
    abort();
  }
#endif
}
void lisp_decrement_recursive_allocation_counter(core::ThreadLocalState* thread)
{
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  --thread->_RecursiveAllocationCounter;
#endif
};

};


namespace gctools {
#if 0
AllocationRecord* allocation_backtrace(size_t kind, uintptr_clasp_t stamp, size_t size, AllocationRecord* prev) {
// Play with Unix backtrace(3)
#define BACKTRACE_SIZE 1024
  void *buffer[BACKTRACE_SIZE];
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  int nptrs;
  nptrs = backtrace(buffer, BACKTRACE_SIZE);
  char **strings = backtrace_symbols(buffer, nptrs);
  AllocationRecord* record = new BacktraceRecord(strings,nptrs,kind,stamp,size,prev);
  return record;
};
#endif
};


namespace gctools {

void register_thread(mp::Process_sp process, void* stack_base) {
#ifdef USE_BOEHM
  // ----   Boehm stuff needs to be done in the thread function
//  GC_stack_base gc_stack_base;
//  GC_get_stack_base(&gc_stack_base);
//  GC_register_my_thread(&gc_stack_base);
#endif
#ifdef USE_MPS
  my_mps_thread_reg(&process->thr_o);
#endif
};

void unregister_thread(mp::Process_sp process) {
#ifdef USE_BOEHM
  // ----   Boehm stuff needs to be done in the thread function
//  GC_unregister_my_thread();
#endif
#ifdef USE_MPS
  my_mps_thread_deref(process->thr_o);
//  printf("%s:%d  add support to add threads for MPS\n", __FILE__, __LINE__ );
#endif
};

};


namespace gctools {


char *clasp_alloc_atomic(size_t buffer) {
  return (char *)malloc(buffer);
}

void clasp_dealloc(char* buffer) {
  if (buffer) {
    free(buffer);
  }
}


};


namespace gctools {
void rawHeaderDescribe(const uintptr_clasp_t *headerP) {
  uintptr_clasp_t headerTag = (*headerP) & Header_s::tag_mask;
  switch (headerTag) {
  case 0:
      printf("  %p : %" Puintptr_clasp_t "(%p) %" Puintptr_clasp_t "(%p)\n", headerP, *headerP, (void*)*headerP, *(headerP + 1), (void*)*(headerP + 1));
      printf(" Not an object header!\n");
      break;
  case Header_s::stamp_tag: {
    printf("  %p : %" Puintptr_clasp_t " (%p)\n", headerP, *headerP, (void*)*headerP);
    printf("  %p : %" Puintptr_clasp_t " (%p)\n", (headerP+1), *(headerP+1), (void*)*(headerP+1));
#ifdef DEBUG_GUARD
    printf("  %p : %p\n", (headerP+2), (void*)*(headerP+2));
    printf("  %p : %p\n", (headerP+3), (void*)*(headerP+3));
    printf("  %p : %p\n", (headerP+4), (void*)*(headerP+4));
    printf("  %p : %p\n", (headerP+5), (void*)*(headerP+5));
#endif    
    GCStampEnum kind = (GCStampEnum)((*((Header_s*)headerP)).stamp());
    printf(" stamp tag - stamp: %d", kind);
    fflush(stdout);
    printf("     %s\n", obj_name(kind));
  } break;
  case Header_s::fwd_tag: {
    Header_s *hdr = (Header_s *)headerP;
    printf("  0x%p : 0x%" Puintptr_clasp_t " 0x%" Puintptr_clasp_t "\n", headerP, *headerP, *(headerP + 1));
    printf(" fwd_tag - fwd address: 0x%" Puintptr_clasp_t "\n", (*headerP) & Header_s::fwd_ptr_mask);
    printf("     fwdSize = %" Puintptr_clasp_t "/0x%" Puintptr_clasp_t "\n", hdr->fwdSize(), hdr->fwdSize());
  } break;
  case Header_s::pad_tag:
      printf("  0x%p : 0x%" PRu " 0x%" PRu "\n", headerP, *headerP, *(headerP + 1));
      if (((*headerP) & Header_s::pad1_tag) == Header_s::pad1_tag) {
        printf("   pad1_tag\n");
        printf("  0x%p : 0x%" PRu "\n", headerP, *headerP);
      } else {
        printf("   pad_tag\n");
        printf("  0x%p : 0x%" PRu "\n", headerP, *headerP);
        printf("  0x%p : 0x%" PRu "\n", (headerP+1), *(headerP+1));
      }
      break;
  }
#if DEBUG_GUARD
  Header_s* header = (Header_s*)headerP;
  header->validate();
  printf("This object passed the validate() test\n");
#endif
};
};


extern "C" {
void client_describe(void *taggedClient) {
  if (gctools::tagged_generalp(taggedClient) || gctools::tagged_consp(taggedClient)) {
    printf("%s:%d  GC managed object - describing header\n", __FILE__, __LINE__);
    // Currently this assumes that Conses and General objects share the same header
    // this may not be true in the future
    // conses may be moved into a separate pool and dealt with in a different way
    const uintptr_clasp_t *headerP;
    if (gctools::tagged_generalp(taggedClient)) {
      headerP = reinterpret_cast<const uintptr_clasp_t *>(gctools::ClientPtrToBasePtr(gctools::untag_general(taggedClient)));
    } else {
      headerP = reinterpret_cast<const uintptr_clasp_t *>(gctools::ClientPtrToBasePtr(gctools::untag_cons(taggedClient)));
    }
    gctools::rawHeaderDescribe(headerP);
  } else {
    printf("%s:%d Not a tagged pointer - might be immediate value\n", __FILE__, __LINE__);
    printf("    Trying to interpret as client pointer\n");
    const uintptr_clasp_t* headerP;
    headerP = reinterpret_cast<const uintptr_clasp_t*>(gctools::ClientPtrToBasePtr(taggedClient));
    gctools::rawHeaderDescribe(headerP);
  }
};
};

extern "C" {

void client_validate(core::T_sp client) {
  if (client.generalp()) {
    client_validate_tagged(client.tagged_());
  }
}

void client_validate_General_O_ptr(const core::General_O* client_ptr) {
  const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(gctools::ClientPtrToBasePtr(reinterpret_cast<const void*>(client_ptr)));
  header->validate();
}

void client_validate_tagged(gctools::Tagged taggedClient) {
  if ( gctools::tagged_generalp(taggedClient))
  {
    core::General_O* client = reinterpret_cast<core::General_O*>(gctools::untag_general(taggedClient));
    client_validate_General_O_ptr(client);
  } else if (gctools::tagged_consp(taggedClient)) {
    // Nothing can be done to validate CONSes, they are too compact.
  }    
};



void header_describe(gctools::Header_s* headerP) {
  gctools::rawHeaderDescribe((uintptr_clasp_t*)headerP);
};
};



namespace gctools {
size_t random_tail_size() {
  size_t ts = ((rand() % 8) + 1) * Alignment();
  return ts;
}

void Header_s::signal_invalid_object(const Header_s* header, const char* msg)
{
  printf("%s:%d  Invalidate object with header @ %p message: %s\n", __FILE__, __LINE__, (void*)header, msg);
  abort();
}


void Header_s::validate() const {
  if ( this->header._value == 0 ) signal_invalid_object(this,"header is 0");
  if ( this->invalidP() ) signal_invalid_object(this,"header is invalidP");
  if ( this->stampP() ) {
#ifdef DEBUG_GUARD    
    if ( this->guard != 0xFEEAFEEBDEADBEEF) signal_invalid_object(this,"normal object bad header guard");
#endif
    if ( this->stamp() > global_NextStamp ) signal_invalid_object(this,"normal object bad header stamp");
#ifdef DEBUG_GUARD
    if ( this->_tail_start & 0xffffffffff000000 ) signal_invalid_object(this,"bad tail_start");
    if ( this->_tail_size & 0xffffffffff000000 ) signal_invalid_object(this,"bad tail_size");
    for ( unsigned char *cp=((unsigned char*)(this)+this->_tail_start), 
            *cpEnd((unsigned char*)(this)+this->_tail_start+this->_tail_size); cp < cpEnd; ++cp ) {
      if (*cp!=0xcc) signal_invalid_object(this,"bad tail content");
    }
#endif
  }
#ifdef USE_MPS
#ifdef DEBUG_GUARD
  if ( this->fwdP() ) {
    if ( this->guard != 0xFEEAFEEBDEADBEEF) signal_invalid_object(this,"bad fwdP guard");
    for ( unsigned char *cp=((unsigned char*)(this)+this->_tail_start), 
            *cpEnd((unsigned char*)(this)+this->_tail_start+this->_tail_size); cp < cpEnd; ++cp ) {
      if (*cp!=0xcc) signal_invalid_object(this,"bad tail content");
    }
  }
#endif
#endif
}
};



namespace gctools {

/*! See NextStamp(...) definition in memoryManagement.h.
  global_NextBuiltInStamp starts at STAMP_max+1
  so that it doesn't use any stamps that correspond to KIND values
   assigned by the static analyzer. */
std::atomic<Stamp>   global_NextStamp = ATOMIC_VAR_INIT(STAMP_max+1);

void OutOfStamps() {
    printf("%s:%d Hello future entity!  Congratulations! - you have run clasp long enough to run out of STAMPs - %" PRu " are allowed - change the clasp header layout or add another word for the stamp\n", __FILE__, __LINE__, Header_s::largest_possible_stamp );
    abort();
}

//GCStack _ThreadLocalStack;
const char *_global_stack_marker;
size_t _global_stack_max_size;
/*! Keeps track of the next available header KIND value */
stamp_t global_next_header_stamp = (stamp_t)STAMP_max+1;

#if 0
    HeapRoot* 	rooted_HeapRoots = NULL;
    StackRoot* 	rooted_StackRoots = NULL;
#endif
};

#if 0
#ifdef USE_BOEHM
#include "boehmGarbageCollection.cc"
#endif

#if defined(USE_MPS)
#include "mpsGarbageCollection.cc"
#endif
#endif

namespace gctools {

size_t global_alignup_sizeof_header;

void monitorAllocation(stamp_t k, size_t sz) {
#ifdef DEBUG_MONITOR_ALLOCATIONS  
  if (global_monitorAllocations.counter >= global_monitorAllocations.start && global_monitorAllocations.counter < global_monitorAllocations.end) {
    core::core__clib_backtrace(global_monitorAllocations.backtraceDepth);
  }
  global_monitorAllocations.counter++;
#endif
}


int handleFatalCondition() {
  int exitCode = 0;
  try {
    throw;
  } catch (core::ExitProgramException &ee) {
    // Do nothing
    //            printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
    exitCode = ee.getExitResult();
  } catch (core::TerminateProgramIfBatch &ee) {
    // Do nothing
    printf("Caught TerminateProgramIfBatch in %s:%d\n", __FILE__, __LINE__);
  } catch (core::CatchThrow &ee) {
    _lisp->print(BF("%s:%d Uncaught THROW frame[%s] - this should NEVER happen - the stack should never be unwound unless there is a CATCH clause that matches the THROW") % __FILE__ % __LINE__ % ee.getFrame());
  } catch (core::Unwind &ee) {
    _lisp->print(BF("At %s:%d - Unwind caught frame: %d index: %d") % __FILE__ % __LINE__ % ee.getFrame() % ee.index());
  } catch (HardError &ee) {
    _lisp->print(BF("At %s:%d - HardError caught: %s") % __FILE__ % __LINE__ % ee.message());
  }
#if 0
        catch ( ... )
        {
            _lisp->print(BF("Unknown exception in main - everything should be caught lower down %s:%d") % __FILE__ % __LINE__);
        }
#endif
  return exitCode;
}

stamp_t next_header_kind()
{
    stamp_t next = global_next_header_stamp;
    ++global_next_header_stamp;
    return next;
}

core::Fixnum ensure_fixnum(stamp_t val)
{
  if ( val > most_positive_fixnum || val < most_negative_fixnum ) {
    SIMPLE_ERROR(BF("The value %d cannot be converted into a FIXNUM") % val );
  }
  return (core::Fixnum)val;
}

CL_LAMBDA();
CL_DOCSTRING(R"doc(Return the next available header KIND value and increment the global variable global_next_header_stamp)doc");
CL_DEFUN core::Fixnum gctools__next_header_kind()
{
    stamp_t next = global_next_header_stamp;
    ++global_next_header_stamp;
    return ensure_fixnum(next);
}

std::atomic<uint64_t> global_TotalRootTableSize;
std::atomic<uint64_t> global_NumberOfRootTables;


/*! initial_data is a gctools::Tagged pointer to a List of tagged pointers.
*/
void initialize_gcroots_in_module(GCRootsInModule* roots, core::T_O** root_address, size_t num_roots, gctools::Tagged initial_data) {
  global_TotalRootTableSize += num_roots;
  global_NumberOfRootTables++;
  core::T_O** shadow_mem = NULL;
#ifdef USE_BOEHM
  shadow_mem = reinterpret_cast<core::T_O**>(boehm_create_shadow_table(num_roots));
#endif
  // Get the address of the memory space in the llvm::Module
  uintptr_clasp_t address = reinterpret_cast<uintptr_clasp_t>(root_address);
  core::T_O** module_mem = reinterpret_cast<core::T_O**>(address);
//  printf("%s:%d:%s address=%p nargs=%" PRu "\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, nargs);
//  printf("%s:%d:%s constants-table contents: vvvvv\n", __FILE__, __LINE__, __FUNCTION__ );
  // Create a GCRootsInModule structure to write the constants with
  // FIXME: The GCRootsInModule is on the stack - once it's gone we loose the ability
  //        to keep track of the constants and in the future when we start GCing code
  //        we need to keep track of the constants.
  new (roots) GCRootsInModule(reinterpret_cast<void*>(shadow_mem),reinterpret_cast<void*>(module_mem),num_roots);
  size_t i = 0;
  if (initial_data != 0 ) {
    core::List_sp args((gctools::Tagged)initial_data);
    for ( auto c : args ) {
      core::T_sp arg = oCar(c);
      roots->set(i,arg.tagged_());
    //if (debug) write_bf_stream(BF("Filling roots table[%d]@%p -> %p\n") % i % ct.address(i) % (void*)arg.tagged_());
      ++i;
    }
  }
#ifdef USE_MPS
  // MPS registers the roots with the GC and doesn't need a shadow table
  mps_register_roots(reinterpret_cast<void*>(module_mem),num_roots);
#endif
}

core::T_O* read_gcroots_in_module(GCRootsInModule* roots, size_t index) {
  return (core::T_O*)(roots->get(index));
}

void shutdown_gcroots_in_module(GCRootsInModule* roots) {
#ifdef USE_BOEHM
  GC_FREE(roots->_boehm_shadow_memory);
#endif
#ifdef USE_MPS
  printf("%s:%d   Here deallocate the roots and tell the GC that they don't need to be tracked anymore\n", __FILE__, __LINE__ );
#endif
}

CL_DEFUN Fixnum gctools__nextStampValue() {
  return global_NextStamp;
}

CL_LAMBDA(address args);
CL_DEFUN void gctools__register_roots(core::T_sp taddress, core::List_sp args) {
  core::T_O** shadow_mem = NULL;
  size_t nargs = core::cl__length(args);
#ifdef USE_BOEHM
  shadow_mem = reinterpret_cast<core::T_O**>(boehm_create_shadow_table(nargs));
#endif
  // Get the address of the memory space in the llvm::Module
  uintptr_clasp_t address = translate::from_object<uintptr_clasp_t>(taddress)._v;
  core::T_O** module_mem = reinterpret_cast<core::T_O**>(address);
//  printf("%s:%d:%s address=%p nargs=%" PRu "\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, nargs);
//  printf("%s:%d:%s constants-table contents: vvvvv\n", __FILE__, __LINE__, __FUNCTION__ );
  // Create a ConstantsTable structure to write the constants with
  GCRootsInModule ct(reinterpret_cast<void*>(shadow_mem),reinterpret_cast<void*>(module_mem),nargs);
  size_t i = 0;
  for ( auto c : args ) {
    core::T_sp arg = oCar(c);
    ct.set(i,arg.tagged_());
    ++i;
  }
#ifdef USE_MPS
  // MPS registers the roots with the GC and doesn't need a shadow table
  mps_register_roots(reinterpret_cast<void*>(module_mem),nargs);
#endif
}



int startupGarbageCollectorAndSystem(MainFunctionType startupFn, int argc, char *argv[], size_t stackMax, bool mpiEnabled, int mpiRank, int mpiSize) {

  void* stackMarker = &stackMarker;
  gctools::_global_stack_marker = (const char*)&stackMarker;
  gctools::_global_stack_max_size = stackMax;
//  printf("%s:%d       global_stack_marker = %p\n", __FILE__, __LINE__, gctools::_global_stack_marker );
  global_alignup_sizeof_header = AlignUp(sizeof(Header_s));
  { // Debugging info
    size_t alignment = Alignment();
#if 0
    printf("%s:%d Alignment() = %" PRu "\n", __FILE__, __LINE__, alignment);
#ifdef USE_MPS
    printf("%s:%d Align(1) = %" PRu "\n", __FILE__, __LINE__, Align(1));
    printf("%s:%d Align(Alignment()) = %" PRu "\n", __FILE__, __LINE__, Align(Alignment()));
#endif
    printf("%s:%d Alignup(1) = %" PRu "\n", __FILE__, __LINE__, AlignUp(1));
    printf("%s:%d Alignup(Alignment()) = %" PRu "\n", __FILE__, __LINE__, AlignUp(Alignment()));
    printf("%s:%d global_alignup_sizeof_header = %" PRu "\n", __FILE__, __LINE__, global_alignup_sizeof_header );
#endif
  }
  build_stamp_field_layout_tables();
#ifdef SIGRTMIN
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGRTMIN + 2
#else
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGUSR1
#endif
  gctools::initialize_signals(DEFAULT_THREAD_INTERRUPT_SIGNAL);

#if defined(USE_MPS)
  int exitCode = gctools::initializeMemoryPoolSystem(startupFn, argc, argv, mpiEnabled, mpiRank, mpiSize);
#endif
#if defined(USE_BOEHM)
  int exitCode = gctools::initializeBoehm(startupFn, argc, argv, mpiEnabled, mpiRank, mpiSize);
#endif
  mp::ClaspThreads_exit(); // run pthreads_exit
  return exitCode;
}

Tagged GCRootsInModule::set(size_t index, Tagged val) {
#ifdef USE_BOEHM
  // shadow_memory is only used by Boehm
  if (this->_boehm_shadow_memory != this->_module_memory) {
    reinterpret_cast<core::T_O**>(this->_boehm_shadow_memory)[index] = reinterpret_cast<core::T_O*>(val);
  }
#endif
  reinterpret_cast<core::T_O**>(this->_module_memory)[index] = reinterpret_cast<core::T_O*>(val);
  return val;
}

size_t GCRootsInModule::push_back( Tagged val) {
  size_t index = this->_num_entries;
  this->_num_entries++;
  this->set(index,val);
  return index;
}

Tagged GCRootsInModule::get(size_t index) {
  return reinterpret_cast<Tagged>(reinterpret_cast<core::T_O**>(this->_module_memory)[index]);
}

};
