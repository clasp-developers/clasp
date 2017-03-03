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

#include <llvm/Support/ErrorHandling.h>
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

namespace gctools {
std::vector<Immediate_info> get_immediate_info() {
  std::vector<Immediate_info> info;
  info.push_back(Immediate_info(kind_fixnum,"FIXNUM"));
  info.push_back(Immediate_info(kind_single_float,"SINGLE_FLOAT"));
  info.push_back(Immediate_info(kind_character,"CHARACTER"));
  info.push_back(Immediate_info(kind_cons,"CONS"));
  info.push_back(Immediate_info(kind_va_list_s,"VA_LIST_S"));
  if ( (info.size()+1) != kind_first_general ) {
    printf("get_immediate_info does not set up all of the immediate types\n");
    abort();
  }
  return info;
};
};


namespace gctools {
#if 0
AllocationRecord* allocation_backtrace(size_t kind, uintptr_t stamp, size_t size, AllocationRecord* prev) {
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
  printf("%s:%d  add support to add threads for MPS\n", __FILE__, __LINE__ );
//#error "add support to add threads for MPS"
#endif
};

void unregister_thread(mp::Process_sp process) {
#ifdef USE_BOEHM
  // ----   Boehm stuff needs to be done in the thread function
//  GC_unregister_my_thread();
#endif
#ifdef USE_MPS
  printf("%s:%d  add support to add threads for MPS\n", __FILE__, __LINE__ );
#endif
};

};


namespace gctools {

// false == SIGABRT invokes debugger, true == terminate (used in core__exit)
bool global_debuggerOnSIGABRT = true;
#define INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP 16386
int global_pollTicksPerCleanup = INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP;
int global_signalTrap = 0;
int global_pollTicksGC = INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP;

void do_pollSignals() {
  int signo = global_signalTrap;
  SET_SIGNAL(0);
  if (signo == SIGINT) {
    printf("You pressed Ctrl+C\n");
    core::eval::funcall(cl::_sym_break, core::SimpleBaseString_O::make("Break on Ctrl+C"));
      //    core__invoke_internal_debugger(_Nil<core::T_O>());
    printf("Resuming after Ctrl+C\n");
  } else if (signo == SIGCHLD) {
      //            printf("A child terminated\n");
  } else if (signo == SIGFPE) {
    printf("%s:%d A floating point error occurred\n", __FILE__, __LINE__);
    core__invoke_internal_debugger(_Nil<core::T_O>());
  } else if (signo == SIGABRT) {
    printf("ABORT was called!!!!!!!!!!!!\n");
    core__invoke_internal_debugger(_Nil<core::T_O>());
      //    core:eval::funcall(cl::_sym_break,core::SimpleBaseString_O::make("ABORT was called"));
  }
#ifdef USE_MPS
  if (--global_pollTicksGC == 0 ) {
    global_pollTicksGC = global_pollTicksPerCleanup;
    gctools::gctools__cleanup();
  }
#endif
}


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
void rawHeaderDescribe(const uintptr_t *headerP) {
  uintptr_t headerTag = (*headerP) & Header_s::tag_mask;
  switch (headerTag) {
  case 0:
    printf("  0x%p : 0x%lu 0x%lu\n", headerP, *headerP, *(headerP + 1));
    printf(" Not an object header!\n");
    break;
  case Header_s::kind_tag: {
    printf("  0x%p : 0x%lu\n", headerP, *headerP);
    printf("  0x%p : 0x%lu\n", (headerP+1), *(headerP+1));
#ifdef DEBUG_GUARD
    printf("  0x%p : 0x%p\n", (headerP+2), (void*)*(headerP+2));
    printf("  0x%p : 0x%p\n", (headerP+3), (void*)*(headerP+3));
    printf("  0x%p : 0x%p\n", (headerP+4), (void*)*(headerP+4));
    printf("  0x%p : 0x%p\n", (headerP+5), (void*)*(headerP+5));
#endif    
    gctools::GCKindEnum kind = (gctools::GCKindEnum)((*headerP) >> 2);
    printf(" Kind tag - kind: %d", kind);
    fflush(stdout);
    printf("     %s\n", obj_name(kind));
  } break;
  case Header_s::fwd_tag: {
    Header_s *hdr = (Header_s *)headerP;
    printf("  0x%p : 0x%lu 0x%lu\n", headerP, *headerP, *(headerP + 1));
    printf(" fwd_tag - fwd address: 0x%lu\n", (*headerP) & Header_s::fwd_ptr_mask);
    printf("     fwdSize = %lu/0x%lu\n", hdr->fwdSize(), hdr->fwdSize());
  } break;
  case Header_s::pad_tag:
    printf("  0x%p : 0x%lu 0x%lu\n", headerP, *headerP, *(headerP + 1));
    if (((*headerP) & Header_s::pad1_tag) == Header_s::pad1_tag) {
      printf("   pad1_tag\n");
      printf("  0x%p : 0x%lu\n", headerP, *headerP);
    } else {
      printf("   pad_tag\n");
      printf("  0x%p : 0x%lu\n", headerP, *headerP);
      printf("  0x%p : 0x%lu\n", (headerP+1), *(headerP+1));
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
    const uintptr_t *headerP;
    if (gctools::tagged_generalp(taggedClient)) {
      headerP = reinterpret_cast<const uintptr_t *>(gctools::ClientPtrToBasePtr(gctools::untag_general(taggedClient)));
    } else {
      headerP = reinterpret_cast<const uintptr_t *>(gctools::ClientPtrToBasePtr(gctools::untag_cons(taggedClient)));
    }
    gctools::rawHeaderDescribe(headerP);
  } else {
    printf("%s:%d Not a tagged pointer - might be immediate value\n", __FILE__, __LINE__);
    printf("    Trying to interpret as client pointer\n");
    const uintptr_t* headerP;
    headerP = reinterpret_cast<const uintptr_t*>(gctools::ClientPtrToBasePtr(taggedClient));
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
  gctools::rawHeaderDescribe((uintptr_t*)headerP);
};
};



namespace gctools {
size_t random_tail_size() {
  size_t ts = ((rand() % 8) + 1) * Alignment();
  return ts;
}

void Header_s::quick_validate() const {
#if DEBUG_GUARD_VALIDATE
  if (this->guard != 0xFEEAFEEBDEADBEEF) {
    printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
    printf("%s:%d   Missing magic value 0x0FEEAFEEBDEADBEEF in object header!!!!! @ %p\n", __FILE__, __LINE__, (void*)this);
    abort();
  }
  const unsigned char* tail = (const unsigned char*)this+this->tail_start;
  if ((*tail) != 0xcc) {
    printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
    printf("%s:%d bad tail value !!!!! @ %p  value = %x  should be 0xcc\n", __FILE__, __LINE__, (void*)tail, *tail);
    abort();
  }
#endif
}


void Header_s::validate() const {
  if ( this->header == 0 ) {
    printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
    printf("%s:%d NULL object header!!!!! @ %p\n", __FILE__, __LINE__, (void*)this);
    abort();
  }
  if ( this->invalidP() ) {
    printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
    printf("%s:%d Invalid object header does not have a real tag\n", __FILE__, __LINE__ );
    abort();
  }
  if ( this->kindP() ) {
#ifdef DEBUG_GUARD    
    if ( this->guard != 0xFEEAFEEBDEADBEEF) {
      printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
      printf("%s:%d  INVALID object  this->guard@%p is bad guard value->%p\n", __FILE__, __LINE__, (void*)&this->guard, (void*)this->guard );
      abort();
    }
#endif
    if ( this->kind() > global_NextStamp ) {
      printf("%s:%d  INVALID object  this->kind()=%d > KIND_max=%d\n", __FILE__, __LINE__, this->kind(), KIND_max );
      abort();
    }
#ifdef DEBUG_GUARD
    if ( this->tail_start & 0xffffffffff000000 ) {
      printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
      printf("%s:%d   header->tail_start@%p is not a reasonable value -> %x\n", __FILE__,__LINE__, (void*)&this->tail_start, this->tail_start);
    }
    if ( this->tail_size & 0xffffffffff000000 ) {
      printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
      printf("%s:%d   header->tail_size@%p is not a reasonable value -> %x\n", __FILE__, __LINE__, (void*)&this->tail_size,this->tail_size);
    }
    for ( unsigned char *cp=((unsigned char*)(this)+this->tail_start), 
            *cpEnd((unsigned char*)(this)+this->tail_start+this->tail_size); cp < cpEnd; ++cp ) {
      if (*cp!=0xcc) {
        printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
        printf("%s:%d INVALID tail header@%p bad tail byte@%p -> %x\n", __FILE__, __LINE__, (void*)this, cp, *cp );
        abort();
      }
    }
#endif
  }
#ifdef DEBUG_GUARD
  if ( this->fwdP() ) {
    if ( this->guard != 0xFEEAFEEBDEADBEEF) {
      printf("%s:%d  In validate header@%p  header -> %p\n", __FILE__, __LINE__, (void*)this, (void*)this->header);
      printf("%s:%d  INVALID object  this->guard is bad value->%p\n", __FILE__, __LINE__, (void*)this->guard );
      abort();
    }
    for ( unsigned char *cp=((unsigned char*)(this)+this->tail_start), 
            *cpEnd((unsigned char*)(this)+this->tail_start+this->tail_size); cp < cpEnd; ++cp ) {
      if (*cp!=0xcc) {
        printf("%s:%d INVALID tail header@%p bad tail byte@%p -> %x\n", __FILE__, __LINE__, (void*)this, cp, *cp );
        abort();
      }
    }
  }
#endif
}
};



namespace gctools {

/*! See NextStamp(...) definition in memoryManagement.h.
  global_NextBuiltInStamp starts at KIND_max+1
  so that it doesn't use any stamps that correspond to KIND values
   assigned by the static analyzer. */
Stamp   global_NextStamp = KIND_max+1;

void OutOfStamps() {
    printf("%s:%d Hello future entity!  Congratulations! - you have run clasp long enough to run out of STAMPs - %lu are allowed - change the clasp header layout or add another word for the stamp\n", __FILE__, __LINE__, Header_s::largest_possible_kind );
    abort();
}

GCStack _ThreadLocalStack;
const char *_global_stack_marker;
size_t _global_stack_max_size;
/*! Keeps track of the next available header KIND value */
kind_t global_next_header_kind = (kind_t)KIND_max+1;

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

MonitorAllocations global_monitorAllocations;

void monitorAllocation(kind_t k, size_t sz) {
  printf("%s:%d monitor allocation of %s with %zu bytes\n", __FILE__, __LINE__, obj_name(k), sz);
  if (global_monitorAllocations.counter >= global_monitorAllocations.start && global_monitorAllocations.counter < global_monitorAllocations.end) {
    core::core__clib_backtrace(global_monitorAllocations.backtraceDepth);
  }
  global_monitorAllocations.counter++;
}

void handle_signals(int signo) {
  //
  // Indicate that a signal was caught and handle it at a safe-point
  //
  SET_SIGNAL(signo);
  if (signo == SIGABRT && global_debuggerOnSIGABRT) {
    printf("%s:%d Trapped SIGABRT - starting debugger\n", __FILE__, __LINE__);
    core::LispDebugger debugger(_Nil<core::T_O>());
    debugger.invoke();
  }
}

void fatal_error_handler(void *user_data, const std::string &reason, bool gen_crash_diag) {
  printf("Hit a fatal error in llvm/clang: %s\n", reason.c_str());
  printf("Clasp is terminating via abort(0)\n");
  abort();
}

#ifdef USE_BOEHM
void clasp_warn_proc(char *msg, GC_word arg) {
  printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__);
  printf(msg, arg);
}
#endif

void setupSignals() {
  if (signal(SIGINT, handle_signals) == SIG_ERR) {
    printf("failed to register SIGINT signal-handler with kernel\n");
  }
  if (signal(SIGCHLD, handle_signals) == SIG_ERR) {
    printf("failed to register SIGCHLD signal-handler with kernel\n");
  }
  if (signal(SIGABRT, handle_signals) == SIG_ERR) {
    printf("failed to register SIGABRT signal-handler with kernel\n");
  }
#if 0
#ifdef _TARGET_OS_LINUX
  feenableexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
#ifdef _TARGET_OS_DARWIN
  feenableexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
  if (signal(SIGFPE, handle_signals) == SIG_ERR) {
    printf("failed to register SIGFPE signal-handler with kernel\n");
  }
#endif
  llvm::install_fatal_error_handler(fatal_error_handler, NULL);
}

gc::GCStack *threadLocalStack() {
  return &_ThreadLocalStack;
}

int handleFatalCondition() {
  int exitCode = 0;
  try {
    throw;
  } catch (core::ExitProgram &ee) {
    // Do nothing
    //            printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
    exitCode = ee.getExitResult();
  } catch (core::TerminateProgramIfBatch &ee) {
    // Do nothing
    printf("Caught TerminateProgramIfBatch in %s:%d\n", __FILE__, __LINE__);
  } catch (core::Condition &ee) {
    IMPLEMENT_MEF(BF("Figure out what to do if we catch a Condition"));
    //        printf("Caught Condition at %s:%d - %s\n", __FILE__, __LINE__, ee.message().c_str() );
    //        printf("Stack trace:\n%s", ee.conditionObject()->getStackTraceDump().c_str() );
  } catch (core::CatchThrow &ee) {
    _lisp->print(BF("%s:%d Uncaught THROW frame[%s] - this should NEVER happen - the stack should never be unwound unless there is a CATCH clause that matches the THROW") % __FILE__ % __LINE__ % ee.getFrame());
  } catch (core::Unwind &ee) {
    _lisp->print(BF("At %s:%d - Unwind caught frame: %d index: %d") % __FILE__ % __LINE__ % ee.getFrame() % ee.index());
  } catch (core::HardError &ee) {
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

kind_t next_header_kind()
{
    kind_t next = global_next_header_kind;
    ++global_next_header_kind;
    return next;
}

core::Fixnum ensure_fixnum(kind_t val)
{
  if ( val > most_positive_fixnum || val < most_negative_fixnum ) {
    SIMPLE_ERROR(BF("The value %lz cannot be converted into a FIXNUM") % val );
  }
  return (core::Fixnum)val;
}

CL_LAMBDA();
CL_DOCSTRING(R"doc(Return the next available header KIND value and increment the global variable global_next_header_kind)doc");
CL_DEFUN core::Fixnum gctools__next_header_kind()
{
    kind_t next = global_next_header_kind;
    ++global_next_header_kind;
    return ensure_fixnum(next);
}

/*! initial_data is a gctools::Tagged pointer to a List of tagged pointers.
*/
void initialize_gcroots_in_module(GCRootsInModule* roots, core::T_sp* root_address, size_t num_roots, gctools::Tagged initial_data) {
  core::T_sp* shadow_mem = NULL;
#ifdef USE_BOEHM
  shadow_mem = reinterpret_cast<core::T_sp*>(boehm_create_shadow_table(num_roots));
#endif
  // Get the address of the memory space in the llvm::Module
  uintptr_t address = reinterpret_cast<uintptr_t>(root_address);
  core::T_sp* module_mem = reinterpret_cast<core::T_sp*>(address);
//  printf("%s:%d:%s address=%p nargs=%lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, nargs);
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
    //if (debug) BFORMAT_T(BF("Filling roots table[%lu]@%p -> %p\n") % i % ct.address(i) % (void*)arg.tagged_());
      ++i;
    }
  }
#ifdef USE_MPS
  // MPS registers the roots with the GC and doesn't need a shadow table
  mps_register_roots(reinterpret_cast<void*>(module_mem),num_roots);
#endif
}

void shutdown_gcroots_in_module(GCRootsInModule* roots) {
#ifdef USE_BOEHM
  GC_FREE(roots->_boehm_shadow_memory);
#endif
#ifdef USE_MPS
  printf("%s:%d   Here deallocate the roots and tell the GC that they don't need to be tracked anymore\n", __FILE__, __LINE__ );
#endif
}

CL_LAMBDA(address args);
CL_DEFUN void gctools__register_roots(core::T_sp taddress, core::List_sp args) {
  core::T_sp* shadow_mem = NULL;
  size_t nargs = core::cl__length(args);
#ifdef USE_BOEHM
  shadow_mem = reinterpret_cast<core::T_sp*>(boehm_create_shadow_table(nargs));
#endif
  // Get the address of the memory space in the llvm::Module
  uintptr_t address = translate::from_object<uintptr_t>(taddress)._v;
  core::T_sp* module_mem = reinterpret_cast<core::T_sp*>(address);
//  printf("%s:%d:%s address=%p nargs=%lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, nargs);
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
  void* stackMarker = NULL;
  gctools::_global_stack_marker = (const char*)&stackMarker;
  gctools::_global_stack_max_size = stackMax;
//  printf("%s:%d       global_stack_marker = %p\n", __FILE__, __LINE__, gctools::_global_stack_marker );
  global_alignup_sizeof_header = AlignUp(sizeof(Header_s));

  { // Debugging info
    size_t alignment = Alignment();
#if 0
    printf("%s:%d Alignment() = %lu\n", __FILE__, __LINE__, alignment);
#ifdef USE_MPS
    printf("%s:%d Align(1) = %lu\n", __FILE__, __LINE__, Align(1));
    printf("%s:%d Align(Alignment()) = %lu\n", __FILE__, __LINE__, Align(Alignment()));
#endif
    printf("%s:%d Alignup(1) = %lu\n", __FILE__, __LINE__, AlignUp(1));
    printf("%s:%d Alignup(Alignment()) = %lu\n", __FILE__, __LINE__, AlignUp(Alignment()));
    printf("%s:%d global_alignup_sizeof_header = %lu\n", __FILE__, __LINE__, global_alignup_sizeof_header );
#endif
  }

  build_kind_field_layout_tables();

  setupSignals();


#if defined(USE_MPS)
  int exitCode = gctools::initializeMemoryPoolSystem(startupFn, argc, argv, mpiEnabled, mpiRank, mpiSize);
#endif

#if defined(USE_BOEHM)
  GC_INIT();
  GC_allow_register_threads();
  GC_set_java_finalization(1);
//  GC_allow_register_threads();
  GC_set_all_interior_pointers(1); // tagged pointers require this
                                   //printf("%s:%d Turning on interior pointers\n",__FILE__,__LINE__);
  GC_set_warn_proc(clasp_warn_proc);
  //  GC_enable_incremental();
  GC_init();
  _ThreadLocalStack.allocateStack(gc::thread_local_cl_stack_min_size);
  void* topOfStack;
  core::ThreadLocalState thread_local_state(&topOfStack);
  my_thread = &thread_local_state;
#if 0
  // I'm not sure if this needs to be done for the main thread
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#endif
  int exitCode = startupFn(argc, argv, mpiEnabled, mpiRank, mpiSize);
#if 0
  GC_unregister_my_thread();
#endif
#endif
  mp::ClaspThreads_exit(); // run pthreads_exit
  return exitCode;
}

Tagged GCRootsInModule::set(size_t index, Tagged val) {
#ifdef USE_BOEHM
  // shadow_memory is only used by Boehm
  reinterpret_cast<core::T_sp*>(this->_boehm_shadow_memory)[index] = core::T_sp(val);
#endif
  reinterpret_cast<core::T_sp*>(this->_module_memory)[index] = core::T_sp(val);
  return val;
}
};
