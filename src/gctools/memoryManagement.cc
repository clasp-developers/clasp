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

#define DEBUG_LEVEL_NONE

#include <stack>
#include <utility> // pair
#include <unistd.h>
#include <fcntl.h>
#include <clasp/core/foundation.h>
#include <clasp/gctools/gcalloc.h>
#include <clasp/core/object.h>
#include <clasp/core/bformat.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/function.h>
#include <clasp/core/debugger.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/mpPackage.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/code.h>
#if 0
#include <clasp/core/bundle.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/compiler.h>
#include <clasp/gctools/gc_interface.fwd.h>
#endif
// #include "main/allHeaders.cc"

#ifdef _TARGET_OS_LINUX
#include <signal.h>
#endif

#if 0
#define GCROOT_LOG(x)                                                                                                              \
  if (_sym_STARdebug_gcrootsSTAR && _sym_STARdebug_gcrootsSTAR.boundp() && _sym_STARdebug_gcrootsSTAR->symbolValue() &&            \
      _sym_STARdebug_gcrootsSTAR->symbolValue().notnilp()) {                                                                       \
    printf x;                                                                                                                      \
  }
#else
#define GCROOT_LOG(x)
#endif

SYMBOL_EXPORT_SC_(GcToolsPkg, STARdebug_gcrootsSTAR);

extern "C" {
void gc_park() {
#if defined(USE_BOEHM)
  boehm_park();
#elif defined(USE_MMTK)

#endif
};

void gc_release() {
#if defined(USE_BOEHM)
  boehm_release();
#elif defined(USE_MMTK)
  MISSING_GC_SUPPORT();
#endif
};

__attribute__((noinline)) void HitAllocationSizeThreshold() { my_thread_low_level->_Allocations._HitAllocationSizeCounter++; }

__attribute__((noinline)) void HitAllocationNumberThreshold() { my_thread_low_level->_Allocations._HitAllocationNumberCounter++; }
}

#include <clasp/core/scrape.h>

////////////////////////////////////////////////////////////
//
// GC_MANAGED_TYPE
//
// Objects that are managed by the GC and need a stamp
//   but are not directly accessible to Common Lisp
// GC_MANAGED_TYPE(core::Lisp);
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
GC_MANAGED_TYPE(gctools::GCBitUnitArray_moveable<1,0>);
GC_MANAGED_TYPE(gctools::GCBitUnitArray_moveable<2,0>);
GC_MANAGED_TYPE(gctools::GCBitUnitArray_moveable<4,0>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<clbind::detail::edge>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::KeyValuePair>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::AuxArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::CacheRecord>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::DynamicBinding>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::ExceptionEntry>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::KeywordArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::OptionalArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::RequiredArgument>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::SymbolClassHolderPair>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<core::SymbolStorage>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<clbind::detail::vertex>);

GC_MANAGED_TYPE(gctools::GCVector_moveable<core::T_O *>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<double>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<float>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::KeyValuePair>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Instance_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Creator_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::FileScope_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<int>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>);
GC_MANAGED_TYPE(gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>);

namespace gctools {

size_t global_sizeof_fwd;

// GCStack _ThreadLocalStack;
size_t _global_stack_max_size;

#if 0
    HeapRoot* 	rooted_HeapRoots = NULL;
    StackRoot* 	rooted_StackRoots = NULL;
#endif

void GCRootsInModule::setup_transients(core::SimpleVector_O** transient_alloca, size_t transient_entries) {
  if (!transient_alloca && transient_entries != 0) {
    printf("%s:%d:%s PROBLEM!!! transient_alloca is %p and transient_entries is %lu\n", __FILE__, __LINE__, __FUNCTION__,
           transient_alloca, transient_entries);
    abort();
  }
  if (transient_alloca && transient_entries > 0) {
    core::SimpleVector_sp sv = core::SimpleVector_O::make(transient_entries);
    for (size_t ii = 0; ii < transient_entries; ++ii) {
      (*sv)[ii] = core::make_fixnum(12345);
    }
    GCROOT_LOG(("%s:%d  Setup simple vector@%p\n", __FILE__, __LINE__, (void*)sv.tagged_()));
    *transient_alloca = &(*sv);
    this->_TransientAlloca = transient_alloca;
  } else {
    this->_TransientAlloca = nullptr;
  }
}

GCRootsInModule::GCRootsInModule(void* module_mem, size_t num_entries, core::SimpleVector_O** transient_alloca,
                                 size_t transient_entries, size_t function_pointer_count, void** fptrs) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Compiled code literals are from %p to %p\n", __FILE__, __LINE__, __FUNCTION__, module_mem,
                            (char*)module_mem + (sizeof(core::T_O*) * num_entries)));
  llvmo::JITDataReadWriteMaybeExecute();
  this->_function_pointer_count = function_pointer_count;
  this->_function_pointers = fptrs;
  this->_num_entries = num_entries;
  this->_capacity = num_entries;
  this->_module_memory = module_mem;
  this->setup_transients(transient_alloca, transient_entries);
  llvmo::JITDataReadExecute();
}

/*! initial_data is a gctools::Tagged pointer to a List of tagged pointers.
 */
void initialize_gcroots_in_module(GCRootsInModule* roots, core::T_O** root_address, size_t num_roots, gctools::Tagged initial_data,
                                  core::SimpleVector_O** transientAlloca, size_t transient_entries, size_t function_pointer_count,
                                  void** fptrs) {
  // Get the address of the memory space in the llvm::Module
  uintptr_t address = reinterpret_cast<uintptr_t>(root_address);
  core::T_O** module_mem = reinterpret_cast<core::T_O**>(address);
  //  printf("%s:%d:%s address=%p nargs=%" PRu "\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, nargs);
  //  printf("%s:%d:%s constants-table contents: vvvvv\n", __FILE__, __LINE__, __FUNCTION__ );
  // Create a GCRootsInModule structure to write the constants with
  // FIXME: The GCRootsInModule is on the stack - once it's gone we loose the ability
  //        to keep track of the constants and in the future when we start GCing code
  //        we need to keep track of the constants.
  new (roots) GCRootsInModule(reinterpret_cast<void*>(module_mem), num_roots, transientAlloca, transient_entries,
                              function_pointer_count, (void**)fptrs);
  size_t idx = 0;
  if (initial_data != 0) {
    core::List_sp args((gctools::Tagged)initial_data);
    for (auto c : args) {
      core::T_sp arg = CONS_CAR(c);

      //
      // This is where we translate some literals
      // This is like load-time
      //
      if (gc::IsA<core::SimpleCoreFunGenerator_sp>(arg)) {
        core::SimpleCoreFunGenerator_sp fdgen = gc::As_unsafe<core::SimpleCoreFunGenerator_sp>(arg);
        arg = core::makeSimpleCoreFunFromGenerator(fdgen, roots, fptrs);
      } else if (gc::IsA<core::CoreFunGenerator_sp>(arg)) {
        core::CoreFunGenerator_sp fdgen = gc::As_unsafe<core::CoreFunGenerator_sp>(arg);
        arg = core::makeCoreFunFromGenerator(fdgen, fptrs);
      }

      roots->setLiteral(idx, arg.tagged_());
      ++idx;
    }
  }
}

core::T_O* read_gcroots_in_module(GCRootsInModule* roots, size_t index) { return (core::T_O*)(roots->getLiteral(index)); }

void shutdown_gcroots_in_module(GCRootsInModule* roots) { roots->_TransientAlloca = NULL; }

DOCGROUP(clasp);
CL_DEFUN Fixnum gctools__nextStampValue() { return Header_s::StampWtagMtag::shift_unshifted_stamp(global_NextUnshiftedStamp); }
DOCGROUP(clasp);
CL_DEFUN Fixnum gctools__NextUnshiftedStampValue() { return global_NextUnshiftedStamp; }

CL_LAMBDA(address args);
DOCGROUP(clasp);
CL_DEFUN void gctools__register_roots(core::T_sp taddress, core::List_sp args) {
  size_t nargs = core::cl__length(args);
  // Get the address of the memory space in the llvm::Module
  uintptr_t address = translate::make_from_object<uintptr_t>(taddress);
  core::T_O** module_mem = reinterpret_cast<core::T_O**>(address);
  //  printf("%s:%d:%s address=%p nargs=%" PRu "\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, nargs);
  //  printf("%s:%d:%s constants-table contents: vvvvv\n", __FILE__, __LINE__, __FUNCTION__ );
  // Create a ConstantsTable structure to write the constants with
  GCRootsInModule ct(reinterpret_cast<void*>(module_mem), nargs, NULL, 0, 0, NULL);
  size_t i = 0;
  for (auto c : args) {
    core::T_sp arg = oCar(c);
    ct.setLiteral(i, arg.tagged_());
    ++i;
  }
}

}; // namespace gctools
namespace gctools {
void lisp_increment_recursive_allocation_counter(ThreadLocalStateLowLevel* thread, size_t header_value) {
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  int x = thread->_RecursiveAllocationCounter + 1;
  thread->_RecursiveAllocationCounter = x;
  if (x != 1) {
    printf("%s:%d A recursive allocation took place - these are illegal!!!!\n     The outer header_value is %lu and the inner one "
           "is %lu\n",
           __FILE__, __LINE__, thread->_RecursiveAllocationHeaderValue, header_value);
    dbg_safe_backtrace();
    abort();
  }
  thread->_RecursiveAllocationHeaderValue = header_value;
#endif
}
void lisp_decrement_recursive_allocation_counter(ThreadLocalStateLowLevel* thread) {
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  --thread->_RecursiveAllocationCounter;
#endif
};

}; // namespace gctools

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
}; // namespace gctools

namespace gctools {

void register_thread(mp::Process_sp process, void* stack_base) {
#if defined(USE_BOEHM)
  // ----   Boehm stuff needs to be done in the thread function
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#else
  MISSING_GC_SUPPORT();
#endif
};

void unregister_thread(mp::Process_sp process) {
#if defined(USE_BOEHM)
  // ----   Boehm stuff needs to be done in the thread function
  GC_unregister_my_thread();
#else
  MISSING_GC_SUPPORT();
#endif
};

}; // namespace gctools

namespace gctools {

char* clasp_alloc_atomic(size_t buffer) { return (char*)malloc(buffer); }

void clasp_dealloc(char* buffer) {
  if (buffer) {
    free(buffer);
  }
}

}; // namespace gctools

namespace gctools {

bool is_memory_readable(const void* address, size_t bytes) {
  int fd[2];
  int ret = pipe(fd);
  if (ret == -1) {
    printf("%s:%d:%s Error creating pipe\n", __FILE__, __LINE__, __FUNCTION__);
    wait_for_user_signal("Error creating pipe in is_memory_readable");
  }

  // Try to write to an unwritable file descriptor
  ret = write(fd[1], address, bytes);
  close(fd[0]);
  close(fd[1]);

  if (ret == -1 && errno == EFAULT) {
    // Memory is not readable
    return false;
  } else {
    // Memory is readable
    return true;
  }
}

void rawHeaderDescribe(const uintptr_t* headerP) {
  uintptr_t headerTag = (*headerP) & Header_s::mtag_mask;
  switch (headerTag) {
  case Header_s::invalid0_mtag:
  case Header_s::invalid1_mtag:
  case Header_s::invalid2_mtag: {
    printf("  %p : %" PRIuPTR "(%p) %" PRIuPTR "(%p)\n", headerP, *headerP, (void*)*headerP, *(headerP + 1), (void*)*(headerP + 1));
    printf(" Not an object header!\n");
    break;
  }
  case Header_s::stamp_mtag: {
    if (is_memory_readable((void*)headerP, 8)) {
      printf("   %p : %18p <- header\n", headerP, (void*)*headerP);
    } else {
      printf("   %p : <<<<< The address is NOT readable\n", headerP);
      return;
    }
#ifndef DEBUG_GUARD
    printf("   %p : %18p <- vtable\n", (headerP + 1), (void*)*(headerP + 1));
    fflush(stdout);
#else
    printf("   %p : %18p\n", (headerP + 1), (void*)*(headerP + 1));
    printf("   %p : %18p\n", (headerP + 2), (void*)*(headerP + 2));
    printf("   %p : %18p\n", (headerP + 3), (void*)*(headerP + 3));
    printf("   %p : %18p\n", (headerP + 4), (void*)*(headerP + 4));
    printf("   %p : %18p\n", (headerP + 5), (void*)*(headerP + 5));
#endif
    size_t stamp_wtag = (GCStampEnum)((*((Header_s*)headerP))._badge_stamp_wtag_mtag.stamp_wtag());
    GCStampEnum kind = (GCStampEnum)((*((Header_s*)headerP))._badge_stamp_wtag_mtag.stamp());
    printf(" ACTUAL stamp_wtag   = %4zu", stamp_wtag);
    fflush(stdout);
    printf(" name: %s\n", obj_name(kind));
  } break;
  case Header_s::fwd_mtag: {
    Header_s* hdr = (Header_s*)headerP;
    printf("  0x%p : 0x%" PRIuPTR " 0x%" PRIuPTR "\n", headerP, *headerP, *(headerP + 1));
    printf(" fwd_tag - fwd address: 0x%" PRIuPTR "\n", (*headerP) & Header_s::mtag_mask);
  } break;
  case Header_s::pad1_mtag:
    printf("  0x%p : 0x%" PRIuPTR " 0x%" PRIuPTR "\n", headerP, *headerP, *(headerP + 1));
    printf("   pad1_tag\n");
    printf("  0x%p : 0x%" PRIuPTR "\n", headerP, *headerP);
    break;
  case Header_s::pad_mtag:
    printf("   pad_tag\n");
    printf("  0x%p : 0x%" PRIuPTR "\n", headerP, *headerP);
    printf("  0x%p : 0x%" PRIuPTR "\n", (headerP + 1), *(headerP + 1));
    break;
  }
#ifdef DEBUG_GUARD
  Header_s* header = (Header_s*)headerP;
  header->validate();
  printf("This object passed the validate() test\n");
#endif
};
}; // namespace gctools

extern "C" {
void client_describe(void* taggedClient) {
  if (gctools::tagged_generalp(taggedClient) || gctools::tagged_consp(taggedClient)) {
    // Currently this assumes that Conses and General objects share the same header
    // this may not be true in the future
    // conses may be moved into a separate pool and dealt with in a different way
    const uintptr_t* headerP;
    if (gctools::tagged_generalp(taggedClient)) {
      headerP = reinterpret_cast<const uintptr_t*>(gctools::GeneralPtrToHeaderPtr(gctools::untag_general(taggedClient)));
    } else {
      headerP = reinterpret_cast<const uintptr_t*>(gctools::GeneralPtrToHeaderPtr(gctools::untag_cons(taggedClient)));
    }
    gctools::rawHeaderDescribe(headerP);
  } else {
    printf("%s:%d Not a tagged pointer - might be immediate value\n", __FILE__, __LINE__);
    printf("    Trying to interpret as client pointer\n");
    const uintptr_t* headerP;
    headerP = reinterpret_cast<const uintptr_t*>(gctools::GeneralPtrToHeaderPtr(taggedClient));
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
  const gctools::Header_s* header =
      reinterpret_cast<const gctools::Header_s*>(gctools::GeneralPtrToHeaderPtr(reinterpret_cast<const void*>(client_ptr)));
  header->validate();
}

void client_validate_Cons_O_ptr(const core::Cons_O* client_ptr) {
  const gctools::Header_s* header =
      reinterpret_cast<const gctools::Header_s*>(gctools::ConsPtrToHeaderPtr(reinterpret_cast<const void*>(client_ptr)));
  if (!header->_badge_stamp_wtag_mtag.consObjectP()) {
    printf("%s:%d The header %p is not a cons header and it must be\n", __FILE__, __LINE__, (void*)client_ptr);
    abort();
  }
}

void client_validate_tagged(gctools::Tagged taggedClient) {
  if (gctools::tagged_generalp(taggedClient)) {
    core::General_O* client = reinterpret_cast<core::General_O*>(gctools::untag_general(taggedClient));
    client_validate_General_O_ptr(client);
  } else if (gctools::tagged_consp(taggedClient)) {
    // Nothing can be done to validate CONSes, they are too compact.
  }
};

void header_describe(gctools::Header_s* headerP) { gctools::rawHeaderDescribe((uintptr_t*)headerP); };
};

namespace gctools {
size_t random_tail_size() {
  size_t ts = ((rand() % 8) + 1) * Alignment();
  return ts;
}

BaseHeader_s::BadgeStampWtagMtag::BadgeStampWtagMtag(const BadgeStampWtagMtag& other) : StampWtagMtag((StampWtagMtag&)other) {
  this->_header_badge.store(other._header_badge.load());
  //  printf("%s:%d:%s my copy ctor\n", __FILE__, __LINE__, __FUNCTION__ );
}

BaseHeader_s::BaseHeader_s(const BaseHeader_s& other) : _badge_stamp_wtag_mtag(other._badge_stamp_wtag_mtag) {
  printf("%s:%d:%s my copy ctor\n", __FILE__, __LINE__, __FUNCTION__);
}

void BaseHeader_s::signal_invalid_object(const BaseHeader_s* header, const char* msg) {
  printf("%s:%d  Invalid object with header @ %p message: %s\n", __FILE__, __LINE__, (void*)header, msg);
  abort();
}

void BaseHeader_s::validate() const {
  if (((uintptr_t)this & ptag_mask) != 0) {
    printf("%s:%d The header %p is out of alignment\n", __FILE__, __LINE__, (void*)this);
    abort();
  }
  if (this->_badge_stamp_wtag_mtag._value == 0)
    signal_invalid_object(this, "stamp_wtag_mtag is 0");
  if (this->_badge_stamp_wtag_mtag.invalidP())
    signal_invalid_object(this, "header is invalidP");
  if (this->_badge_stamp_wtag_mtag.stampP()) {
#if defined(USE_PRECISE_GC)
    uintptr_t stamp_index = (uintptr_t)this->_badge_stamp_wtag_mtag.stamp_();
    if (stamp_index > STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_max)) { // wasMTAG
      printf("%s:%d A bad stamp was found %lu at addr %p\n", __FILE__, __LINE__, stamp_index, (void*)this);
      signal_invalid_object(this, "stamp out of range in header");
    }
#endif // USE_PRECISE_GC
    if (!(gctools::BaseHeader_s::StampWtagMtag::is_shifted_stamp(this->_badge_stamp_wtag_mtag._value)))
      signal_invalid_object(this, "normal object bad header stamp");
  } else {
    signal_invalid_object(this, "Not a normal object");
  }
}

bool ConsHeader_s::isValidConsObject() const {
  if (((uintptr_t)this & ptag_mask) != 0) {
    printf("%s:%d The cons header %p is out of alignment\n", __FILE__, __LINE__, (void*)this);
    abort();
  }
  void* gcBase;
  if (!is_memory_readable((void*)this, 8))
    goto bad;
  gcBase = GC_base((void*)this);
  if (gcBase != (void*)this)
    goto bad;
  if (this->_badge_stamp_wtag_mtag._value == 0)
    goto bad;
  if (this->_badge_stamp_wtag_mtag.invalidP())
    goto bad;
  if (!this->_badge_stamp_wtag_mtag.consObjectP())
    goto bad;
  return true;
bad:
  return false;
}

bool Header_s::isValidGeneralObject() const {
  if (((uintptr_t)this & ptag_mask) != 0) {
    printf("%s:%d The general header %p is out of alignment\n", __FILE__, __LINE__, (void*)this);
    abort();
  }
  void* gcBase;
  if (!is_memory_readable((void*)this, 8))
    goto bad;
  gcBase = GC_base((void*)this);
  if (gcBase != (void*)this)
    goto bad;
  if (this->_badge_stamp_wtag_mtag._value == 0)
    goto bad;
#ifdef DEBUG_GUARD
  if (this->_badge_stamp_wtag_mtag._value != this->_dup_badge_stamp_wtag_mtag._value)
    goto bad;
#endif
  if (this->_badge_stamp_wtag_mtag.invalidP())
    goto bad;
  if (this->_badge_stamp_wtag_mtag.stampP()) {
#if defined(USE_PRECISE_GC)
    uintptr_t stamp_index = (uintptr_t)this->_badge_stamp_wtag_mtag.stamp_();
    if (stamp_index > STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_max))
      goto bad; // wasMTAG
#endif          // USE_PRECISE_GC
#ifdef DEBUG_GUARD
    if (this->_guard != GUARD1)
      goto bad;
    if (this->_guard2 != GUARD2)
      goto bad;
#endif
    if (!(gctools::Header_s::StampWtagMtag::is_shifted_stamp(this->_badge_stamp_wtag_mtag._value)))
      goto bad;
#ifdef DEBUG_GUARD
    for (unsigned char *cp = ((unsigned char*)(this) + this->_tail_start),
                       *cpEnd((unsigned char*)(this) + this->_tail_start + this->_tail_size);
         cp < cpEnd; ++cp) {
      if (*cp != 0xcc)
        goto bad;
    }
#endif
  }
  return true;
bad:
  // printf("%s:%d:%s Encountered a bad general object at %p value: 0x%x\n", __FILE__, __LINE__, __FUNCTION__, this,
  // this->_badge_stamp_wtag_mtag._value );
  return false;
}

void Header_s::validate() const {
  if (((uintptr_t)this & ptag_mask) != 0) {
    printf("%s:%d The header %p is out of alignment\n", __FILE__, __LINE__, (void*)this);
    abort();
  }
  if (this->_badge_stamp_wtag_mtag._value == 0)
    signal_invalid_object(this, "stamp_wtag_mtag is 0");
#ifdef DEBUG_GUARD
  if (this->_badge_stamp_wtag_mtag._value != this->_dup_badge_stamp_wtag_mtag._value)
    signal_invalid_object(this, "header stamps are invalid");
#endif
  if (this->_badge_stamp_wtag_mtag.invalidP())
    signal_invalid_object(this, "header is invalidP");
  if (this->_badge_stamp_wtag_mtag.stampP()) {
#if defined(USE_PRECISE_GC)
    uintptr_t stamp_index = (uintptr_t)this->_badge_stamp_wtag_mtag.stamp_();
    if (stamp_index > STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_max)) { // wasMTAG
      printf("%s:%d A bad stamp was found %lu at addr %p\n", __FILE__, __LINE__, stamp_index, (void*)this);
      signal_invalid_object(this, "stamp out of range in header");
    }
#endif // USE_PRECISE_GC
#ifdef DEBUG_GUARD
    if (this->_guard != GUARD1)
      signal_invalid_object(this, "normal object bad header guard");
    if (this->_guard2 != GUARD2)
      signal_invalid_object(this, "normal object bad header guard2");
#endif
    if (!(gctools::Header_s::StampWtagMtag::is_shifted_stamp(this->_badge_stamp_wtag_mtag._value)))
      signal_invalid_object(this, "normal object bad header stamp");
#ifdef DEBUG_GUARD
    for (unsigned char *cp = ((unsigned char*)(this) + this->_tail_start),
                       *cpEnd((unsigned char*)(this) + this->_tail_start + this->_tail_size);
         cp < cpEnd; ++cp) {
      if (*cp != 0xcc)
        signal_invalid_object(this, "bad tail content");
    }
#endif
  } else {
    signal_invalid_object(this, "Not a normal object");
  }
}

//
//
// When USE_PRECISE_GC then we expose more methods for Header_s
//

#ifdef USE_PRECISE_GC

//
// Return true if the object represented by this header is polymorphic
bool BaseHeader_s::preciseIsPolymorphic() const {
  if (this->_badge_stamp_wtag_mtag.stampP()) {
    uintptr_t stamp = this->_badge_stamp_wtag_mtag.stamp();
    return global_stamp_layout[stamp].flags & IS_POLYMORPHIC;
  } else if (this->_badge_stamp_wtag_mtag.consObjectP()) {
    return false;
  }
  return false;
}

#endif

DOCGROUP(clasp);
CL_DEFUN core::T_mv gctools__multiple_values_ensure_valid(core::T_mv obj) {
  if (obj.generalp()) {
    client_validate_General_O_ptr(obj.unsafe_general());
  } else if (obj.consp()) {
    client_validate_Cons_O_ptr(obj.unsafe_cons());
  }
  core::MultipleValues& mvn = core::lisp_multipleValues();
  for (size_t ii = 1; ii < obj.number_of_values(); ++ii) {
    if (mvn.valueGet(ii, obj.number_of_values()).generalp()) {
      client_validate_General_O_ptr(mvn.valueGet(ii, obj.number_of_values()).unsafe_general());
    }
  }
  return obj;
}

DOCGROUP(clasp);
CL_DEFUN core::T_sp gctools__ensure_valid(core::T_sp obj) {
  if (obj.generalp()) {
    client_validate_General_O_ptr(obj.unsafe_general());
  } else if (obj.consp()) {
    client_validate_Cons_O_ptr(obj.unsafe_cons());
  }
  return obj;
}
}; // namespace gctools

namespace gctools {

/*! See NextStamp(...) definition in memoryManagement.h.
  global_NextBuiltInStamp starts at STAMP_max+1
  so that it doesn't use any stamps that correspond to KIND values
   assigned by the static analyzer. */
std::atomic<UnshiftedStamp> global_NextUnshiftedStamp(Header_s::StampWtagMtag::first_NextUnshiftedStamp(Header_s::max_clbind_stamp +
                                                                                                        1));
std::atomic<UnshiftedStamp>
    global_NextUnshiftedClbindStamp(Header_s::StampWtagMtag::first_NextUnshiftedStamp(Header_s::max_builtin_stamp + 1));

void OutOfStamps() {
  printf("%s:%d Hello future entity!  Congratulations! - you have run clasp long enough to run out of STAMPs - %lu are allowed - "
         "change the clasp header layout or add another word for the stamp\n",
         __FILE__, __LINE__, (uintptr_t)Header_s::largest_possible_stamp);
  abort();
}

void OutOfClbindStamps() {
  printf("%s:%d Hello future entity!  Congratulations! - you have added enough external libraries so that clasp has run out of "
         "clbind STAMPs - %lu are allowed - change the clasp header layout or add another word for the stamp\n",
         __FILE__, __LINE__, (uintptr_t)Header_s::max_clbind_stamp);
  abort();
}

void FinishAssingingBuiltinStamps() {
  DEPRECATED();
  size_t stamp = global_NextUnshiftedStamp.load();
  size_t nextGeneralStamp = Header_s::max_clbind_stamp + 1;
  printf("%s:%d:%s End of builtin stamps: %lu\n", __FILE__, __LINE__, __FUNCTION__, stamp);
  printf("%s:%d:%s First clbind stamp: %lu \n", __FILE__, __LINE__, __FUNCTION__, stamp + 1);
  printf("%s:%d:%s First general stamp: %lu\n", __FILE__, __LINE__, __FUNCTION__, nextGeneralStamp);
  global_NextUnshiftedClbindStamp.store(stamp + 1);
  global_NextUnshiftedStamp.store(nextGeneralStamp);
}

}; // namespace gctools

namespace gctools {
Tagged GCRootsInModule::setLiteral(size_t raw_index, Tagged val) {
  BOUNDS_ASSERT(raw_index < this->_capacity);
  BOUNDS_ASSERT(raw_index < this->_num_entries);
#if 0
  printf("%s:%d:%s setting literal raw_index = %lu  this->_module_memory = %p - turn off optnone\n",
         __FILE__, __LINE__, __FUNCTION__,
         raw_index, (void*)this->_module_memory );
#endif
  llvmo::JITDataReadWriteMaybeExecute();
  reinterpret_cast<core::T_O**>(this->_module_memory)[raw_index] = reinterpret_cast<core::T_O*>(val);
  llvmo::JITDataReadExecute();
  return val;
}
Tagged GCRootsInModule::getLiteral(size_t raw_index) {
  BOUNDS_ASSERT(raw_index < this->_capacity);
  BOUNDS_ASSERT(raw_index < this->_num_entries);
  return reinterpret_cast<Tagged>(reinterpret_cast<core::T_O**>(this->_module_memory)[raw_index]);
}

size_t GCRootsInModule::push_back(Tagged val) {
  size_t index = this->_num_entries;
  this->_num_entries++;
  this->setLiteral(index, val);
  return index;
}

Tagged GCRootsInModule::setTransient(size_t index, Tagged val) {
  if (this->_TransientAlloca) {
    core::SimpleVector_O* transients = *this->_TransientAlloca;
    if (transients) {
      BOUNDS_ASSERT(index < transients->length());
      core::T_sp tval((gctools::Tagged)val);
      if (transients) {
        GCROOT_LOG(("%s:%d:%s GCRootsInModule@%p[%lu] - writing %p of transient vector %p length: %lu\n", __FILE__, __LINE__,
                    __FUNCTION__, (void*)this, index, (void*)tval.tagged_(), (void*)transients.tagged_(), transients->length()));
        GCROOT_LOG(("     value -> %s\n", _rep_(tval).c_str()));
      } else {
        GCROOT_LOG(("%s:%d:%s GCRootsInModule@%p - writing %p to transient@%lu of transient vector %p BUT ITS NOT THERE!!!\n",
                    __FILE__, __LINE__, __FUNCTION__, (void*)this, (void*)tval.tagged_(), index, (void*)transients.tagged_()));
      }
      (*transients)[index] = tval;
      return val;
    }
    printf("%s:%d There is no transients vector\n", __FILE__, __LINE__);
    abort();
  }
  printf("%s:%d:%s The _TransientAlloca was NULL but index is %lu\n", __FILE__, __LINE__, __FUNCTION__, index);
  abort();
}

Tagged GCRootsInModule::getTransient(size_t index) {
  if (this->_TransientAlloca) {
    core::SimpleVector_O* transients = *this->_TransientAlloca;
    if (transients) {
      BOUNDS_ASSERT(index < transients->length());
      core::T_sp tval = (*transients)[index];
      if (transients) {
        GCROOT_LOG(("%s:%d:%s GCRootsInModule@%p[%lu] - read %p of transient vector %p length: %lu value-> %s\n", __FILE__,
                    __LINE__, __FUNCTION__, (void*)this, index, (void*)tval.tagged_(), (void*)transients.tagged_(),
                    transients->length(), _rep_(tval).c_str()));
      } else {
        GCROOT_LOG(("%s:%d:%s GCRootsInModule@%p - writing %p to transient@%lu of transient vector %p BUT ITS NOT THERE!!!\n",
                    __FILE__, __LINE__, __FUNCTION__, (void*)this, (void*)tval.tagged_(), index, (void*)transients.tagged_()));
      }
      return tval.tagged_();
    }
    printf("%s:%d There is no transients vector\n", __FILE__, __LINE__);
    abort();
  }
  printf("%s:%d:%s There _TransientAlloca is NULL index = %lu\n", __FILE__, __LINE__, __FUNCTION__, index);
  abort();
}

Tagged GCRootsInModule::setTaggedIndex(char tag, size_t index, Tagged val) {
  GCROOT_LOG(("%s:%d:%s GCRootsInModule@%p[%lu] tag '%d'\n", __FILE__, __LINE__, __FUNCTION__, (void*)this, index, tag));
  switch (tag) {
  case 'l':
  case LITERAL_TAG_CHAR: {
    return setLiteral(index, val);
  }
  case 't':
  case TRANSIENT_TAG_CHAR: {
    return setTransient(index, val);
  };
  };
  printf("%s:%d Illegal index %lu/0x%lx tag %c\n", __FILE__, __LINE__, index, index, tag);
  abort();
}

Tagged GCRootsInModule::getTaggedIndex(char tag, size_t index) {
  GCROOT_LOG(("%s:%d:%s GCRootsInModule@%p[%lu] tag '%d'\n", __FILE__, __LINE__, __FUNCTION__, (void*)this, index, tag));
  switch (tag) {
  case 'l':
  case LITERAL_TAG_CHAR: {
    return getLiteral(index);
  }
  case 't':
  case TRANSIENT_TAG_CHAR: {
    return getTransient(index);
  };
  };
  printf("%s:%d Illegal index %lu/0x%lx tag %c\n", __FILE__, __LINE__, index, index, tag);
  abort();
}

void* GCRootsInModule::lookup_function(size_t index) {
  if (index < this->_function_pointer_count) {
    return (void*)this->_function_pointers[index];
  }
  printf("%s:%d Illegal function pointer index %lu must be less than %lu\n", __FILE__, __LINE__, index,
         this->_function_pointer_count);
  abort();
}

}; // namespace gctools

namespace gctools {

/* Walk all of the roots, passing the address of each root and what it represents */
template <typename RootWalkCallback>
void walkRoots(RootWalkCallback&& callback) {
  callback((Tagged*)&_lisp);
  for (size_t jj = 0; jj < global_symbol_count; ++jj) {
    callback((Tagged*)&global_symbols[jj]);
  }
};

#define GENERAL_PTR_TO_HEADER_PTR(_general_) GeneralPtrToHeaderPtr((void*)_general_)
// #define HEADER_PTR_TO_GENERAL_PTR(_header_) headerPointerToGeneralPointer((gctools::Header_s*)_header_)

#define ADDR_T uintptr_t
#define EXTRA_ARGUMENTS , Tagged containingObject, std::stack<std::pair<Tagged, Tagged>>& markStack

#define POINTER_FIX(_ptr_) markStack.emplace(containingObject, *reinterpret_cast<Tagged*>(_ptr_))

#define OBJECT_SCAN mw_obj_scan
#define OBJECT_SKIP mw_obj_skip
#define EPHEMERON_FIX(a, b)
#include "obj_scan.cc"
#undef OBJECT_SKIP
#undef OBJECT_SCAN

#define CONS_SCAN mw_cons_scan
#define CONS_SKIP mw_cons_skip
#include "cons_scan.cc"
#undef CONS_SKIP
#undef CONS_SCAN

#undef POINTER_FIX
#undef ADDR_T
#undef EXTRA_ARGUMENTS

template <class Callback>
static void mapAllObjectsInternal(std::set<Tagged>& markSet,
                                  Callback callback) {
  std::stack<std::pair<Tagged, Tagged>> markStack;

  // process all roots
  walkRoots([&](Tagged* rootf) { markStack.emplace(0, *rootf); });

  while (!markStack.empty()) {
    // pop an object. we don't need the containing object.
    Tagged tagged = markStack.top().second; markStack.pop();

    switch(ptag(tagged)) {
    case general_tag: { // general object
      if (!markSet.contains(tagged)) { // only process each object once
        markSet.insert(tagged);
        uintptr_t client = untag_object(tagged);
        Header_s* header = (Header_s*)GeneralPtrToHeaderPtr((void*)client);
        // the mw_foo_scan functions push all fields of the object
        // onto the markStack to keep the loop going.
        mw_obj_scan(client, tagged, markStack);
        // now's the time to callback. Could also go before the scan
        callback(tagged);
      }
    } break;
    case cons_tag: {
      if (!markSet.contains(tagged)) {
        markSet.insert(tagged);
        uintptr_t client = untag_object(tagged);
        mw_cons_scan(client, tagged, markStack);
        callback(tagged);
      }
    } break;
    default: break; // immediate, vaslist, etc.: ignore
    }
  }
}

void mapAllObjects(void (*callback)(Tagged, void*), void* data) {
  std::set<Tagged> markSet;
  mapAllObjectsInternal(markSet, [&](Tagged obj) { callback(obj, data); });
}

// Used in snapshot save to avoid walking memory repeatedly.
std::set<Tagged> setOfAllObjects() {
  std::set<Tagged> markSet;
  mapAllObjectsInternal(markSet, [](Tagged) {});
  return markSet; // hoping for NRVO optimization, i guess.
}

// Check that all fields in all objects point to valid objects.
// Also check for functions that can't be resolved with dlsym, since that's
// important for snapshot save.
// Return the set of corrupt fields, represented as pairs of a non-corrupt
// object and the address of a field within that object; the object contained
// in that field is corrupt.
// If a corrupt object is accessible in multiple fields, only one field containing
// it is returned.
std::set<std::pair<Tagged, Tagged>> memtest(std::set<core::T_sp, T_sp_less>& dladdrFailed) {
  std::stack<std::pair<Tagged, Tagged>> markStack;
  std::set<Tagged> markSet;
  std::set<std::pair<Tagged, Tagged>> corrupt;

  std::set<void*> uniqueEntryPoints;

  walkRoots([&](Tagged* rootAddr) { markStack.emplace(0, *rootAddr); });

  while (!markStack.empty()) {
    auto p = markStack.top(); markStack.pop();
    Tagged containingObject = p.first;
    Tagged tagged = p.second;

    switch (tagged & ptag_mask) {
    case general_tag: {
      if (!markSet.contains(tagged)) {
        markSet.insert(tagged);
        uintptr_t client = tagged & ptr_mask;
        Header_s* header = (Header_s*)GeneralPtrToHeaderPtr((void*)client);
        if (header->isValidGeneralObject()) {
          mw_obj_scan(client, tagged, markStack);
          // If this is a function, check its dladdrability.
          core::T_sp tobj(tagged);
          if (tobj.isA<core::SimpleFun_O>()) {
            auto sfun = tobj.as_unsafe<core::SimpleFun_O>();
            if (!sfun->dladdrablep(uniqueEntryPoints))
              dladdrFailed.insert(tobj);
          }
        } else {
          corrupt.emplace(containingObject, tagged);
        }
      }
    } break;
    case cons_tag: {
      if (!markSet.contains(tagged)) {
        markSet.insert(tagged);
        uintptr_t client = tagged & ptr_mask;
        ConsHeader_s* header = (ConsHeader_s*)ConsPtrToHeaderPtr((void*)client);
        if (header->isValidConsObject())
          mw_cons_scan(client, tagged, markStack);
        else {
          corrupt.emplace(containingObject, tagged);
        }
      }
    } break;
    case vaslist0_tag:
#if TAG_BITS == 4
    case vaslist1_tag:
#endif
        break; // not checked presently - FIXME?
    case fixnum00_tag:
    case fixnum01_tag:
#if TAG_BITS == 4
    case fixnum10_tag:
    case fixnum11_tag:
#endif
    case character_tag:
    case single_float_tag:
#ifdef CLASP_SHORT_FLOAT
    case short_float_tag:
#endif
    case UNBOUND_TAG: // FIXME: put const definition in pointer_tagging.h somewhere?
        break; // immediate, nothing to do
    default: // unknown tag - object is corrupt
        corrupt.emplace(containingObject, tagged); break;
    }
  }
  return corrupt;
}

/* Return the size of the object */
size_t objectSize(BaseHeader_s* header) {
  if (header->_badge_stamp_wtag_mtag.consObjectP()) {
    // It's a cons object
    size_t consSize;
    uintptr_t client = (uintptr_t)HeaderPtrToConsPtr(header);
    [[maybe_unused]] uintptr_t clientLimit = mw_cons_skip(client, consSize);
    return consSize;
  } else {
    // It's a general object - walk it
    size_t objectSize;
    uintptr_t client = (uintptr_t)HeaderPtrToGeneralPtr<void*>(header);
    mw_obj_skip(client, objectSize);
    return objectSize;
  }
}

}; // namespace gctools

namespace gctools {

gctools::BaseHeader_s::badge_t lisp_general_badge(core::General_sp object) {
  const gctools::Header_s* header = gctools::header_pointer(object.unsafe_general());
  gctools::BaseHeader_s::badge_t read_badge = header->_badge_stamp_wtag_mtag._header_badge.load();
  if (read_badge == gctools::BaseHeader_s::BadgeStampWtagMtag::NoBadge) {
    gctools::BaseHeader_s::badge_t expected_badge = gctools::BaseHeader_s::BadgeStampWtagMtag::NoBadge;
    gctools::BaseHeader_s::badge_t badge = lisp_calculate_heap_badge();
    if (!header->_badge_stamp_wtag_mtag._header_badge.compare_exchange_strong(expected_badge, badge)) {
      return expected_badge;
    }
    return badge;
  }
  return read_badge;
}

gctools::BaseHeader_s::badge_t lisp_cons_badge(core::Cons_sp object) {
  const gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(object.unsafe_cons());
  gctools::BaseHeader_s::badge_t read_badge = header->_badge_stamp_wtag_mtag._header_badge.load();
  if (read_badge == gctools::BaseHeader_s::BadgeStampWtagMtag::NoBadge) {
    gctools::BaseHeader_s::badge_t expected_badge = gctools::BaseHeader_s::BadgeStampWtagMtag::NoBadge;
    gctools::BaseHeader_s::badge_t badge = lisp_calculate_heap_badge();
    if (!header->_badge_stamp_wtag_mtag._header_badge.compare_exchange_strong(expected_badge, badge)) {
      return expected_badge;
    }
    return badge;
  }
  return read_badge;
}

uint32_t lisp_badge(core::T_sp object) {
  if (object.consp()) {
    core::Cons_sp cobject = gc::As_unsafe<core::Cons_sp>(object);
    return lisp_cons_badge(cobject);
  } else if (object.generalp()) {
    return lisp_general_badge(gc::As_unsafe<core::General_sp>(object));
  } else
    return 0;
}

uint32_t lisp_calculate_heap_badge() {
  if (!my_thread)
    return 123456;
  return my_thread->random();
}

}; // namespace gctools
