/*
    File: mpsGarbageCollection.cc
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

/*
struct MemoryCode {
Cmd _cmd;
size_t _data;
const char* _description;
}

enum Cmd {
class_kind, class_size, field_fix,
container_kind, container_jump_table_index,
templated_class_kind, templated_class_jump_table_index,
layout_end
};
class_kind, kind_value, name
class_size, size, NULL
field_fix, offset_words, name
container_kind, kind_value, name
container_jump_table_index, jump_table_index, NULL
templated_class_kind, kind_value, name
templated_class_jump_table_index, jump_table_index, NULL


*/
//#define MPS_LOVEMORE 1

#include <clasp/core/foundation.h>

#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/posixTime.h> // was core/posixTime.cc???
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/globals.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/mpPackage.h>
#include <clasp/gctools/gc_interface.fwd.h>

#ifdef USE_MPS

extern "C" {
#include "clasp/mps/code/mpscawl.h" // MVFF pool
#include "clasp/mps/code/mpscmvff.h" // MVFF pool
#include "clasp/mps/code/mpscamc.h" // AMC pool
#include "clasp/mps/code/mpscsnc.h" // SNC pool
#include <clasp/gctools/mygc.h>
};

#include <clasp/gctools/gctoolsPackage.h>

extern "C" {
struct PointerSearcher {
  PointerSearcher() : poolObjects(0){};
  int poolObjects;
  int stackAddresses;
  vector<mps_addr_t> poolMatches;
  vector<mps_addr_t> stackMatches;
};

void pointerSearcherAddRef(PointerSearcher *searcher, mps_addr_t ref) {
  searcher->poolMatches.push_back(ref);
}

extern void memory_find_ref(mps_arena_t arena, mps_addr_t ref, PointerSearcher *searcher);
};


extern "C" {
mps_arena_t global_arena;
};

namespace gctools {

MpsMetrics globalMpsMetrics;

/* --------------------------------------------------
   --------------------------------------------------
   --------------------------------------------------
   Global variables for MPS
   --------------------------------------------------
   --------------------------------------------------
*/

#ifdef DEBUG_MPS_UNDERSCANNING
bool global_underscanning = DEBUG_MPS_UNDERSCANNING_INITIAL;
#else
bool global_underscanning = false;
#endif

//    mps_pool_t _global_mvff_pool;
mps_pool_t _global_amc_pool;
mps_pool_t global_amc_cons_pool;
mps_pool_t _global_amcz_pool;
mps_pool_t _global_awl_pool;
mps_ap_t _global_weak_link_allocation_point;
mps_ap_t _global_strong_link_allocation_point;
//    mps_ap_t _global_mvff_allocation_point;
mps_ap_t _global_automatic_mostly_copying_zero_rank_allocation_point;
mps_ap_t _global_automatic_mostly_copying_allocation_point;
mps_ap_t global_amc_cons_allocation_point;

mps_pool_t global_non_moving_pool;
mps_pool_t global_unmanaged_pool;
mps_ap_t global_non_moving_ap;
size_t global_sizeof_fwd;

};

namespace gctools {
struct root_list {
  mps_root_t _root;
  root_list* _next;
  root_list(mps_root_t root, root_list* next) :_root(root), _next(next) {};
};

root_list* global_root_list = NULL;

};

extern "C" {
// The following is defined in mygc.c
mps_res_t clasp_scan_area_tagged(mps_ss_t ss,
                                 void* base, void* limit,
                                 void* closure);
};

namespace gctools {
void mps_register_roots(void* roots_begin, size_t num_roots) {
  mps_root_t mps_root;
  mps_res_t res;
  void* roots_end = reinterpret_cast<void*>(reinterpret_cast<char*>(roots_begin)+num_roots*sizeof(core::T_sp));
//  printf("%s:%d About to mps_root_create_area_tagged %" PRu " roots -> roots_begin@%p roots_end@%p\n", __FILE__, __LINE__, num_roots, roots_begin, roots_end );
  res = mps_root_create_area_tagged(&mps_root,
                                    global_arena,
                                    mps_rank_exact(),
                                    0, // DLM suggested this because MPS_RM_PROT will have problems with two roots on same memory page
                                    roots_begin,
                                    roots_end,
                                    clasp_scan_area_tagged,
                                    gctools::tag_mask,  // #b111
                                    0 ); // DLM says this will be ignored
  if ( res != MPS_RES_OK ) {
    SIMPLE_ERROR(BF("Could not mps_root_create_area_tagged - error: %d") % res );
  }
  // Save the root list in a linked list
  root_list* rl = new root_list(mps_root, global_root_list);
  global_root_list = rl;
}

// Delete all of the mps roots.
// This happens in reverse order of how they were created because - you know - linked list.
void delete_my_roots() {
  root_list* rl = global_root_list;
  while (rl) {
    mps_root_destroy(rl->_root);
    rl = rl->_next;
  }
  global_root_list = NULL;
};

};

namespace gctools {

string gcResultToString(GC_RESULT res) {
  switch (res) {
  case MPS_RES_OK:
    return "operation succeeded.";
  case MPS_RES_FAIL:
    return "operation failed.";
  case MPS_RES_IO:
    return "an input/output error occurred.";
  case MPS_RES_LIMIT:
    return "an internal limitation was exceeded.";
  case MPS_RES_MEMORY:
    return "needed memory could not be obtained.";
  case MPS_RES_RESOURCE:
    return "a needed resource could not be obtained.";
  case MPS_RES_UNIMPL:
    return "operation is not implemented.";
  case MPS_RES_COMMIT_LIMIT:
    return "the arena’s commit limit would be exceeded.";
  case MPS_RES_PARAM:
    return "an invalid parameter was passed.";
  default:
    return "Unknown GC_RESULT";
  };
};

void searchMemoryForAddress(mps_addr_t addr) {
  PointerSearcher searcher;
  //        memory_find_ref(global_arena, addr, &searcher );

  // Search the stack
  const char* sptr = reinterpret_cast<const char *>(&searcher) + 1;
  for (; sptr < _global_stack_marker; ++sptr) {
    if (*sptr == reinterpret_cast<uintptr_clasp_t>(addr)) {
      searcher.stackMatches.push_back(reinterpret_cast<mps_addr_t>(const_cast<char*>(sptr)));
    }
    ++(searcher.stackAddresses);
  }
  printf("Searched through %d pool objects\n", searcher.poolObjects);
  for (auto hit : searcher.poolMatches) {
    printf("addr=%p found in pool at  @%p\n", addr, hit);
  }
  printf("Searched through %d stack addresses\n", searcher.stackAddresses);
  for (auto sit : searcher.poolMatches) {
    printf("addr=%p found on stack at @%p\n", addr, sit);
  }
  printf("--------------------------- Search done\n");
};

#define GC_RESULT_ERROR(res, msg)                                     \
  {                                                                   \
    string error = gcResultToString(res);                             \
    THROW_HARD_ERROR(BF("GC_RESULT error: %s   %s\n") % error % msg); \
  }

static void obj_fwd(mps_addr_t old_client, mps_addr_t new_client) {
  // I'm assuming both old and new client pointers have valid headers at this point
  DEBUG_THROW_IF_INVALID_CLIENT(old_client);
  DEBUG_THROW_IF_INVALID_CLIENT(new_client);
  mps_addr_t limit = obj_skip(old_client);
  size_t size = (char *)limit - (char *)old_client;
  if (size < global_sizeof_fwd) {
    THROW_HARD_ERROR(BF("obj_fwd needs size >= %u") % global_sizeof_fwd);
  }
  Header_s *header = reinterpret_cast<Header_s *>(const_cast<void *>(ClientPtrToBasePtr(old_client)));
  header->setFwdSize(size);
  header->setFwdPointer(new_client);
}

static mps_addr_t obj_isfwd(mps_addr_t client) {
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  const Header_s *header = reinterpret_cast<const Header_s *>(ClientPtrToBasePtr(client));
  if (header->fwdP()) return header->fwdPointer();
  return NULL;
}

static void obj_pad(mps_addr_t base, size_t size) {
  size_t alignment = Alignment();
  assert(size >= alignment);
  Header_s *header = reinterpret_cast<Header_s *>(base);
  if (size == alignment) {
    header->setPad(Header_s::pad1_tag);
  } else {
    header->setPad(Header_s::pad_tag);
    header->setPadSize(size);
  }
}


GC_RESULT cons_scan(mps_ss_t ss, mps_addr_t client, mps_addr_t limit) {
//  printf("%s:%d in cons_scan\n", __FILE__, __LINE__ );
  mps_addr_t original_client = client;
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    while (client<limit) {
      core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
      if ( !cons->hasGcTag() ) {
#if DEBUG_VALIDATE_GUARD
        client_validate(cons->_Car.raw_());
        client_validate(cons->_Cdr.raw_());
#endif
        core::T_O* old_car = cons->_Car.raw_();
        SMART_PTR_FIX(cons->_Car);
        SMART_PTR_FIX(cons->_Cdr);
        client = reinterpret_cast<mps_addr_t>((char*)client+sizeof(core::Cons_O));
      } else if (cons->fwdP()) {
        client = (char *)(client) + sizeof(core::Cons_O);
      } else if (cons->pad1P()) {
        client = (char *)(client) + Alignment();
      } else if (cons->padP()) {
        client = (char *)(client) + cons->padSize();
      } else {
        printf("%s:%d CONS in cons_scan (it's not a CONS or any of MPS fwd/pad1/pad2 car=%p cdr=%p\n", __FILE__, __LINE__, cons->_Car.raw_(), cons->_Cdr.raw_());
        abort();
      }
    };
  } MPS_SCAN_END(GC_SCAN_STATE);
  return MPS_RES_OK;
};

mps_addr_t cons_skip(mps_addr_t client) {
//  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  mps_addr_t oldClient = client;
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
  if ( cons->pad1P() ) {
    client = reinterpret_cast<mps_addr_t>((char*)client+Alignment());
  } else if (cons->padP() ) {
    client = reinterpret_cast<mps_addr_t>((char*)client + cons->padSize());
  } else {
    client = reinterpret_cast<mps_addr_t>((char*)client + sizeof(core::Cons_O));
  }
  return client;
}


static void cons_fwd(mps_addr_t old_client, mps_addr_t new_client) {
//  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  // I'm assuming both old and new client pointers have valid headers at this point
  mps_addr_t limit = cons_skip(old_client);
  size_t size = (char *)limit - (char *)old_client;
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(old_client);
  cons->setFwdPointer(new_client);
}

static mps_addr_t cons_isfwd(mps_addr_t client) {
//  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
  if (cons->fwdP()) {
    return cons->fwdPointer();
  }
  return NULL;
}

static void cons_pad(mps_addr_t base, size_t size) {
//  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  size_t alignment = Alignment();
  assert(size >= alignment);
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(base);
  if (size == alignment) {
    cons->setPad1();
  } else {
    cons->setPad(size);
  }
}

// -----------------------------------------------------------
// -----------------------------------------------------------
//
// Code to deal with the thread local side-stacks
//
//
#if 0
static mps_res_t stack_frame_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit) {
  STACK_TELEMETRY2(telemetry::label_stack_frame_scan_start,
                   (uintptr_clasp_t)base,
                   (uintptr_clasp_t)limit);
  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      mps_addr_t original_base = base;
      uintptr_clasp_t *headerAndFrame = (uintptr_clasp_t *)base;
      GCStack::frameType ftype = (GCStack::frameType)FRAME_HEADER_TYPE_FIELD(headerAndFrame);
      size_t sz = FRAME_HEADER_SIZE_FIELD(headerAndFrame);
      switch (ftype) {
      case GCStack::frame_t: {
        uintptr_clasp_t *frameStart = FRAME_START(headerAndFrame);
        size_t elements = frameStart[gc::IdxNumElements];
        uintptr_clasp_t *taggedPtr = &frameStart[gc::IdxValuesArray];
        for (size_t i = 0; i < elements; ++i) {
          taggedPtrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(taggedPtr));
          ++taggedPtr;
        }
        base = (char *)base + sz;
      } break;
      case GCStack::pad_t:
        base = (char *)base + sz;
        break;
      default:
          printf("%s:%d stack_frame_scan Unexpected object on side-stack\n", __FILE__, __LINE__ );
          abort();
      }
      STACK_TELEMETRY3(telemetry::label_stack_frame_scan,
                       (uintptr_clasp_t)original_base,
                       (uintptr_clasp_t)base,
                       (uintptr_clasp_t)ftype);
    }
  }
  MPS_SCAN_END(ss);
  return MPS_RES_OK;
};

static mps_addr_t stack_frame_skip(mps_addr_t base) {
  uintptr_clasp_t *headerAndFrame = (uintptr_clasp_t *)base;
  mps_addr_t original_base = base;
  GCStack::frameType ftype = (GCStack::frameType)FRAME_HEADER_TYPE_FIELD(headerAndFrame);
  size_t sz = FRAME_HEADER_SIZE_FIELD(headerAndFrame);
  switch (ftype) {
  case GCStack::frame_t:
    base = (char *)base + sz;
    break;
  case GCStack::pad_t:
    base = (char *)base + sz;
    break;
  default:
      printf("%s:%d stack_frame_skip Unexpected object on side-stack\n", __FILE__, __LINE__ );
      abort();
      break;
  }
  STACK_TELEMETRY3(telemetry::label_stack_frame_skip,
                   (uintptr_clasp_t)original_base,
                   (uintptr_clasp_t)base,
                   (uintptr_clasp_t)((char *)original_base - (char *)base));
  return base;
}

static void stack_frame_pad(mps_addr_t addr, size_t size) {
  STACK_TELEMETRY2(telemetry::label_stack_frame_pad,
                   (uintptr_clasp_t)addr,
                   (uintptr_clasp_t)size);
  uintptr_clasp_t *obj = reinterpret_cast<uintptr_clasp_t *>(addr);
  GCTOOLS_ASSERT(size >= STACK_ALIGN_UP(sizeof(uintptr_clasp_t)));
  FRAME_HEADER_TYPE_FIELD(obj) = (int)GCStack::frameType::pad_t;
  FRAME_HEADER_SIZE_FIELD(obj) = size;
}


void mpsAllocateStack(gctools::GCStack *stack) {
  mps_res_t res;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, STACK_ALIGNMENT);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, stack_frame_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, stack_frame_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, stack_frame_pad);
    res = mps_fmt_create_k(&stack->_ObjectFormat, global_arena, args);
    if (res != MPS_RES_OK)
      THROW_HARD_ERROR(BF("Couldn't create stack frame format"));
  }
  MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, stack->_ObjectFormat);
    res = mps_pool_create_k(&stack->_Pool, global_arena, mps_class_snc(), args);
    if (res != MPS_RES_OK)
      THROW_HARD_ERROR(BF("Couldn't create stack frame pool"));
  }
  MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
    res = mps_ap_create_k(&stack->_AllocationPoint, stack->_Pool, args);
    if (res != MPS_RES_OK)
      THROW_HARD_ERROR(BF("Couldn't create stack frame allocation point"));
  }
  MPS_ARGS_END(args);
  stack->_IsActive = true;
};

void mpsDeallocateStack(gctools::GCStack *stack) {
  if (stack->_TotalSize != 0) {
    THROW_HARD_ERROR(BF("mpsDeallocateStack called on a stack that is not completely empty - it contains %u bytes") % stack->_TotalSize);
  }
  stack->_IsActive = false;
  mps_arena_park(global_arena);
  mps_ap_destroy(stack->_AllocationPoint);
  mps_pool_destroy(stack->_Pool);
  mps_fmt_destroy(stack->_ObjectFormat);
  mps_arena_release(global_arena);
  //  printf("%s:%d deallocateStack\n", __FILE__, __LINE__ );
};
#endif

};

// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------

extern "C" {

size_t global_finalization_requests = 0;
void my_mps_finalize(void* client) {
  mps_finalize(global_arena,&client);
  ++gctools::globalMpsMetrics.finalizationRequests;
  ++global_finalization_requests;
  if (global_finalization_requests>16) {
    size_t finalizations;
    processMpsMessages(finalizations);
    global_finalization_requests = 0;
  }
}

size_t processMpsMessages(size_t& finalizations) {
  size_t messages(0);
  finalizations = 0;
  int mGcStart(0);
  int mGc(0);
  core::Number_sp startTime = gc::As<core::Number_sp>(core::cl__get_internal_run_time());
  mps_message_type_t type;
  while (mps_message_queue_type(&type, global_arena)) {
    mps_message_t message;
    mps_bool_t b;
    b = mps_message_get(&message, global_arena, type);
    ++messages;
    assert(b); /* we just checked there was one */
    if (type == mps_message_type_gc_start()) {
      ++mGcStart;
    } else if (type == mps_message_type_gc()) {
      ++mGc;
#if 0
                printf("Message: mps_message_type_gc()\n");
                size_t live = mps_message_gc_live_size(global_arena, message);
                size_t condemned = mps_message_gc_condemned_size(global_arena, message);
                size_t not_condemned = mps_message_gc_not_condemned_size(global_arena, message);
                printf("Collection finished.\n");
                printf("    live %" PRu "\n", (unsigned long)live);
                printf("    condemned %" PRu "\n", (unsigned long)condemned);
                printf("    not_condemned %" PRu "\n", (unsigned long)not_condemned);
                printf("    clock: %" PRu "\n", (unsigned long)mps_message_clock(global_arena, message));
#endif
    } else if (type == mps_message_type_finalization()) {
      ++finalizations;
      //                printf("%s:%d mps_message_type_finalization received\n", __FILE__, __LINE__);
      mps_addr_t ref_o;
      mps_message_finalization_ref(&ref_o, global_arena, message);
      // Figure out what pool the pointer belonged to and recreate the
      //   original tagged object pointer
      //   For general objects the object may already have been destructed (dead_object)
      //   and replaced with a PAD tag - if so - skip the whole finalization process
      mps_pool_t pool = clasp_pool_of_addr(ref_o);
      core::T_sp obj;
      bool dead_object = false;
      if (pool) {
        if (pool == gctools::global_amc_cons_pool) {
          obj = gctools::smart_ptr<core::Cons_O>((core::Cons_O*)ref_o);
        } else {
          gctools::Header_s* header = (gctools::Header_s*)((char*)ref_o - sizeof(gctools::Header_s));
          dead_object = !(header->stampP());
          obj = gctools::smart_ptr<core::T_O>((core::T_O*)ref_o);
        }
      } else {
        printf("%s:%d   MPS could not figure out what pool the pointer %p belongs to - treating it like a dead object and no finalizer will be invoked\n", __FILE__, __LINE__, ref_o);
        dead_object = true;
      }
      printf("%s:%d finalization message for %p reconstituted tagged ptr = %p  dead_object -> %d\n", __FILE__, __LINE__, (void*)ref_o, (void*)obj.tagged_(), dead_object );
#if 1
      if (!dead_object) {
        bool invoked_finalizer = false;
        auto ht = gctools::As<core::WeakKeyHashTable_sp>(gctools::_sym_STARfinalizersSTAR->symbolValue());
        core::T_mv res = ht->gethash(obj);
        if (res.second().notnilp()) {
          printf("%s:%d           Trying to pass object %p to finalizer at %p\n", __FILE__, __LINE__, (void*)obj.tagged_(), (void*)res.tagged_());
          core::List_sp finalizers = res;
          for ( auto cur : finalizers ) {
            core::T_sp finalizer = oCar(cur);
            core::eval::funcall(finalizer,obj);
            printf("%s:%d Ran finalizer callback.\n", __FILE__, __LINE__ );
          }
          ht->remhash(obj);
          invoked_finalizer = true;
        }
#endif
        if (!invoked_finalizer && obj.generalp()) obj_finalize(ref_o);
      }
    } else {
      printf("Message: UNKNOWN!!!!!\n");
    }
    mps_message_discard(global_arena, message);
  }
#if 0
//        printf("%s:%d Leaving processMpsMessages\n",__FILE__,__LINE__);
        core::Number_sp endTime = core::cl__get_internal_run_time().as<core::Number_O>();
        core::Number_sp deltaTime = core::contagen_mul(core::contagen_sub(endTime,startTime),core::make_fixnum(1000));
        core::Number_sp deltaSeconds = core::contagen_div(deltaTime,cl::_sym_internalTimeUnitsPerSecond->symbolValue().as<core::Number_O>());
        printf("%s:%d [processMpsMessages %s millisecs for  %d finalization/ %d gc-start/ %d gc messages]\n", __FILE__, __LINE__, _rep_(deltaSeconds).c_str(), mFinalize, mGcStart, mGc );
        fflush(stdout);
#endif
  return messages;
};
};

namespace gctools {

void test_mps_allocation() {
  int numAllocations = 10000;
  printf("Starting test_mps_allocation -> allocating %d objects\n", numAllocations);
  size_t finalizations;
  for (int i = 0; i < numAllocations; ++i) {
    core::SimpleBaseString_sp ss = core::SimpleBaseString_O::make("Hi there, this is a test");
    processMpsMessages(finalizations);
  }
  printf("Done test_mps_allocation - allocated %d objects\n", numAllocations);
}

mps_addr_t awlFindDependent(mps_addr_t other) {
  gctools::WeakObject *wo = reinterpret_cast<gctools::WeakObject *>(other);
  return reinterpret_cast<mps_addr_t>(wo->dependentPtr());
}

/* -------------------------------------------------- */

mps_addr_t dummyAwlFindDependent(mps_addr_t addr) {
  return NULL;
}

// The defaults are
//     #define CHAIN_SIZE 6400 // 256 // 6400
//     size_t arenaSizeMb = 320;
//     size_t spareCommitLimitMb = 320;
//     size_t nurseryKb = CHAIN_SIZE;
//     size_t nurseryMortalityPercent = 80;
//     size_t generation1Kb = CHAIN_SIZE*4;
//     size_t generation1MortalityPercent = 50;
// Try something like
// export CLASP_MPS_CONFIG="32 32 16 80 32 80 64"
// to debug MPS
bool maybeParseClaspMpsConfig(size_t &arenaMb, size_t &spareCommitLimitMb, size_t &nurseryKb, size_t &nurseryMortalityPercent, size_t &generation1Kb, size_t &generation1MortalityPercent, size_t &keyExtendByKb ) {
  char *cur = getenv("CLASP_MPS_CONFIG");
  size_t values[20];
  int numValues = 0;
  if (cur) {
    printf("Default CLASP_MPS_CONFIG = %" PRu " %lu %lu %lu %lu %lu %lu\n",
           arenaMb,
           spareCommitLimitMb,
           nurseryKb,
           nurseryMortalityPercent,
           generation1Kb,
           generation1MortalityPercent,
           keyExtendByKb );
    printf("Changed to CLASP_MPS_CONFIG = %s\n", cur);
    while (*cur && numValues < 20) {
      values[numValues] = strtol(cur, &cur, 10);
      ++numValues;
    }
    if (numValues > 7)
      numValues = 7;
    switch (numValues) {
    case 7:
        keyExtendByKb = values[6];
    case 6:
        generation1MortalityPercent = values[5];
    case 5:
        generation1Kb = values[4];
    case 4:
        nurseryMortalityPercent = values[3];
    case 3:
        nurseryKb = values[2];
    case 2:
        spareCommitLimitMb = values[1];
    case 1:
        arenaMb = values[0];
        printf("CLASP_MPS_CONFIG...\n");
        printf("                    arenaMb = %" PRu "\n", arenaMb);
        printf("         spareCommitLimitMb = %" PRu "\n", spareCommitLimitMb);
        printf("                  nurseryKb = %" PRu "\n", nurseryKb);
        printf("    nurseryMortalityPercent = %" PRu "\n", nurseryMortalityPercent);
        printf("              generation1Kb = %" PRu "\n", generation1Kb);
        printf("generation1MortalityPercent = %" PRu "\n", generation1MortalityPercent);
        printf("              keyExtendByKb = %" PRu "\n", keyExtendByKb );
        return true;
        break;
    default:
        break;
    };
  }
  return false;
}


void run_quick_tests()
{
  core::List_sp l1 = core::Cons_O::create(core::clasp_make_fixnum(1),_Nil<core::T_O>());
  core::List_sp l2 = core::Cons_O::create(core::clasp_make_fixnum(1),l1);
  core::List_sp l3 = core::Cons_O::create(core::clasp_make_fixnum(1),l2);
  core::List_sp l4 = core::Cons_O::create(core::clasp_make_fixnum(1),l3);
}

#define LENGTH(array) (sizeof(array) / sizeof(array[0]))

int initializeMemoryPoolSystem(MainFunctionType startupFn, int argc, char *argv[], bool mpiEnabled, int mpiRank, int mpiSize) {
  if (Alignment() == 16) {
    //            printf("%s:%d WARNING   Alignment is 16 - it should be 8 - check the Alignment() function\n!\n!\n!\n!\n",__FILE__,__LINE__);
  }
  global_sizeof_fwd = AlignUp(sizeof(Header_s));
//  global_alignup_sizeof_header = AlignUp(sizeof(Header_s));

#define CHAIN_SIZE 6400*5 // 256 // 6400
  size_t arenaSizeMb = 1600;
  size_t spareCommitLimitMb = 320;
  size_t nurseryKb = CHAIN_SIZE;
  size_t nurseryMortalityPercent = 80;
  size_t generation1Kb = CHAIN_SIZE * 4;
  size_t generation1MortalityPercent = 50;
  size_t keyExtendByKb = 64;  // 64K

  // Try something like   export CLASP_MPS_CONFIG="32 32 16 80 32 80 64"   to debug MPS
  maybeParseClaspMpsConfig(arenaSizeMb, spareCommitLimitMb, nurseryKb, nurseryMortalityPercent, generation1Kb, generation1MortalityPercent, keyExtendByKb );

  double nurseryMortalityFraction = nurseryMortalityPercent / 100.0;
  double generation1MortalityFraction = generation1MortalityPercent / 100.0;

//        printf( "arenaSizeMb[%" PRu "] spareCommitLimitMb[%lu] nurseryKb[%lu] nurseryMortalityFraction[%f] generation1Kb[%lu] generation1MortalityFraction[%f]\n", arenaSizeMb, spareCommitLimitMb, nurseryKb, nurseryMortalityFraction, generation1Kb, generation1MortalityFraction );

#define AMC_CHAIN_SIZE CHAIN_SIZE
  // Now the generation chain
  mps_gen_param_s gen_params[] = {
      {nurseryKb, nurseryMortalityFraction},         // { Nursery_size, Nursery_mortality }
      {generation1Kb, generation1MortalityFraction}, // { Generation1_size, Generation1_mortality }
  };

  mps_res_t res;
  MPS_ARGS_BEGIN(args) {
#ifdef MPS_LOVEMORE
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_INCREMENTAL, 0);
#endif
    MPS_ARGS_ADD(args, MPS_KEY_PAUSE_TIME, 100.0); // accept up to 0.1 seconds pause time
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, arenaSizeMb * 1024 * 1024);
    res = mps_arena_create_k(&global_arena, mps_arena_class_vm(), args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create MPS arena");

  // David suggested this - it never gives back memory to the OS
  mps_arena_spare_commit_limit_set(global_arena, spareCommitLimitMb * 1024 * 1024);

  mps_fmt_t obj_fmt;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_HEADER_SIZE, sizeof(Header_s));
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, Alignment());
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, obj_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
    res = mps_fmt_create_k(&obj_fmt, global_arena, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create obj format");

  mps_chain_t general_chain;
  res = mps_chain_create(&general_chain,
                         global_arena,
                         LENGTH(gen_params),
                         gen_params);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create amc chain");

#ifdef DEBUG_MPS_FENCEPOST_FREE
  mps_pool_debug_option_s debug_options = {
      "fencepost", 9,
      "free", 4,
  };
#endif

  // Create the AMC pool
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, general_chain);
#ifdef DEBUG_MPS_FENCEPOST_FREE
    MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options);
#endif
    MPS_ARGS_ADD(args, MPS_KEY_INTERIOR, 1);
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY,keyExtendByKb*1024);
    MPS_ARGS_ADD(args, MPS_KEY_LARGE_SIZE,keyExtendByKb*1024);
    res = mps_pool_create_k(&_global_amc_pool, global_arena, mps_class_amc(), args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create amc pool");



// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
  // Pool for CONS objects

  mps_fmt_t cons_fmt;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, Alignment());
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, cons_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, cons_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, cons_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, cons_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, cons_pad);
    res = mps_fmt_create_k(&cons_fmt, global_arena, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create cons format");

  mps_chain_t cons_chain;
  res = mps_chain_create(&cons_chain,
                         global_arena,
                         LENGTH(gen_params),
                         gen_params);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create cons_chain");
  // Create the AMC CONS pool
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, cons_fmt);
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, cons_chain);
#ifdef DEBUG_MPS_FENCEPOST_FREE
    MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options);
#endif
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY,keyExtendByKb*1024);
    MPS_ARGS_ADD(args, MPS_KEY_LARGE_SIZE,keyExtendByKb*1024);
    res = mps_pool_create_k(&global_amc_cons_pool, global_arena, mps_class_amc(), args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create amc cons pool");


  /* Objects that can not move but are managed by the garbage collector
     go in the global_non_moving_pool.
     Use an AWL pool rather than an AMS pool until the AMS pool becomes a production pool */
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    res = mps_pool_create_k(&global_non_moving_pool, global_arena, mps_class_awl(), args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create awl pool");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
    res = mps_ap_create_k(&global_non_moving_ap, global_non_moving_pool, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create global_non_moving_ap");


/* ------------------------------------------------------------------------
   Create a pool for objects that aren't moved and arent managed by the GC.
*/
#if 0
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, general_chain);
    MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, dummyAwlFindDependent);
    res = mps_pool_create_k(&global_non_moving_pool, global_arena, mps_class_awl(), args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create ams pool");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
    res = mps_ap_create_k(&global_non_moving_ap, global_non_moving_pool, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create global_non_moving_ap");
#endif


  // Create the AMCZ pool
  mps_pool_t _global_amcz_pool;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, general_chain);
    res = mps_pool_create_k(&_global_amcz_pool, global_arena, mps_class_amcz(), args);
  }
  MPS_ARGS_END(args);

  mps_fmt_t weak_obj_fmt;
  MPS_ARGS_BEGIN(args) {
#ifndef RUNNING_GC_BUILDER
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, Alignment());
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, weak_obj_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, weak_obj_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, weak_obj_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, weak_obj_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, weak_obj_pad);
#endif
    res = mps_fmt_create_k(&weak_obj_fmt, global_arena, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create obj format");

  // Create the AWL pool for weak hash tables here
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, weak_obj_fmt);
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, general_chain);
    MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, awlFindDependent);
    res = mps_pool_create_k(&_global_awl_pool, global_arena, mps_class_awl(), args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create awl pool");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
    res = mps_ap_create_k(&_global_strong_link_allocation_point, _global_awl_pool, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create global_strong_link_allocation_point");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_weak());
    res = mps_ap_create_k(&_global_weak_link_allocation_point, _global_awl_pool, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create global_weak_link_allocation_point");

  // And the allocation points
  res = mps_ap_create_k(&_global_automatic_mostly_copying_allocation_point,
                        _global_amc_pool, mps_args_none);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create mostly_copying_allocation_point");

    res = mps_ap_create_k(&global_amc_cons_allocation_point,
                        global_amc_cons_pool, mps_args_none);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create global_amc_cons_allocation_point");

  // res = mps_ap_create_k(&_global_mvff_allocation_point, _global_mvff_pool, mps_args_none );
  // if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create mvff_allocation_point");

  res = mps_ap_create_k(&_global_automatic_mostly_copying_zero_rank_allocation_point,
                        _global_amcz_pool, mps_args_none);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Couldn't create mostly_copying_zero_rank_allocation_point");

  // register the current and only thread
  mps_thr_t global_thread;
  res = mps_thread_reg(&global_thread, global_arena);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not register thread");

  // register the main thread stack scanner
  mps_root_t global_stack_root;
  // use mask
  res = mps_root_create_thread_tagged(&global_stack_root,
                                      global_arena,
                                      mps_rank_ambig(),
                                      0,
                                      global_thread,
                                      mps_scan_area_tagged_or_zero,
                                      gctools::pointer_tag_mask,
                                      gctools::pointer_tag_eq,
                                      reinterpret_cast<mps_addr_t>(const_cast<char*>(_global_stack_marker)));
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create stack root");

  /* Deal with finalization!!!! */
  mps_message_type_enable(global_arena, mps_message_type_finalization());
  mps_message_type_enable(global_arena, mps_message_type_gc());
  mps_message_type_enable(global_arena, mps_message_type_gc_start());

  // register the main thread roots in static and heap space

  //#define TEST_MPS        1

  for ( int i=0; i<global_symbol_count; ++i ) {
    global_symbols[i].rawRef_() = (core::Symbol_O*)NULL;
  }

  int exit_code = 0;

  mps_root_t global_scan_root;
  res = mps_root_create(&global_scan_root,
                        global_arena,
                        mps_rank_exact(),
                        0,
                        main_thread_roots_scan,
                        NULL,
                        0);
  if (res != MPS_RES_OK)
    GC_RESULT_ERROR(res, "Could not create scan root");

//  mps_register_root(reinterpret_cast<gctools::Tagged*>(&globalTaggedRunTimeValues));

  threadLocalStack()->allocateStack(gc::thread_local_cl_stack_min_size);

#ifdef RUNNING_GC_BUILDER
  printf("%s:%d mps-prep version of clasp started up\n", __FILE__, __LINE__);
  printf("%s:%d   You could run some tests here\n", __FILE__, __LINE__);
  printf("%s:%d   ... shutting down now\n", __FILE__, __LINE__);
  exit_code = 0;
#else
  #if 1
  run_quick_tests();
  core::ThreadLocalState thread_local_state;
  my_thread = &thread_local_state;
  exit_code = startupFn(argc, argv, mpiEnabled, mpiRank, mpiSize);
  #else
  printf("%s:%d Skipping startupFn\n", __FILE__, __LINE__ );
  test_mps_allocation();
  exit_code = 0;
  #endif
#endif
  size_t finalizations;
  processMpsMessages(finalizations);

  delete_my_roots();
  threadLocalStack()->deallocateStack();
  mps_root_destroy(global_scan_root);
  mps_root_destroy(global_stack_root);
  mps_thread_dereg(global_thread);
  mps_ap_destroy(_global_automatic_mostly_copying_zero_rank_allocation_point);
  mps_ap_destroy(global_amc_cons_allocation_point);
  mps_ap_destroy(_global_automatic_mostly_copying_allocation_point);
  mps_ap_destroy(_global_weak_link_allocation_point);
  mps_ap_destroy(_global_strong_link_allocation_point);
  mps_ap_destroy(global_non_moving_ap);
  mps_pool_destroy(_global_awl_pool);
  mps_pool_destroy(_global_amcz_pool);
  mps_pool_destroy(global_non_moving_pool);
  mps_pool_destroy(global_amc_cons_pool);
  mps_pool_destroy(_global_amc_pool);
  mps_arena_park(global_arena);
  mps_chain_destroy(cons_chain);
  mps_chain_destroy(general_chain);
  mps_fmt_destroy(weak_obj_fmt);
  mps_fmt_destroy(obj_fmt);
  mps_fmt_destroy(cons_fmt);
  mps_arena_destroy(global_arena);

  return exit_code;
};
};


namespace gctools {

CL_DEFUN void gctools__enable_underscanning(bool us)
{
  global_underscanning = us;
}

};

extern "C" {

void check_all_clients() {
  mps_arena_park(global_arena);
  // Add code to walk the pool and check everything
  mps_arena_release(global_arena);
}

};



namespace gctools {

void my_mps_thread_reg(mps_thr_t* threadP) {
  mps_res_t result = mps_thread_reg(threadP,global_arena);
  if (result != MPS_RES_OK) {
    printf("%s:%d Could not register thread\n", __FILE__, __LINE__ );
    abort();
  }
}

void my_mps_thread_deref(mps_thr_t thread) {
  mps_res_t result = mps_thread_dereg(thread);
  if (result != MPS_RES_OK) {
    printf("%s:%d Could not register thread\n", __FILE__, __LINE__ );
    abort();
  }
}

};
#endif // whole file #ifdef USE_MPS
