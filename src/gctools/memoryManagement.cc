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
#include <clasp/gctools/skip.h>
#include <clasp/gctools/scan.h>
#include <clasp/core/object.h>
#include <clasp/core/bformat.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/debugger.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/mpPackage.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/code.h>

#ifdef _TARGET_OS_LINUX
#include <signal.h>
#endif

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

DOCGROUP(clasp);
CL_DEFUN Fixnum gctools__nextStampValue() { return Header_s::StampWtagMtag::shift_unshifted_stamp(global_NextUnshiftedStamp); }
DOCGROUP(clasp);
CL_DEFUN Fixnum gctools__NextUnshiftedStampValue() { return global_NextUnshiftedStamp; }

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

}; // namespace gctools

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
#ifdef USE_BOEHM
  gcBase = GC_base((void*)this);
  if (gcBase != (void*)this)
    goto bad;
#endif
  if (!this->_badge_stamp_wtag_mtag.stampP())
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
#ifdef USE_BOEHM
  gcBase = GC_base((void*)this);
  if (gcBase != (void*)this)
    goto bad;
#endif
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

/* Walk all of the global (not per-thread) roots,
 * passing the address of each root */
template <std::invocable<Tagged*> RootWalkCallback>
void walkGlobalRoots(RootWalkCallback&& callback) {
  // luckily, just the one god object references everything else
  callback((Tagged*)&_lisp);
};

template <std::invocable<Tagged*> ThreadWalkCallback>
void walkThreadRoots(ThreadWalkCallback&& callback) {
  // NOTE we don't need the threads mutex since we must have stopped the world.
  for (auto cur : _lisp->_Roots._ActiveThreads) {
    mp::Process_sp proc = core::oCar(cur).as_assert<mp::Process_O>();
    core::ThreadLocalState* tls = proc->_ThreadInfo;
    tls->walkRoots(callback);
    tls->walkVMStack(callback);
    tls->walkControlStack([&](Tagged* tp) {
      // The control stack we have to walk conservatively.
      switch(ptag(*tp)) {
      case general_tag: {
        Header_s* header = (Header_s*)GeneralPtrToHeaderPtr(untag_object((void*)*tp));
        if (header->isValidGeneralObject())
          callback(tp);
      } break;
      case cons_tag: {
        ConsHeader_s* header = (ConsHeader_s*)ConsPtrToHeaderPtr(untag_object((void*)*tp));
        if (header->isValidConsObject())
          callback(tp);
      } break;
      default: callback(tp);
      }
    });
  }
}

template <std::invocable<Tagged*> WalkCallback>
void walkRoots(WalkCallback&& callback) {
  walkGlobalRoots(callback);
  walkThreadRoots(callback);
}

static void mw_obj_scan(core::General_O* client,
                        std::stack<std::pair<Tagged, Tagged>>& markStack) {
  auto fix = [&](core::T_O** field) {
    markStack.emplace((Tagged)client, (Tagged)*field);
  };
  scan::general(client, fix, [](WeakPointer*){}, [](Ephemeron*){});
}

static void mw_cons_scan(core::Cons_O* client,
                         std::stack<std::pair<Tagged, Tagged>>& markStack) {
  auto fix = [&](core::T_O** field) {
    markStack.emplace((Tagged)client, (Tagged)*field);
  };
  scan::cons(client, fix);
}

template <class Callback>
static void mapAllObjectsInternal(std::set<Tagged>& markSet,
                                  Callback callback) {
  std::stack<std::pair<Tagged, Tagged>> markStack;

  // process all roots
  walkGlobalRoots([&](Tagged* rootf) { markStack.emplace(0, *rootf); });

  while (!markStack.empty()) {
    // pop an object. we don't need the containing object.
    Tagged tagged = markStack.top().second; markStack.pop();

    switch(ptag(tagged)) {
    case general_tag: { // general object
      if (!markSet.contains(tagged)) { // only process each object once
        markSet.insert(tagged);
        uintptr_t client = untag_object(tagged);
        // the mw_foo_scan functions push all fields of the object
        // onto the markStack to keep the loop going.
        mw_obj_scan((core::General_O*)client, markStack);
        // now's the time to callback. Could also go before the scan
        callback(tagged);
      }
    } break;
    case cons_tag: {
      if (!markSet.contains(tagged)) {
        markSet.insert(tagged);
        uintptr_t client = untag_object(tagged);
        mw_cons_scan((core::Cons_O*)client, markStack);
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

  walkGlobalRoots([&](Tagged* rootAddr) { markStack.emplace(0, *rootAddr); });

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
          mw_obj_scan((core::General_O*)client, markStack);
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
          mw_cons_scan((core::Cons_O*)client, markStack);
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
    return cons_skip((core::Cons_O*)HeaderPtrToConsPtr(header));
  } else {
    // It's a general object - walk it
    return general_skip(HeaderPtrToGeneralPtr<core::General_O>(header));
  }
}

void traceablep(std::unordered_map<Tagged, bool>& testing) {
  std::set<Tagged> markSet;
  std::stack<Tagged> markStack;
  std::set<Ephemeron*> ephemera;

  auto fail = testing.end();

  auto fix_field
    = [&](core::T_O** field) { markStack.push((Tagged)*field); };

  auto fix_eph
    = [&](Ephemeron* eph) {
      // Insert ephemeron during normal tracing, otherwise do nothing
      // and get an iterator reference so we can erase if needed.
      auto it = ephemera.insert(eph).first;
      auto p = eph->get_no_lock();
      // If the ephemeron key is dead, we're done with this
      if (p.key.deletedp()) {
        ephemera.erase(it);
        return;
      }
      // OK, not deleted. Have we traced the key? If so,
      // trace the value and remove us from processing.
      Tagged key = p.key.tagged_();
      switch(ptag(key)) {
      case general_tag: case cons_tag:
          if (!markSet.contains(key)) break;
          [[fallthrough]];
      default: {
          // or we're something always alive, like a fixnum
          // or no_key
          ephemera.erase(it);
          Tagged value = p.value.tagged_();
          markStack.push(value);
      } break;
      }
      // We have not traced the key, but might do so later,
      // so leave us in the set.
    };

  walkRoots([&](Tagged* rootf) { markStack.push(*rootf); });

 trace:
  while (!markStack.empty()) {
    Tagged tagged = markStack.top(); markStack.pop();

    // Is this a value we're trying to trace?
    auto it = testing.find(tagged);
    if (it != fail)
      it->second = true; // yes, and it's reachable

    // Scan fields.
    switch(ptag(tagged)) {
    case general_tag: {
      if (!markSet.contains(tagged)) {
        markSet.insert(tagged);
        uintptr_t client = untag_object(tagged);
        scan::general((core::General_O*)client,
                      fix_field, [](WeakPointer*){}, fix_eph);
      }
    } break;
    case cons_tag: {
      if (!markSet.contains(tagged)) {
        markSet.insert(tagged);
        uintptr_t client = untag_object(tagged);
        scan::cons((core::Cons_O*)client, fix_field);
      }
    } break;
    default: break;
    }
  }
  // Normal tracing completed, but check for remaining ephemera.
  // for (auto eit : ephemera) fix_eph(eit);
  // does not work, because we erase iterators as we go.
  for (auto eit = ephemera.begin(); eit != ephemera.end();) {
    fix_eph(*(eit++));
  }
  // If any ephemera were live, go back and scan those references.
  if (!markStack.empty()) goto trace;
  // Otherwise we are done.
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
