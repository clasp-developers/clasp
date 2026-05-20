/*
    File: mmtkGarbageCollection.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/gctools/stw.h>
#ifdef USE_MMTK
#include <clasp/gctools/mmtkGarbageCollection.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/roots.h>
#include <clasp/gctools/scan.h>
#include <clasp/gctools/skip.h>

namespace gctools {

__attribute__((noinline)) void initializeMmtk(ClaspInfo* claspInfo) {
  // Build and initialise the MMTk instance.
  MMTkClaspBuilder builder = mmtk_clasp_create_builder();
  // NoGC plan: allocate but never collect (Phase 1).
  mmtk_clasp_set_option(builder, "plan", "NoGC");
  // Dynamic heap: start at 1 GiB, grow up to 96 GiB as needed (NoGC never collects).
  mmtk_clasp_set_dynamic_heap_size(builder, (size_t)1 * 1024 * 1024 * 1024,
                                   (size_t)96 * 1024 * 1024 * 1024);
  mmtk_clasp_init(builder);

  // Set up thread-local state for the main thread.
  my_thread = (core::ThreadLocalState*)malloc(sizeof(core::ThreadLocalState));
  new (my_thread) core::ThreadLocalState(false);
  my_thread_low_level = &my_thread->_LowLevel;

  // The mutator for this thread was bound by ThreadLocalState's ctor.
  mmtk_clasp_initialize_collection(my_thread);
}

// --- GC interface stubs ---

void collect_garbage() {
  mmtk_clasp_handle_user_collection_request(my_thread);
}

void set_finalizer_list(core::T_sp object, core::List_sp finalizers) {
  (void)object;
  (void)finalizers;
}

void clear_finalizer_list(core::T_sp object) {
  (void)object;
}

void invoke_finalizers() {}

bool heap_ptr_p(const void* p) {
  return mmtk_clasp_is_in_mmtk_spaces(p);
}

size_t heap_size() { return mmtk_clasp_total_bytes(); }

size_t free_bytes() { return mmtk_clasp_free_bytes(); }

size_t bytes_since_gc() { return 0; }

CL_DEFUN size_t core__dynamic_usage() { return mmtk_clasp_total_bytes(); }

}; // namespace gctools

// Scan the pointer fields of an object, calling callback for each field address.
// Uses scan::cons / scan::general_pointers; weak pointers are treated as strong
// for now (they are traced but not cleared if the referent dies).
extern "C" void clasp_scan_object(void* client, ClaspPreciseRootCallback callback, void* data) {
  const gctools::BaseHeader_s* near = gctools::base_header_ptr(client);
  if (near->_badge_stamp_wtag_mtag.consObjectP()) {
    gctools::scan::cons(static_cast<core::Cons_O*>(client),
                        [&](core::T_O** field) { callback(static_cast<void*>(field), data); });
  } else {
    gctools::scan::general_pointers(static_cast<core::General_O*>(client),
                                    [&](core::T_O** field) { callback(static_cast<void*>(field), data); });
  }
}

// Total allocation size (header + body) for an object given its client pointer.
//
// The stamp is always readable from client-sizeof(BaseHeader_s) (= client-8):
// for conses it's ConsHeader_s; for general objects without DEBUG_GUARD it's
// Header_s; for general objects with DEBUG_GUARD
// it's the _dup_badge_stamp_wtag_mtag guard copy at the end of Header_s.
// That lets us distinguish cons from general before dispatching to the right
// skip function and header size.
extern "C" size_t clasp_object_size(void* client) {
  const gctools::BaseHeader_s* near = gctools::base_header_ptr(client);
  if (near->_badge_stamp_wtag_mtag.consObjectP()) {
    return sizeof(gctools::ConsHeader_s) + gctools::cons_skip(static_cast<core::Cons_O*>(client));
  } else {
    return sizeof(gctools::Header_s) + gctools::general_skip(static_cast<core::General_O*>(client));
  }
}

// callback for MMTk's mutator(tls)
extern "C" MMTkClaspMutator clasp_get_mutator(void* tls) {
  return static_cast<core::ThreadLocalState*>(tls)->_LowLevel._mmtk_mutator;
}

// Root-scanning callbacks for MMTk's Rust scanning implementation.
// These are called with the world stopped.

extern "C" void clasp_walk_global_roots(ClaspPreciseRootCallback callback, void* data) {
  gctools::walkGlobalRoots([&](gctools::Tagged* tp) { callback(static_cast<void*>(tp), data); });
}

extern "C" void clasp_walk_thread_precise_roots(void* tls, ClaspPreciseRootCallback callback, void* data) {
  core::ThreadLocalState* ts = static_cast<core::ThreadLocalState*>(tls);
  ts->walkRoots([&](gctools::Tagged* tp) { callback(static_cast<void*>(tp), data); });
  ts->walkVMStack([&](gctools::Tagged* tp) { callback(static_cast<void*>(tp), data); });
}

extern "C" void clasp_walk_thread_conservative_roots(void* tls, ClaspConservativeRootCallback callback, void* data) {
  core::ThreadLocalState* ts = static_cast<core::ThreadLocalState*>(tls);
  ts->walkControlStack([&](gctools::Tagged* tp) {
    // Strip the tag without using untag_object (which asserts) since stack
    // values may have coincidental tag bits and not be valid pointers.
    void* client = reinterpret_cast<void*>(*tp & gctools::ptr_mask);
    if (!client || !mmtk_clasp_is_in_mmtk_spaces(client))
      return;
    switch (gctools::ptag(*tp)) {
    case gctools::general_tag: {
      gctools::Header_s* header = (gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(client);
      if (header->isValidGeneralObject())
        callback(client, data);
    } break;
    case gctools::cons_tag: {
      gctools::ConsHeader_s* header = (gctools::ConsHeader_s*)gctools::ConsPtrToHeaderPtr(client);
      if (header->isValidConsObject())
        callback(client, data);
    } break;
    default:
      break;
    }
  });
}

#endif // USE_MMTK
