#pragma once

/*
    File: boehmGarbageCollection.h
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

/*! Return the most derived pointer of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) (dynamic_cast<void*>(_smartptr_.px_ref()))
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void*>(dynamic_cast<const void*>(_ptr_)))

#ifdef USE_PRECISE_GC
namespace gctools {
struct GcScanStateType {};
#define GC_RESULT int
#define GC_SCAN_STATE_TYPE GcScanStateType
#define GC_POINTER void*
}; // namespace gctools
#endif

namespace gctools {
void* boehm_create_shadow_table(size_t nargs);
};

namespace gctools {
/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
int initializeMemoryManagement(int argc, char* argv[], void* dummy);
}; // namespace gctools

namespace gctools {
// Looks like an MPS mps_ld_s structure
// So that field_offset table for MPS can be used by boehm
// Instance variable must be called _LocationDependency
struct BogusBoehmLocationDependencyTracker {
  unsigned long _epoch;
  unsigned long _rs;
};

}; // namespace gctools

namespace gctools {
#ifndef RUNNING_PRECISEPREP

// Given an interior_pointer
// Return true and the base object if the interior_pointer points into an object
// Return false and undefined for object if it does not.
// This is the General_O object case
template <typename GeneralType>
inline bool tagged_pointer_from_interior_pointer(clasp_ptr_t interior_pointer, Tagged& tagged_pointer) {
  void* base = GC_base(static_cast<void*>(interior_pointer));
  if (base) {
    GeneralType* client = HeaderPtrToGeneralPtr<GeneralType>(base);
    tagged_pointer = (gctools::Tagged)tag_general<GeneralType*>(client);
    return true;
  }
  return false;
}

// core::Cons_sp specializer
template <> inline bool tagged_pointer_from_interior_pointer<core::Cons_O>(clasp_ptr_t interior_pointer, Tagged& tagged_pointer) {
  void* base = GC_base(static_cast<void*>(interior_pointer));
  if (base) {
    core::Cons_O* client = HeaderPtrToGeneralPtr<core::Cons_O>(base);
    tagged_pointer = (gctools::Tagged)tag_cons<core::Cons_O*>(client);
    return true;
  }
  return false;
}
#endif
void boehm_set_finalizer_list(gctools::Tagged object, gctools::Tagged finalizers);
void boehm_clear_finalizer_list(gctools::Tagged object);

void clasp_warn_proc(char* msg, GC_word arg);

void startupBoehm(gctools::ClaspInfo* claspInfo);
int runBoehm(gctools::ClaspInfo* claspInfo);
void shutdownBoehm();

}; // namespace gctools

extern "C" {
// Do the same thing that mps_park and mps_release do
void boehm_park();
void boehm_release();

void boehm_callback_reachable_object_find_stamps(void* ptr, size_t sz, void* client_data);
void boehm_callback_reachable_object_find_owners(void* ptr, size_t sz, void* client_data);
};

namespace gctools {
// ------------
struct ReachableClass {
  ReachableClass() : _Kind(gctools::STAMPWTAG_null){};
  ReachableClass(gctools::GCStampEnum tn) : _Kind(tn), instances(0), totalSize(0) {}
  void update(size_t sz) {
    ++this->instances;
    this->totalSize += sz;
  };
  gctools::GCStampEnum _Kind;
  size_t instances;
  size_t totalSize;
  size_t print(std::ostringstream& output);
};

typedef map<gctools::GCStampEnum, ReachableClass> ReachableClassMap;

struct FindStamp {
  gctools::GCStampEnum _stamp;
  std::vector<void*> _addresses;
  FindStamp(gctools::GCStampEnum stamp) : _stamp(stamp){};
};

struct FindOwner {
  void* _pointer;
  std::vector<void*> _addresses;
  FindOwner(void* pointer) : _pointer(pointer){};
};

/*!
 * claspgc_room - the GC specific implementation of ROOM
 */
void clasp_gc_room(std::ostringstream& OutputStream, RoomVerbosity verbosity);

void clasp_gc_registerRoots(void* rootsStart, size_t numberOfRoots);
void clasp_gc_deregisterRoots(void* rootsStart, size_t numberOfRoots);
}; // namespace gctools
