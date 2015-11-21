/*
    File: memoryManagement.h
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
#ifndef _clasp_memoryManagement_H
#define _clasp_memoryManagement_H

// Define compile-time flags that effect structure sizes
//
#include <clasp/gctools/configure_memory.h>

#include <clasp/gctools/hardErrors.h>

#define INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(x)

#define USE_WEAK_POINTER

#ifdef USE_BOEHM
#include <clasp/gc/gc.h>
#include <clasp/gc/gc_allocator.h>
typedef void *LocationDependencyPtrT;
#endif // USE_BOEHM

#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mps.h>
#include <clasp/mps/code/mpsavm.h>
};
typedef mps_ld_t LocationDependencyPtrT;
#endif

typedef int (*MainFunctionType)(int argc, char *argv[], bool &mpiEnabled, int &mpiRank, int &mpiSize);

#define GC_LOG(x)
#define GCPRIVATE public
#define GCPROTECTED public

#include <clasp/gctools/hardErrors.h>

namespace gctools {

template <typename T>
constexpr size_t depreciatedAlignmentT() { return alignof(T); };
template <typename T>
constexpr size_t depreciatedAlignUpT(size_t size) { return (size + depreciatedAlignmentT<T>() - 1) & ~(depreciatedAlignmentT<T>() - 1); };
};

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Define what a Header_s is for each garbage collector
// as well as other GC specific stuff
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

namespace gctools {
/*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
template <class OT>
struct GCKind;
extern size_t global_alignup_sizeof_header;
extern void *_global_stack_marker;
extern size_t _global_stack_max_size;
};

namespace gctools {

#ifdef USE_BOEHM
class Header_s;
#endif
#ifdef USE_MPS
class Header_s;
#endif

template <typename T>
struct GCHeader {
#ifdef USE_BOEHM
  typedef Header_s HeaderType;
#endif
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
  typedef Header_s HeaderType;
#else
  typedef Header_s HeaderType;
#endif
#endif
};

template <typename T>
struct GCAllocationPoint;
};

/*!
  Template struct:   DynamicCast

  Specialized in clasp_gc.cc

*/

#include <clasp/gctools/pointer_tagging.h>

#include <clasp/gctools/tagged_cast.h>

#ifdef USE_BOEHM
#include <clasp/gctools/boehmGarbageCollection.h>
#endif
#ifdef USE_MPS
#include <clasp/gctools/mpsGarbageCollection.h>
#endif

namespace gctools {

struct MonitorAllocations {
  bool on;
  bool stackDump;
  int counter;
  int start;
  int end;
  int backtraceDepth;
  MonitorAllocations() : on(false), stackDump(false), counter(0){};
};
extern MonitorAllocations global_monitorAllocations;

extern void monitorAllocation(GCKindEnum k, size_t sz);

#ifdef GC_MONITOR_ALLOCATIONS
#define MONITOR_ALLOCATION(k, sz)     \
  if (global_monitorAllocations.on) { \
    monitorAllocation(k, sz);         \
  }
#else
#define MONITOR_ALLOCATION(k, sz)
#endif
}
extern "C" {
char *obj_name(gctools::GCKindEnum kind);
char *obj_kind_name(core::T_O *ptr);
size_t obj_kind(core::T_O *ptr);
extern void obj_dump_base(void *base);
};

namespace gctools {
/*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
template <class OT>
struct GCKind {
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
  static GCKindEnum const Kind = KIND_null;
#else
  // We need a default Kind when running the gc-builder.lsp static analyzer
  // but we don't want a default Kind when compiling the mps version of the code
  // to force compiler errors when the Kind for an object hasn't been declared
  static GCKindEnum const Kind = KIND_null; // provide default for weak dependents
#endif // RUNNING_GC_BUILDER
#endif // USE_MPS
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
  static GCKindEnum const Kind = KIND_null; // minimally define KIND_null
#else
                                            // We don't want a default Kind when compiling the boehm version of the code
                                            // to force compiler errors when the Kind for an object hasn't been declared
// using clasp_gc.cc
#endif // USE_CXX_DYNAMIC_CAST
#endif
};
};

namespace gctools {

/*
 * atomic == Object contains no internal tagged pointers, is collectable
 * normal == Object contains internal tagged pointers, is collectable
 * collectable_immobile == Object cannot be moved but is collectable
 * noncollectable_immobile == Object cannot be moved and cannot be automatically collected
 */
  typedef enum { atomic,
                 normal,
                 collectable_immobile,
                 noncollectable_immobile } GCInfo_policy;
  
template <class OT>
struct GCInfo {
  static constexpr GCInfo_policy Policy = normal;
  static bool const NeedsInitialization = true; // Currently, by default,  everything needs initialization
  static bool const NeedsFinalization = false;  // By default, nothing needs finalization
};
};

#include <clasp/gctools/smart_pointers.h>

namespace gctools {
template <typename T>
void *SmartPtrToBasePtr(smart_ptr<T> obj) {
  void *ptr;
  if (obj.objectp()) {
    ptr = reinterpret_cast<void *>(reinterpret_cast<char *>(obj.untag_object()) - sizeof(Header_s));
  } else {
    THROW_HARD_ERROR(BF("Bad pointer for SmartPtrToBasePtr"));
    //            ptr = reinterpret_cast<void*>(obj.px_ref());
  }
  return ptr;
}
};

#define DECLARE_onHeapScanGCRoots()
#define DECLARE_onStackScanGCRoots()

namespace gctools {

/*! Size of containers given the number of elements */
template <typename Cont_impl>
size_t sizeof_container(size_t n) {
  size_t headerSz = sizeof(Cont_impl);
  size_t dataSz = sizeof(typename Cont_impl::value_type) * n;
  size_t totalSz = headerSz + dataSz;
  GC_LOG(("headerSz[%lu] + ( value_size[%lu] * n[%lu] -> dataSz[%lu] ) --> totalSz[%lu]\n",
          headerSz, sizeof(typename Cont_impl::value_type), n, dataSz, totalSz));
  return AlignUp(totalSz);
};

template <class T>
inline size_t sizeof_container_with_header(size_t num) {
  return sizeof_container<T>(num) + sizeof(Header_s);
};
};

namespace gctools {
class GCStack;
GCStack *threadLocalStack();
};

#include <clasp/gctools/gcStack.h>
#include <clasp/gctools/gcalloc.h>

#define GC_ALLOCATE(_class_, _obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::allocate()
#define GC_ALLOCATE_VARIADIC(_class_, _obj_, ...) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::allocate(__VA_ARGS__)

#define GC_ALLOCATE_UNCOLLECTABLE(_class_, _obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::rootAllocate()

#define GC_COPY(_class_, _obj_, _orig_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::copy(_orig_)

/*! These don't do anything at the moment
  but may be used in the future to create unsafe-gc points
*/

#define SUPPRESS_GC() \
  {}
#define ENABLE_GC() \
  {}

namespace gctools {

int handleFatalCondition();

/* Start up the garbage collector and the main function.
       The main function is wrapped within this function */
int startupGarbageCollectorAndSystem(MainFunctionType startupFn, int argc, char *argv[], size_t stackMax, bool mpiEnabled, int mpiRank, int mpiSize);
};

#endif // _clasp_memoryManagement_H
