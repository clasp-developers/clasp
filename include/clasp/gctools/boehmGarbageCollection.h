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
#ifndef _clasp_boehmGarbageCollection_H
#define _clasp_boehmGarbageCollection_H

/*! Return the most derived pointer of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) (dynamic_cast<void *>(_smartptr_.px_ref()))
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void *>(dynamic_cast<const void *>(_ptr_)))

#if 0  // header is defined in memoryManagement.h
namespace gctools {
//#define BIG_BOEHM_HEADER

#ifdef USE_BOEHM_MEMORY_MARKER
  extern int globalBoehmMarker;
#endif
  class Header_s {
  public:
  Header_s(kind_t k) : Kind(k)
#ifdef BIG_BOEHM_HEADER
      , ValidStamp(0xBEEFDEADDEADBEEF)
#endif
#ifdef USE_BOEHM_MEMORY_MARKER
      , Marker(globalBoehmMarker)
#endif
      {
#ifdef _DEBUG_BUILD
        if (k > KIND_max) {
          printf("%s:%d Allocating object of kind: %zu - this is beyond KIND_max: %d\n", __FILE__, __LINE__, k, KIND_max);
        }
#if defined(USE_BOEHM)&&defined(USE_CXX_DYNAMIC_CAST)
    // nothing
#else
        if (k == 0) {
          printf("%s:%d Allocating object of kind: %zu - this is not allowed except for maybe in boehmdc\n", __FILE__, __LINE__, k);
        }
#endif
#endif
      };
#if defined(DEBUG_GUARD)
    #error "Boehm cannot be compiled with DEBUG_GUARD - turn it off"
#endif
  public:
    kind_t Kind;
#ifdef BIG_BOEHM_HEADER
    uintptr_t ValidStamp;
#endif
#ifdef USE_BOEHM_MEMORY_MARKER // defined in foundation.h
    int Marker;
#endif
  public:
    bool isValid() const {
#ifdef BIG_BOEHM_HEADER
      return this->ValidStamp == 0xDEADBEEF;
#else
      return true;
#endif
    };
    bool invalidP() const { return false; };
      bool kindP() const { return true; };
    GCKindEnum kind() const { return (GCKindEnum) this->Kind; };
    bool markerMatches(int m) const {
#ifdef USE_BOEHM_MEMORY_MARKER
      if (m) {
        return this->Marker == m;
      } else
        return true;
#else
      return true;
#endif
    }
    static size_t HeaderSize() { return sizeof(Header_s); };
  };
};
#endif


namespace gctools {
  void* boehm_create_shadow_table(size_t nargs);
};

namespace gctools {
/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
int initializeMemoryManagement(MainFunctionType startup, int argc, char *argv[], void *dummy);
};

namespace gctools {
  // Looks like an MPS mps_ld_s structure
  // So that field_offset table for MPS can be used by boehm
  // Instance variable must be called _LocationDependency
 struct BogusBoehmLocationDependencyTracker {
    unsigned long _epoch;
    unsigned long _rs;
  };


};

namespace gctools {

  void boehm_set_finalizer_list(gctools::Tagged object, gctools::Tagged finalizers );
  void boehm_clear_finalizer_list(gctools::Tagged object);


};
#endif // _clasp_boehmGarbageCollection_H
