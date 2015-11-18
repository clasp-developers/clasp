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

#define IGNORE(_ptr_)
#define SMART_PTR_FIX(_ptr_)
#define WEAK_SMART_PTR_FIX(_ptr_)
#define GCHOLDER_SYMBOLMAP_FIX(_map_)
#define GCHOLDER_STRINGMAP_FIX(_map_)
#define GCHOLDER_VECTOR0_FIX(_vec_)
#define GCHOLDER_UNORDEREDSET_FIX(_vec_)
#define GCHOLDER_INDEXEDSYMBOLMAP_FIX(_map_)
#define STLVECTOR_FIX(_set_)
#define STLSET_FIX(_set_)
#define STLMAP_SMART_FIRST_FIX(_map_)
#define STLMAP_SMART_SECOND_FIX(_map_)
#define STLMAP_SMART_FIRST_SECOND_FIX(_map_)
#define STLMULTIMAP_SMART_FIRST_FIX(_map_)
#define STLMULTIMAP_SMART_SECOND_FIX(_map_)
#define STLMULTIMAP_SMART_FIRST_SECOND_FIX(_map_)
#define STL_VECTOR_REQUIRED_ARGUMENT_FIX(_vec_)
#define STL_VECTOR_OPTIONAL_ARGUMENT_FIX(_vec_)
#define REST_ARGUMENT_FIX(_arg_)
#define STL_VECTOR_KEYWORD_ARGUMENT_FIX(_vec_)
#define STL_VECTOR_AUX_ARGUMENT_FIX(_vec_)

namespace gctools {
class GCObject;
class GCLinkedList;

class GCObject {
public:
  GCObject &operator=(const GCObject &) { return *this; };
};

/*! GCKindEnum has one integer value for each type allocated by the GC.
This value is written into the Header_s of every allocated object.
If USE_CXX_DYNAMIC_CAST is defined then GCKindEnum has only one value and every Boehm header
contains that value and C++ dynamic_cast<...> is used to determine IsA relationships.
If USE_CXX_DYNAMIC_CAST is not defined then the GCKindEnum values calculated by
the mps-interface.lsp static analyzer are used along with template functions that
calculate IsA relationships using simple GCKindEnum range comparisons.
*/

#ifdef USE_CXX_DYNAMIC_CAST
typedef enum { KIND_null = 0,
               KIND_max } GCKindEnum; // minimally define this GCKind
#else
typedef
#define GC_ENUM
#include STATIC_ANALYZER_PRODUCT
    GCKindEnum;
#undef GC_ENUM
#endif

//#define BIG_BOEHM_HEADER

#ifdef USE_BOEHM_MEMORY_MARKER
extern int globalBoehmMarker;
#endif
class Header_s {
public:
  Header_s(GCKindEnum k) : Kind(k)
#ifdef BIG_BOEHM_HEADER
                           ,
                           ValidStamp(0xDEADBEEF), TypeidName(name)
#endif
#ifdef USE_BOEHM_MEMORY_MARKER
                           ,
                           Marker(globalBoehmMarker)
#endif
  {
#ifdef _DEBUG_BUILD
    if (k > KIND_max) {
      printf("%s:%d Allocating object of kind: %zu - this is beyond KIND_max: %d\n", __FILE__, __LINE__, k, KIND_max);
    }
    if (k == 0) {
      printf("%s:%d Allocating object of kind: %zu - this is not allowed except for maybe in boehmdc\n", __FILE__, __LINE__, k);
    }
#endif
  };

private:
#ifdef _ADDRESS_MODEL_64
  uint64_t Kind;
#endif
#ifdef _ADDRESS_MODEL_32
  uint32_t Kind;
#endif
#ifdef BIG_BOEHM_HEADER
  uintptr_t ValidStamp;
  const char *TypeidName;
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
  const char *name() const {
#ifdef BIG_BOEHM_HEADER
    return this->TypeidName;
#else
    return "TypeIdUnavailable";
#endif
  };
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

#if 0
class TemplatedHeader_s : public Header_s {
public:
  TemplatedHeader_s(const char *name, BoehmKind k) : Header_s(name, k){};
};
#endif
constexpr size_t Alignment() {
  //            return sizeof(Header_s);
  return alignof(Header_s);
};
constexpr size_t AlignUp(size_t size) { return (size + Alignment() - 1) & ~(Alignment() - 1); };

template <class T>
inline size_t sizeof_with_header() { return AlignUp(sizeof(T)) + AlignUp(sizeof(Header_s)); };

#if 0
template <class T>
inline size_t sizeof_with_templated_header() { return AlignUp(sizeof(T)) + AlignUp(sizeof(TemplatedHeader_s)); };
#endif

void headerDescribe(core::T_O *taggedClient);
};

namespace gctools {

inline void *ClientPtrToBasePtr(void *mostDerived) {
  size_t headerSize = AlignUp(sizeof(Header_s));
  void *ptr = reinterpret_cast<char *>(mostDerived) - headerSize;
  return ptr;
}

template <typename T>
inline T *BasePtrToMostDerivedPtr(void *base) {
  size_t headerSize = AlignUp(sizeof(Header_s));
  T *ptr = reinterpret_cast<T *>(reinterpret_cast<char *>(base) + headerSize);
  return ptr;
}
};

namespace core {
class T_O;
class WrappedPointer_O;
class Functoid;
class Creator;
class Iterator_O;
};
namespace clbind {
class ConstructorCreator;
};

#ifndef USE_CXX_DYNAMIC_CAST
#define DECLARE_FORWARDS
#include STATIC_ANALYZER_PRODUCT
#undef DECLARE_FORWARDS
#endif

namespace gctools {
#ifndef USE_CXX_DYNAMIC_CAST
#define GC_DYNAMIC_CAST
#include STATIC_ANALYZER_PRODUCT // "main/clasp_gc.cc"
#undef GC_DYNAMIC_CAST
#endif
};

namespace gctools {
/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
int initializeMemoryManagement(MainFunctionType startup, int argc, char *argv[], void *dummy);
};

#endif // _clasp_boehmGarbageCollection_H
