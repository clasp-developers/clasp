/*
    File: intrusiveRefCountGarbageCollection.h
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
#ifndef _brcl_intrusiveRefCountGarbageCollection_H
#define _brcl_intrusiveRefCountGarbageCollection_H

namespace gctools {

extern bool _GlobalDebugAllocations;

//#define USE_GC_REF_COUNT_WRAPPER          1

#ifdef USE_GC_REF_COUNT_WRAPPER
template <typename T>
struct GCWrapper {
  typedef T WrappedType;
  struct GCHeader {
    class_id _Kind;
    mutable uint _ReferenceCount;
  };
  GCHeader _Header;

  static size_t sizeof_wrapper() { return sizeof(GCHeader) + sizeof(T); };
  static size_t offsetof_object() { return sizeof(GCHeader); };

  // Variadic constructor
  GCWrapper() : _Header({reg::registered_class<T>::id, 0}){};

  template <typename U>
  static GCWrapper *gcHeader(U *ptr) { return reinterpret_cast<GCWrapper *>(reinterpret_cast<char *>(dynamic_cast<void *>(ptr)) - offsetof_object()); };

  template <typename U>
  static const GCWrapper *gcHeader(const U *ptr) { return reinterpret_cast<const GCWrapper *>(reinterpret_cast<const char *>(dynamic_cast<const void *>(ptr)) - offsetof_object()); };

  template <typename U>
  static void gcAddRef(U *ptr) {
    GCWrapper *gch = const_cast<GCWrapper<T> *>(GCWrapper::gcHeader(ptr));
    gch->_Header._ReferenceCount++;
#if 0
            if ( _GlobalDebugAllocations ) {
                printf("%s:%d - inc ref to %d at %p\n", __FILE__, __LINE__, gch->_Header._ReferenceCount, gch );
            }
#endif
  };

  template <typename U>
  static void gcRelease(U *ptr) {
    const GCWrapper *gch = GCWrapper::gcHeader(ptr);
    if (--(gch->_Header._ReferenceCount) == 0) {
      T *obj = gch->gcobject();
      obj->~T();
      void *addr = reinterpret_cast<void *>(const_cast<GCWrapper<T> *>(gch));
#if 0
                if ( _GlobalDebugAllocations ) {
                    printf("%s:%d - dec ref and free of object@%p\n", __FILE__, __LINE__, addr);
                }
#endif
      free(addr);
    } else {
#if 0
                if ( _GlobalDebugAllocations ) {
                    printf("%s:%d - dec ref to %d at %p\n", __FILE__, __LINE__, gch->_Header._ReferenceCount, gch );
                }
#endif
    }
  };

  template <typename U>
  static int gcKind(U *ptr) {
    GCWrapper *gcw = GCWrapper::gcHeader(ptr);
    return gcw->_Header._Kind;
  };

  T *gcobject() const { return reinterpret_cast<T *>(reinterpret_cast<char *>(const_cast<GCWrapper<T> *>(this)) + offsetof_object()); }
};
#endif
};

// Memory allocation MACROS to replace MPS using macros
//#define GC_RESERVE_ GET1(_class_,_obj_,_arg1_) _obj_ = RP_OLD_Create1<_class_>(_arg1_,false);

//  // old way
// #define GC_RESERVE_GET(_class_,_obj_) _obj_ = RP_OLD_Create<_class_>(false);
// #define GC_RESERVE_GET_DONT_INITIALIZE(_class_,_obj_) _obj_ = RP_OLD_Create<_class_>(false);
// #define GC_RESERVE_GET_VARIADIC(_class_,_obj_,...) _obj_ = RP_OLD_Create_VARIADIC<_class_>(false,__VA_ARGS__);
// #define GC_COPY_GET(_class_,_obj_,_orig_) _obj_ = RP_OLD_Copy<_class_>(_orig_);
//

#define GC_ALLOCATE(_class_, _obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::allocate()
#define GC_ALLOCATE_UNCOLLECTABLE(_class_, _obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::rootAllocate()
#define GC_ALLOCATE_VARIADIC(_class_, _obj_, ...) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::allocate(__VA_ARGS__)

#define GC_COPY(_class_, _obj_, _orig_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::copy(_orig_)

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

extern GCObject *_GlobalAllocatedObjects;
extern uint _GlobalAllocationFingerprint;
extern unsigned long long _GlobalAllocationCounter;

class GCObject {
public:
#ifdef USE_REFCOUNT
#ifndef USE_GC_REF_COUNT_WRAPPER
  mutable int _ReferenceCount;

protected:
  // New ctor
  GCObject() : _ReferenceCount(0){};
  // Copy  ctor
  GCObject(const GCObject &orig) : _ReferenceCount(0){};

public:
  int referenceCount() const { return this->_ReferenceCount; };
#endif
#endif
  GCObject &operator=(const GCObject &) { return *this; };
};

#define ALIGNMENT alignof(char *)
#define ALIGN_UP(size) \
  (((size)+ALIGNMENT - 1) & ~(ALIGNMENT - 1))

typedef const char *Header_s;

template <class T>
inline size_t sizeof_with_header() { return ALIGN_UP(sizeof(T)) + ALIGN_UP(sizeof(gctools::Header_s)); };

/*! Make objects self describing by prepending a pointer to their typeInfoName */
struct GCHeader {
  Header_s typeInfoName;
};

#define BASE_TO_OBJ_PTR(_gcptr_) reinterpret_cast<void *>(reinterpret_cast<char *>(_gcptr_) + ALIGN_UP(sizeof(gctools::Header_s)))
#define OBJ_TO_BASE_PTR(_objptr_) reinterpret_cast<void *>(reinterpret_cast<char *>(_objptr_) - ALIGN_UP(sizeof(gctools::Header_s)))
};

namespace gctools {
/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
int initializeMemoryManagement(MainFunctionType startup, int argc, char *argv[], void *dummy);
};

#endif // _brcl_intrusiveRefCountGarbageCollection_H
