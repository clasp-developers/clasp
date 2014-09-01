#ifndef _brcl_memoryManagement_H
#define _brcl_memoryManagement_H

#include "hardErrors.h"


#define INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(x)

#define USE_WEAK_POINTER


#ifdef USE_BOEHM
#include "gc/gc.h"
#include "gc/gc_allocator.h"
        typedef void* LocationDependencyPtrT;
#endif // USE_BOEHM

#ifdef USE_MPS


extern "C" 
{
#include "mps/code/mps.h"
#include "mps/code/mpsavm.h"
};
        typedef mps_ld_t LocationDependencyPtrT;
#endif


typedef int (*MainFunctionType)(int argc, char* argv[], bool& mpiEnabled, int& mpiRank, int& mpiSize );

#define GC_LOG(x)


#include "hardErrors.h"


namespace gctools {

    template <typename T>
    constexpr size_t depreciatedAlignmentT() { return alignof(T); };
    template <typename T>
    constexpr size_t depreciatedAlignUpT(size_t size) { return (size + depreciatedAlignmentT<T>() - 1) & ~(depreciatedAlignmentT<T>() - 1);};

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

    extern void* _global_stack_marker;


    /*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
    template <class OT> struct GCKind;
};

namespace gctools {

#ifdef USE_BOEHM
    class Header_s;
#endif
#ifdef USE_MPS
    class Header_s;
#endif

    template <typename T> struct GCHeader {
#ifdef USE_BOEHM
        typedef Header_s                HeaderType;
#endif
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
        typedef Header_s         HeaderType;
#else
        typedef Header_s         HeaderType;
#endif
#endif
    };

    template <typename T> struct GCAllocationPoint;
        

};



namespace gctools {
    template <typename TOPTR, typename FROMPTR>
    struct DynamicCast {
        static bool isA(FROMPTR ptr) {
            return (dynamic_cast<TOPTR>(ptr)!=NULL);
        }
        static TOPTR castOrNULL(FROMPTR client) {
            return dynamic_cast<TOPTR>(client);
        }
    };
};







#ifdef USE_BOEHM
#include "boehmGarbageCollection.h"
#endif
#ifdef USE_MPS
#include "mpsGarbageCollection.h"
#endif


namespace gctools {
    /*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
    template <class OT> struct GCKind {
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
        static GCKindEnum const Kind = KIND_null;
#else
        // We need a default Kind when running the gc-builder.lsp static analyzer
        // but we don't want a default Kind when compiling the mps version of the code
        // to force compiler errors when the Kind for an object hasn't been declared
#endif // RUNNING_GC_BUILDER
#endif // USE_MPS
#ifdef USE_BOEHM
        static GCKindEnum const Kind = KIND_null; // minimally define KIND_null
#endif
    };
};

namespace gctools {




    template <class OT>
    struct GCInfo {
        static constexpr bool Atomic = false;
        static bool const NeedsInitialization = true; // Currently everything needs initialization
        static bool const NeedsFinalization = true; // Currently everything needs finalization
        static constexpr bool Moveable = true;
    };


};






#include "smart_pointers.h"



namespace gctools {
    template <typename T>
    void* SmartPtrToBasePtr(smart_ptr<T> obj)
    {
        void* ptr;
        if ( obj.pointerp() ) {
            ptr = reinterpret_cast<void*>(reinterpret_cast<char*>(obj.pbase()) - sizeof(Header_s));
        } else {
            ptr = reinterpret_cast<void*>(obj.px_ref());
        }
        return ptr;
    }

};


#define DECLARE_onHeapScanGCRoots()
#define DECLARE_onStackScanGCRoots()



namespace gctools {

    /*! Size of containers given the number of elements */
    template <typename Cont_impl>
    size_t sizeof_container(size_t n)
    {
        size_t headerSz = sizeof(Cont_impl);
        size_t dataSz = sizeof(typename Cont_impl::value_type)*n;
        size_t totalSz = headerSz+dataSz;
        GC_LOG(("headerSz[%lu] + ( value_size[%lu] * n[%lu] -> dataSz[%lu] ) --> totalSz[%lu]\n",
                headerSz, sizeof(typename Cont_impl::value_type), n, dataSz, totalSz));
        return AlignUp(totalSz);
    };


    template <class T> inline size_t sizeof_container_with_header(size_t num) {
        return sizeof_container<T>(num)+sizeof(Header_s);
    };

};



#include "gcalloc.h"


#define GC_ALLOCATE(_class_,_obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::allocate()
#define GC_ALLOCATE_VARIADIC(_class_,_obj_,...) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::allocate(__VA_ARGS__)

#define GC_ALLOCATE_UNCOLLECTABLE(_class_,_obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::rootAllocate()

#define GC_COPY(_class_,_obj_,_orig_) gctools::smart_ptr<_class_> _obj_ = gctools::GCObjectAllocator<_class_>::copy(_orig_)







/*! These don't do anything at the moment
  but may be used in the future to create unsafe-gc points
*/

#define SUPPRESS_GC() {}
#define ENABLE_GC() {}




#endif // _brcl_memoryManagement_H
    

