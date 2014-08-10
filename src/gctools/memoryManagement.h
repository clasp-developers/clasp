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


#if 0
namespace gctools {

    struct HeapRoot;
    struct StackRoot;

    /*! This is a common root class for HeapRoot and StackRoot
      Inherit from one of these if you want the GC refactoring tool to write 
      a scanner for the object automatically. */
    struct GC_Automated {};

    /*! Inherit from GC_Manual if you (the programmer)
      will write and maintain the scanGCRoots function for any
      subclass.
      THIS IS USED BY MultipleValues.
    */
    struct GC_Manual {};



    extern 	HeapRoot* 	rooted_HeapRoots;
    extern	StackRoot* 	rooted_StackRoots;

    /*! HeapRoots create a double linked list of nodes so that
      when the system shuts down they can unlink themselves without crashing */
    struct HeapRoot : public GC_Automated
    {
	HeapRoot*	_prev;
	HeapRoot* _next;
	HeapRoot() : _prev(NULL), _next(NULL) {};
	virtual ~HeapRoot() { this->detachFromGCRoot(); };
        virtual const char* repr() const { printf("%s:%d Subclass must implement repr\n", __FILE__, __LINE__ ); return "HeapRoot";};

	/*! Link this node into the list of static roots.
	  Do this in the correct order so that if GC happens
	  rooted_HeapRoots is always pointing to valid roots */
	void attachToGCRoot() {
	    this->_next = rooted_HeapRoots;
	    this->_prev = NULL;
	    if ( rooted_HeapRoots != NULL ) {
		rooted_HeapRoots->_prev = this;
	    }
	    rooted_HeapRoots = this;
            //          printf("%s:%d HeapRoot::attachToGCRoot for %s\n", __FILE__, __LINE__, this->repr() );
	}

	void detachFromGCRoot() {
	    if ( this->_prev != NULL ) this->_prev->_next = this->_next;
	    else rooted_HeapRoots = this->_next;
	    if ( this->_next != NULL ) this->_next->_prev = this->_prev;
//            printf("%s:%d HeapRoot::detachFromGCRoot for %s\n", __FILE__, __LINE__, this->repr() );
	}
    };


    /*! Stack roots create a FIFO stack of nodes */
    struct StackRoot : public GC_Automated
    {
	StackRoot* _next;
	StackRoot() : _next(NULL) {};
	virtual ~StackRoot() { this->detachFromGCRoot(); };
        virtual const char* repr() const {
            printf("%s:%d Subclass must implement repr\n", __FILE__, __LINE__ ); return "StackRoot";
        };

	void attachToGCRoot() {
	    this->_next = rooted_StackRoots;
	    rooted_StackRoots = this;
            printf("%s:%d attachToGCRoot for %s\n", __FILE__, __LINE__, this->repr() );
	}
	void detachFromGCRoot() {
	    rooted_StackRoots = this->_next;
#ifdef LOG_MPS
            printf("%s:%d StackRoot::detachFromGCRoot for %s\n", __FILE__, __LINE__, this->repr() );
#endif
	}
    };


};
#endif


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
    

