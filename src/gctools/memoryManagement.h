#ifndef _brcl_memoryManagement_H
#define _brcl_memoryManagement_H

#include "hardErrors.h"


#ifdef USE_BOEHM
#include "gc/gc.h"
#include "gc/gc_allocator.h"
#endif // USE_BOEHM


typedef int (*MainFunctionType)(int argc, char* argv[], bool& mpiEnabled, int& mpiRank, int& mpiSize );

#define GC_LOG(x)


namespace gctools {
     template <class OT>
    struct GCAllocatorInfo {
        static bool constexpr NeedsInitialization = true;
        static bool constexpr NeedsFinalization = true;
        static bool constexpr Moveable = true;
        static bool constexpr Atomic = false;
    };
};

#include "hardErrors.h"


#ifndef USE_MPS

#define GC_RESULT int
#define GC_SCAN_ARGS_PROTOTYPE int  ____dummy
#define GC_SCAN_ARGS_PASS  ____dummy
#define DECLARE_onHeapScanGCRoots() virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {return 0;};
#define DECLARE_onStackScanGCRoots() virtual GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {return 0;};
#define GC_SCANNER_BEGIN()
#define GC_SCANNER_END()
#define GC_RES_OK 0


namespace gctools {


    typedef enum {KIND_null} GCKindEnum;



};
#else // USE_MPS


extern "C" 
{
#include "mps/code/mps.h"
#include "mps/code/mpsavm.h"
};

#define MPS_RES_T int
#define MPS_SS_T struct mps_ss_s*
#define MPS_ADDR_T void*

#define INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(x) ;

#define GC_RESULT 	mps_res_t
//! Lexical variable used to store the scan state
#define GC_SCAN_STATE 	gc__scan_state
#define GC_SCAN_ARGS_PROTOTYPE 	mps_ss_t GC_SCAN_STATE/* , mps_thr_t gc__thr, void* gc__p, size_t gc__s */
#define GC_SCAN_ARGS_PASS GC_SCAN_STATE /* , gc__thr, gc__p, gc__s */
#define DECLARE_onHeapScanGCRoots() virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE);
#define DECLARE_onStackScanGCRoots() virtual GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE);
#define GC_SCANNER_BEGIN() MPS_SCAN_BEGIN(GC_SCAN_STATE)
#define GC_SCANNER_END() MPS_SCAN_END(GC_SCAN_STATE)
#define GC_RES_OK MPS_RES_OK

namespace gctools {



#ifndef RUNNING_GC_BUILDER
#define GC_ENUM
typedef 
#include GARBAGE_COLLECTION_INCLUDE //"main/clasp_gc.cc"
GCKindEnum ;
#undef GC_ENUM
#else
    typedef enum { KIND_null, KIND_SYSTEM_fwd, KIND_SYSTEM_fwd2, KIND_SYSTEM_pad1, KIND_SYSTEM_pad } GCKindEnum;
#endif
};


#endif // USE_MPS











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

        virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
#ifdef  USE_MPS
        = 0;
#else
        {return GC_RES_OK;};
#endif


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
        virtual GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
#ifdef  USE_MPS
        = 0;
#else
        {return GC_RES_OK;};
#endif

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

#include "smart_pointers.h"

#include "gcalloc.h"

#ifdef USE_MPS
#include "mpsGarbageCollection.h"
#else
#include "intrusiveRefCountGarbageCollection.h"
#endif



namespace gctools {


    /*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
    template <class OT>
    struct GCInfo {
#if defined(RUNNING_GC_BUILDER) || !defined(USE_MPS)
        // We need a default Kind when running the gc-builder.lsp static analyzer
        // but we don't want a default Kind when compiling the mps version of the code
        // to force compiler errors when the Kind for an object hasn't been declared
        static GCKindEnum const Kind = KIND_null;
#endif
        static bool const NeedsInitialization = true; // Currently everything needs initialization
        static bool const NeedsFinalization = true; // Currently everything needs finalization

    };


};


#define SUPPRESS_GC() {}
#define ENABLE_GC() {}




namespace gctools {

    template <class T>
    class StackRootedPointer : public gctools::StackRoot {
    public:
        typedef T   PointeeType;
        typedef T*  PtrType;
        PtrType _px;
    public:
        StackRootedPointer() : _px(NULL) {};
        StackRootedPointer(PtrType p) : _px(p) {};
//    PtrType operator*() const {return *this->_px;};
        PointeeType const operator*() { return *this->_px;};
        PtrType const operator->() { return this->_px;};
        bool nullP() const { return !this->_px;};
        void set(PtrType px) { this->_px = px; };
        PtrType get() const { return this->_px;};
        PtrType& operator++() { return (++(this->_px));};
        GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
        {
#ifdef USE_MPS
            if ( this->_px ) {
                return this->_px->onHeapScanGCRoots(GC_SCAN_ARGS_PASS);
            }
#endif
            return GC_RES_OK;
        };

        virtual ~StackRootedPointer() {this->_px = NULL;};
    };

};



#include "containers.h"




#endif // _brcl_memoryManagement_H
