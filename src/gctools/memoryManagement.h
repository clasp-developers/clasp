#ifndef _brcl_memoryManagement_H
#define _brcl_memoryManagement_H

#include "hardErrors.h"

typedef int (*MainFunctionType)(int argc, char* argv[], bool& mpiEnabled, int& mpiRank, int& mpiSize );

#define GC_LOG(x)

#ifdef USE_MPS
#include "mpsGarbageCollection.h"
#else
#include "intrusiveRefCountGarbageCollection.h"
#endif

#define SUPPRESS_GC() {}
#define ENABLE_GC() {}



namespace gctools {

#if 0
    /*! This class connects the holder to the root */
    class RootedGCHolder {
    };

    /*! Dummy class to identify Holders that are GC'd */
    class GCOnHeap {
    };
#endif



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



    struct HeapRoot;
    struct StackRoot;

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
#ifndef USE_MPS
    {
        return GC_RES_OK;
    }
#else
    {
        if ( this->_px ) {
            return this->_px->onHeapScanGCRoots(GC_SCAN_ARGS_PASS);
        }
        return GC_RES_OK;
    };
#endif
        ;

    virtual ~StackRootedPointer() {this->_px = NULL;};
};

#if 0
    // USE OVERLOADS instead of template functions
    template <typename T >
    GC_RESULT stl_onHeapScanGCRoots( T& val, GC_SCAN_ARGS_PROTOTYPE ) {
        printf("%s:%d %s Implement onHeapScanGCRoots for stl container content\n", __FILE__, __LINE__, __FUNCTION__);
        return GC_RES_OK;
    }
#endif

    template <typename StlContainerType>
    class StackRootedStlContainer : public gctools::StackRoot  {
    public:
        StlContainerType        _Container;
    public:
        StlContainerType& get() { return this->_Container;};
        GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) 
#ifndef USE_MPS
        {
            return GC_RES_OK;
        }
#else
        {
            GC_RESULT res;
            for ( auto it_gc_safe : this->_Container ) {
                res = it_gc_safe.onHeapScanGCRoots(GC_SCAN_ARGS_PASS ); // .onHeapScanGCRoots(GC_SCAN_ARGS_PASS);
                if ( UNLIKELY(res!=GC_RES_OK) ) {
                    return res;
                }
            }
            return GC_RES_OK;
        }
#endif

    };



};



#include "containers.h"




#endif // _brcl_memoryManagement_H
