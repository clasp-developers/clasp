#ifndef _brcl_intrusiveRefCountGarbageCollection_H
#define _brcl_intrusiveRefCountGarbageCollection_H


#define GC_RESULT int
#define GC_SCAN_ARGS_PROTOTYPE int  ____dummy
#define GC_SCAN_ARGS_PASS  ____dummy
#define DECLARE_onHeapScanGCRoots() virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {return 0;};
#define DECLARE_onStackScanGCRoots() virtual GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {return 0;};
#define GC_SCANNER_BEGIN()
#define GC_SCANNER_END()
#define GC_RES_OK 0


namespace gctools {


    typedef enum {KIND_dummy} GCKindEnum;


    extern bool _GlobalDebugAllocations;


    /*! This is not used in intrusive reference counted garbage collection*/
    template <class OT>
    class GCInfo {
    public:
        static GCKindEnum const Kind = KIND_dummy;
    };




//#define USE_GC_REF_COUNT_WRAPPER          1

#ifdef USE_GC_REF_COUNT_WRAPPER
    template <typename T>
    struct GCWrapper {
        typedef         T WrappedType;
        struct GCHeader {
            class_id            _Kind;
            mutable uint        _ReferenceCount;
        };
        GCHeader        _Header;

        static size_t sizeof_wrapper() { return sizeof(GCHeader) + sizeof(T);};
        static size_t offsetof_object() { return sizeof(GCHeader); };

        // Variadic constructor
        GCWrapper() : _Header({reg::registered_class<T>::id,0}) {};

        template <typename U>
        static GCWrapper*  gcHeader(U* ptr) { return reinterpret_cast<GCWrapper*>(reinterpret_cast<char*>(dynamic_cast<void*>(ptr))-offsetof_object());};

        template <typename U>
        static const GCWrapper*  gcHeader(const U* ptr) { return reinterpret_cast<const GCWrapper*>(reinterpret_cast<const char*>(dynamic_cast<const void*>(ptr))-offsetof_object());};

        template <typename U>
        static void gcAddRef(U* ptr) {
            GCWrapper* gch = const_cast<GCWrapper<T>* >(GCWrapper::gcHeader(ptr));
            gch->_Header._ReferenceCount++;
#if 0
            if ( _GlobalDebugAllocations ) {
                printf("%s:%d - inc ref to %d at %p\n", __FILE__, __LINE__, gch->_Header._ReferenceCount, gch );
            }
#endif
        };

        template <typename U>
        static void gcRelease(U* ptr) {
            const GCWrapper* gch = GCWrapper::gcHeader(ptr);
            if ( --(gch->_Header._ReferenceCount) == 0 ) {
                T* obj = gch->gcobject();
                obj->~T();
                void* addr = reinterpret_cast<void*>(const_cast<GCWrapper<T>*>(gch));
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
        static int gcKind(U* ptr) {
            GCWrapper* gcw = GCWrapper::gcHeader(ptr);
            return gcw->_Header._Kind;
        };

        T* gcobject() const {return reinterpret_cast<T*>(reinterpret_cast<char*>(const_cast<GCWrapper<T>*>(this))+offsetof_object());}
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


#if 0
#define GC_RESERVE_MUTABLE_VECTOR_ALLOCATE(_T_,_SIZE_,_ADDR_) _ADDR_ = (vec::MutableVector<_T_>*)malloc(vec::MutableVector<_T_>::_sizeof(_SIZE_))
#define GC_RESERVE_MUTABLE_VECTOR_RESIZE(_T_,_OLD_,_NEW_SIZE_,_NEW_) { \
        GC_RESERVE_MUTABLE_VECTOR_ALLOCATE(_T_,_NEW_SIZE_,_NEW_);       \
        for ( size_t zi(0); zi<_OLD_->_End; ++zi ) {                   \
            _NEW_->operator[](zi) = _OLD_->operator[](zi);             \
        };                                                             \
        _NEW_->_End = _OLD_->_End;                                     \
    }
#define GC_RESERVE_MUTABLE_VECTOR_FREE(/*_T_,*/_ADDR_) free(_ADDR_);
#endif



#ifdef USE_GC_REF_COUNT_WRAPPER
#define GC_SIZEOF_BLOCK(_class_) (gctools::GCWrapper<_class_>::sizeof_wrapper())
#define GC_RESERVE_GET(_class_,_obj_) {                                 \
        gctools::GCWrapper<_class_>* __gch = reinterpret_cast<gctools::GCWrapper<_class_>*>(malloc(GC_SIZEOF_BLOCK(_class_))); \
        new(__gch) gctools::GCWrapper<_class_>();                       \
        _class_* __object = __gch->gcobject();                          \
        new(__object) _class_();                                        \
        _obj_ = __object;                                               \
    };

#define GC_RESERVE_GET_VARIADIC(_class_,_obj_,...) {                    \
    gctools::GCWrapper<_class_>* __gch = reinterpret_cast<gctools::GCWrapper<_class_>*>(malloc(GC_SIZEOF_BLOCK(_class_))); \
    new(__gch) gctools::GCWrapper<_class_>();                           \
    _class_* __object = __gch->gcobject();                              \
    new(__object) _class_(__VA_ARGS__);                                 \
    _obj_ = __object;
};
#define GC_COPY_GET(_class_,_obj_,_orig_) {                             \
        gctools::GCWrapper<_class_>* __gch = reinterpret_cast<gctools::GCWrapper<_class_>*>(malloc(GC_SIZEOF_BLOCK(_class_))); \
        new(__gch) gctools::GCWrapper<_class_>();                       \
        _class_* __object = __gch->gcobject();                          \
        new(__object) _class_(_orig_);                                  \
        _obj_ = __object;                                               \
    };
#else
#define GC_RESERVE_GET(_class_,_obj_) _obj_ = mem::smart_ptr<_class_>(new _class_())
#define GC_RESERVE_GET_VARIADIC(_class_,_obj_,...) _obj_ = mem::smart_ptr<_class_>(new _class_(__VA_ARGS__))
#define GC_COPY_GET(_class_,_obj_,_orig_) _obj_ = mem::smart_ptr<_class_>(new _class_(_orig_))
#endif

#define GC_RESERVE_GET_KIND(_class_,_kind_,_obj_) GC_RESERVE_GET(_class_,_obj_)

#define GC_RESERVE_BEGIN(_class_,_obj_) mem::smart_ptr<_class_> _obj_;
#define GC_RESERVE_END(_class_,_obj_) (_obj_)->initialize();POLL_SIGNALS()
#define GC_RESERVE_END_FINALno_INITno(_class_,_obj_) POLL_SIGNALS()
#define GC_RESERVE_END_FINALyes_INITno(_class_,_obj_) POLL_SIGNALS()


#define GC_RESERVE_END_DONT_INITIALIZE(_class_) POLL_SIGNALS()
#define GC_RESERVE(_class_,_obj_) GC_RESERVE_BEGIN(_class_,_obj_){GC_RESERVE_GET(_class_,_obj_);} GC_RESERVE_END(_class_,_obj_);
#define GC_RESERVE_VARIADIC(_class_,_obj_,...) GC_RESERVE_BEGIN(_class_,_obj_){GC_RESERVE_GET_VARIADIC(_class_,_obj_,__VA_ARGS__);} GC_RESERVE_END(_class_,_obj_);
#define GC_RESERVE_DONT_INITIALIZE(_class_,_obj_) GC_RESERVE_BEGIN(_class_,_obj_){GC_RESERVE_GET(_class_,_obj_);} GC_RESERVE_END_DONT_INITIALIZE(_obj_);

#define GC_COPY_BEGIN(_class_,_obj_) mem::smart_ptr<_class_> _obj_;
#define GC_COPY_END(_class_,_obj_) GC_RESERVE_END_DONT_INITIALIZE()
#define GC_COPY(_class_,_obj_,_orig_) GC_COPY_BEGIN(_class_,_obj_){GC_COPY_GET(_class_,_obj_,_orig_);} GC_COPY_END(_class_,_obj_);



/*! Return the most derived pointer of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) (dynamic_cast<void*>(_smartptr_.px_ref()))
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void*>(dynamic_cast<const void*>(_ptr_)))


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





namespace gctools
{
    class GCObject;
    class GCLinkedList;


    extern GCObject*	_GlobalAllocatedObjects;
    extern uint		_GlobalAllocationFingerprint;
    extern unsigned long long  _GlobalAllocationCounter;


    class GCObject {
#if 0
        template <class T> friend struct ref_counter;
	// general
    protected:
#endif
    public:
#ifndef USE_GC_REF_COUNT_WRAPPER
	mutable int _ReferenceCount;
    protected:
	// New ctor
	GCObject() : _ReferenceCount(0) {};
	// Copy  ctor
	GCObject(const GCObject& orig) : _ReferenceCount(0) {};
#endif
	GCObject& operator=(const GCObject&) { return *this; };
    };


};


namespace gctools
{
    /*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
    int initializeMemoryManagement( MainFunctionType startup, int argc, char* argv[], void* dummy );

};



#endif // _brcl_intrusiveRefCountGarbageCollection_H
