#ifndef _brcl_mpsGarbageCollection_H
#define _brcl_mpsGarbageCollection_H

#include <type_traits>


extern "C" {
    typedef struct SegStruct* Seg;
    typedef mps_arena_t Arena;
    typedef mps_addr_t Addr;
    extern int SegOfAddr(Seg *segReturn, Arena arena, Addr addr);
//    extern int SegPM(Seg segReturn);
    extern void ShieldExpose(Arena arena, Seg seg);
    extern void ShieldCover(Arena arena, Seg seg);
};

namespace gctools {

#define GC_RESULT mps_res_t
#define GC_SCAN_STATE_TYPE mps_ss_t
#define GC_SCAN_STATE ss


    class GCObject
    {
    public:
//	bool isNil() const { return false;};
//	bool isUnbound() const { return false;};
//	bool isObject() const { return true;};
        virtual ~GCObject() {};
    };



#if !defined(RUNNING_GC_BUILDER)
#define GC_ENUM
    typedef
#include GARBAGE_COLLECTION_INCLUDE //"main/clasp_gc.cc"
    GCKindEnum ;
#undef GC_ENUM
#else
    typedef enum { KIND_null, KIND_SYSTEM_fwd, KIND_SYSTEM_fwd2, KIND_SYSTEM_pad1, KIND_SYSTEM_pad } GCKindEnum;
#endif
};


namespace gctools {

    template <class T> inline size_t sizeof_with_header();


    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    //
    // Define Header_s and stuff that can exist in the header
    //
    //

    template <typename T> union Header_s;

//    typedef Header_s<void>* Header_t;

    

    /* Specialize this for every Header_s and set _Kind to its appropriate enum */
    template <typename T>
    struct SKind_s {
        SKind_s() : Kind(GCKind<T>::Kind)
                       , Length(sizeof_with_header<T>())
        {};
        SKind_s(size_t sz) : Kind(GCKind<T>::Kind)
                                , Length(sz)
        {};
	unsigned int	Kind;
        unsigned int    Length;
    };
    struct Pad1_s : public SKind_s<Pad1_s> {};
    struct Pad_s : public SKind_s<Pad_s> {
    };
    struct Fwd2_s : public SKind_s<Fwd2_s> {
	Header_s<void>*	Fwd;
    };
    struct Fwd_s : public SKind_s<Fwd_s> {
	Header_s<void>*	Fwd;
    };




    template <typename T>
    union Header_s {
        Header_s() : kind(sizeof_with_header<T>()) {};
        Header_s(size_t sz) : kind(sz) {};
	SKind_s<T>	        kind;
//        TemplatedKind_s<T>      templatedKind;
	Pad1_s	                pad1;
	Pad_s	                pad;
	Fwd2_s	                fwd2;
	Fwd_s                   fwd;
        static size_t HeaderSize() { return sizeof(Header_s<T>);};
    };

};


namespace gctools {

    constexpr size_t Alignment() { return AlignmentT<Header_s<void>>(); };
    constexpr size_t AlignUp(size_t size) { return AlignUpT<Header_s<void>>(size); };


    template <class T> inline size_t sizeof_with_header() {return AlignUp(sizeof(T))+GCHeader<T>::HeaderType::HeaderSize();};

    template <> inline size_t sizeof_with_header<gctools::Pad1_s>() { return AlignUp(sizeof(gctools::Pad1_s)); };
    template <> inline size_t sizeof_with_header<gctools::Pad_s>() { return AlignUp(sizeof(gctools::Pad_s)); };
    template <> inline size_t sizeof_with_header<gctools::Fwd_s>() { return AlignUp(sizeof(gctools::Fwd_s)); };
    template <> inline size_t sizeof_with_header<gctools::Fwd2_s>() { return AlignUp(sizeof(gctools::Fwd2_s)); };
};

/* Align size upwards and ensure that it's big enough to store a
 * forwarding pointer.
 * This is used by the obj_scan and obj_skip methods
 */
/*   Replaces this macro...
     #define ALIGN(size)                                                \
    (AlignUp<Header_s>(size) >= AlignUp<Header_s>(sizeof_with_header<gctools::Fwd_s>())	\
     ? AlignUp<Header_s>(size)                              \
     : gctools::sizeof_with_header<gctools::Fwd_s>() ) 
*/
namespace gctools {
    inline size_t Align(size_t size) {
        return (AlignUp(size) >= AlignUp(sizeof_with_header<Fwd_s>())
                ? AlignUp(size)
                : sizeof_with_header<Fwd_s>() );
    };
};

namespace gctools {

#define NON_MOVING_POOL_ALLOCATION_POINT _global_automatic_mark_sweep_allocation_point

    extern mps_arena_t _global_arena;

    extern mps_pool_t _global_amc_pool;
    extern mps_pool_t _global_ams_pool;
    extern mps_pool_t _global_amcz_pool;

    extern mps_ap_t _global_automatic_mostly_copying_allocation_point;
    extern mps_ap_t _global_automatic_mark_sweep_allocation_point;
    extern mps_ap_t _global_automatic_mostly_copying_zero_rank_allocation_point;


#ifdef USE_AWL_POOL
    extern mps_pool_t _global_awl_pool;
    extern mps_ap_t _global_automatic_weak_link_allocation_point;
#endif


    template <typename T> struct GCAllocationPoint
    {
#ifdef USE_AMC_POOL
        static mps_ap_t get() { return _global_automatic_mostly_copying_allocation_point; };
#define DEFAULT_ALLOCATION_POINT _global_automatic_mostly_copying_allocation_point
#else
        static mps_ap_t get() { return NON_MOVING_POOL_ALLOCATION_POINT; };
#define DEFAULT_ALLOCATION_POINT NON_MOVING_POOL_ALLOCATION_POINT
#endif
    };

};

namespace core {
    class Fixnum_O;
    class Cons_O;
};

#ifdef USE_PUT_SELECT_CLASSES_IN_AMC_POOL
//
// Turn this on if you want to allocate just a few classes in an AMC pool
//
namespace gctools {
#define AMC_AP _global_automatic_mostly_copying_allocation_point
#if 0
    template <>
    struct allocation_point<core::Fixnum_O> {
        static mps_ap_t get() { return AMC_AP;};
    };
#endif
    template <>
    struct allocation_point<core::Cons_O> {
        static mps_ap_t get() { return AMC_AP;};
    };
};
#endif








/* ------------------------------------------------------------
   ------------------------------------------------------------

   Macros for fixing pointers managed by GC

   ------------------------------------------------------------
*/


/*! Return the block address of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) ((_smartptr_).pbase_ref())
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void*>(dynamic_cast<const void*>(_ptr_)))





namespace gctools {

    template <typename T>
    inline void* MostDerivedPtrToBasePtr(void* mostDerived)
    {
        void* ptr = reinterpret_cast<char*>(mostDerived) - (GCHeader<T>::HeaderType::HeaderSize());
        return ptr;
    }

    template <typename T>
    inline T* BasePtrToMostDerivedPtr(void* base)
    {
        T* ptr = reinterpret_cast<T*>(reinterpret_cast<char*>(base) + (GCHeader<T>::HeaderType::HeaderSize()));
        return ptr;
    }

};


        

#if 1 // use inlined functions rather than macros

//       do { mps_ss_t _ss = (ss); mps_word_t _mps_zs = (_ss)->_zs; mps_word_t _mps_w = (_ss)->_w; mps_word_t _mps_ufs = (_ss)->_ufs; mps_word_t _mps_wt; { {

namespace gctools {
    template <typename T> class smart_ptr;
};

template <typename T>
inline mps_res_t smartPtrFix(mps_ss_t _ss
                        , mps_word_t _mps_zs
                        , mps_word_t _mps_w
                        , mps_word_t& _mps_ufs
                        , mps_word_t _mps_wt
                        , const gctools::smart_ptr<T>* sptrP
#ifdef DEBUG_MPS
                        , const char* sptr_name
#endif
    ) {
    DEBUG_MPS_MESSAGE(boost::format("MY_MPS_FIX of %s@%p px: %p") % sptr_name % (sptrP)  % (sptrP)->px_ref()); 
    if ( sptrP->pointerp() ) {                                    
	if ( MPS_FIX1(_ss,(sptrP)->px_ref()) ) {          
            Seg seg;
            mps_addr_t mostDerived;
            if (SegOfAddr(&seg,gctools::_global_arena,sptrP->px_ref())) {
//                bool segpm = SegPM(seg);
//                if ( segpm ) {
                    ShieldExpose(gctools::_global_arena,seg);
                    mostDerived = dynamic_cast<void*>(sptrP->px_ref());
                    ShieldCover(gctools::_global_arena,seg);
//                } else {
//                    mostDerived = dynamic_cast<void*>(sptrP->px_ref());
//                }
            } else {
                THROW_HARD_ERROR(BF("SegOfAddr for address: %p failed - this should never happen") % sptrP->px_ref());
            }
            mps_addr_t base = gctools::MostDerivedPtrToBasePtr<void>(mostDerived); 
	    int offset = reinterpret_cast<char*>((sptrP)->px_ref()) - reinterpret_cast<char*>(base); 
            DEBUG_MPS_MESSAGE(boost::format("  px_ref()=%p mostDerived=%p  base=%p  offset=%d  seg=%p") % sptrP->px_ref() % mostDerived % base % offset % seg ); 
	    mps_res_t res = MPS_FIX2(_ss,reinterpret_cast<mps_addr_t*>(&base)); 
            DEBUG_MPS_MESSAGE(boost::format("  new_base=%p") % base);
            if (res != MPS_RES_OK) return res;              
            mps_addr_t new_obj = reinterpret_cast<void*>(reinterpret_cast<char*>(base)+offset);
            DEBUG_MPS_MESSAGE(boost::format("  old_obj=%p  new_obj = %p\n") % (sptrP->px_ref()) % new_obj ); 
            (sptrP)->_pxset(new_obj); 
        }								
    };
    return MPS_RES_OK;
};

#ifdef DEBUG_MPS
#define SMART_PTR_FIX(_smartptr_) smartPtrFix(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,&_smartptr_,#_smartptr_)
#else
#define SMART_PTR_FIX(_smartptr_) smartPtrFix(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,&_smartptr_)
#endif

#else
#define SMART_PTR_FIX(_smartptr_)						\
    DEBUG_MPS_MESSAGE(boost::format("MY_MPS_FIX of %s@%p px: %p") % #_smartptr_ % (&(_smartptr_))  % (_smartptr_).px_ref()); \
    if ( (_smartptr_).pointerp() ) {                                    \
	if ( MPS_FIX1(GC_SCAN_STATE,(_smartptr_).px_ref()) ) {          \
            mps_addr_t mostDerived = (_smartptr_).pbase();              \
            mps_addr_t base = MostDerivedPtrToBasePtr<void>(mostDerived); \
	    int offset = reinterpret_cast<char*>((_smartptr_).px_ref()) - reinterpret_cast<char*>(base); \
            DEBUG_MPS_MESSAGE(boost::format("  mostDerived=%p  base=%p  offset=%d\n") % mostDerived % base % offset ); \
	    mps_res_t res = MPS_FIX2(GC_SCAN_STATE,reinterpret_cast<mps_addr_t*>(&base)); \
            if (res != MPS_RES_OK) return res;                          \
            (_smartptr_)._pxset(reinterpret_cast<void*>(reinterpret_cast<char*>(base)+offset)); \
            DEBUG_MPS_MESSAGE(boost::format("  new base = %p\n") % base ); \
        }								\
    };
#endif

#define POINTER_FIX(_ptr_)                                              \
    if ( MPS_FIX1(GC_SCAN_STATE,_ptr_) ) {                              \
        void** ptrP = reinterpret_cast<void**>(&_ptr_);                 \
        mps_addr_t base = MostDerivedPtrToBasePtr<void>(_ptr_);         \
        mps_res_t res = MPS_FIX2(GC_SCAN_STATE,reinterpret_cast<mps_addr_t*>(&(base))); \
        if (res != MPS_RES_OK) return res;                              \
        *ptrP = BasePtrToMostDerivedPtr<void>(base);                    \
    };


extern "C" {

    const char* obj_name(gctools::GCKindEnum kind);

    /*! Implemented in gc_interace.cc */
    mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);

    /*! Implemented in gc_interace.cc */
    mps_addr_t obj_skip(mps_addr_t base);

    /*! Implemented in gc_interace.cc */
    void obj_finalize(mps_addr_t base);

    /*! This must be implemented in the main directory */
    extern mps_res_t main_thread_roots_scan(mps_ss_t GC_SCAN_STATE, void *p, size_t s);
};











namespace gctools
{

    /*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
    int initializeMemoryPoolSystem( MainFunctionType startup, int argc, char* argv[],  mps_fmt_auto_header_s* mps_fmt, bool mpiEnabled, int mpiRank, int mpiSize );




    /*! Search the heap and the stack for an address and print hits
      This can't currently be called from within obj_skip - so it's not
      useful.    Come up with another way to determine ownership of pointers */
    void searchHeapAndStackForAddress(mps_addr_t addr);

};


extern "C" {

    /*! Return the number of messages processed */
    extern int processMpsMessages(void );


};


#endif // _brcl_memoryPoolSystem_H
