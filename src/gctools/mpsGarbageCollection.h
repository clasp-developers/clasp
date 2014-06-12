#ifndef _brcl_mpsGarbageCollection_H
#define _brcl_mpsGarbageCollection_H


namespace gctools {

#define GC_RESULT mps_res_t
#define GC_SCAN_STATE mps_ss_t


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
    extern const char* obj_name(GCKindEnum kind);

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

    typedef union Header_u *Header_t;

    /* Specialize this for every Header_s and set _Kind to its appropriate enum */
    struct Kind_s {
        Kind_s(GCKindEnum kind) : Kind(kind) {};
#ifdef CONFIG_VAR_COOL
        uint       StartIndicator;
#endif
	GCKindEnum	Kind;
    };
    struct Pad1_s : public Kind_s {};
    struct Pad_s : public Kind_s {
	size_t		Size;
    };
    struct Fwd2_s : public Kind_s {
	Header_t	Fwd;
    };
    struct Fwd_s : public Kind_s {
	Header_t	Fwd;
	size_t		Size;
    };
    typedef union Header_u {
        Header_u(GCKindEnum k) : kind(k) {};
	Kind_s	kind;
	Pad1_s	pad1;
	Pad_s	pad;
	Fwd2_s	fwd2;
	Fwd_s	fwd;
    } Header_s;


    /* Specialize this for every Header_s and set _Kind to its appropriate enum */
    struct TemplatedKind_s {
        TemplatedKind_s(GCKindEnum kind, size_t length) : Kind(kind), Length(length) {};
#ifdef CONFIG_VAR_COOL
        uint       StartIndicator;
#endif
	GCKindEnum	Kind;
        size_t          Length;
    };
    typedef union TemplatedHeader_u {
	TemplatedKind_s	kind;
	Pad1_s	pad1;
	Pad_s	pad;
	Fwd2_s	fwd2;
	Fwd_s	fwd;
    } TemplatedHeader_s;


};


namespace gctools {

    constexpr size_t Alignment() { return AlignmentT<Header_s>(); };
    constexpr size_t AlignUp(size_t size) { return AlignUpT<Header_s>(size); };


    template <class T> inline size_t sizeof_with_header() {return AlignUp(sizeof(T))+AlignUp(sizeof(Header_s));};


    template <> inline size_t sizeof_with_header<gctools::Kind_s>() { return AlignUp(sizeof(gctools::Kind_s)); };
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
    extern mps_pool_t _global_ams_pool;
    extern mps_pool_t _global_amc_pool;
    extern mps_pool_t _global_amcz_pool;
    extern mps_pool_t _global_awl_pool;

    extern mps_ap_t _global_automatic_mostly_copying_allocation_point;
    extern mps_ap_t _global_automatic_mostly_copying_zero_rank_allocation_point;
    extern mps_ap_t _global_automatic_mark_sweep_allocation_point;
    extern mps_ap_t _global_automatic_weak_link_allocation_point;


    /*! By default objects get allocated in the AMC pool */
    template <class T>
    struct allocation_point {
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




#define CONTAINER_FIX(_cptr_)                                           \
    if (_cptr_) {                                                       \
        mps_addr_t addr = MostDerivedPtrToBasePtr(_cptr_);                      \
        mps_res_t res = MPS_FIX12(GC_SCAN_STATE,&addr);                 \
        if (res != MPS_RES_OK) return res;                              \
        (_cptr_) = reinterpret_cast<decltype(_cptr_)>(BASE_TO_OBJ_PTR(addr)); \
    }

#define GCVEC0_FIX(_c_) {CONTAINER_FIX(_c_._Vector._Contents); };
#define GCFRAME0_FIX(_c_) {CONTAINER_FIX(_c_._Array._Contents); };
#define GCARRAY0_FIX(_c_) {CONTAINER_FIX(_c_._Array._Contents); };

        


#ifdef USE_TAGGED_PTR_P0
#define MY_MPS_FIX(_smartptr_)						\
    DEBUG_MPS_MESSAGE(boost::format("MY_MPS_FIX of %s@%p pbase: %p  px: %p") % #_smartptr_ % (&(_smartptr_)) % (_smartptr_).pbase_ref() % (_smartptr_).px_ref()); \
    if ( (_smartptr_).pointerp() ) {                                    \
	if ( MPS_FIX1(GC_SCAN_STATE,(_smartptr_).pbase_ref()) ) {          \
            void*& _pbase_ref = (_smartptr_).pbase_ref();                     \
	    int _offset = reinterpret_cast<char*>((_smartptr_).px_ref()) - reinterpret_cast<char*>(_pbase_ref); \
            /*DEBUG_MPS_FIX_BEFORE(_pbase_ref,reinterpret_cast<void*>((_smartptr_).px_ref()),_offset); */  \
	    mps_res_t res = MPS_FIX2(GC_SCAN_STATE,&_pbase_ref);		\
            if (res != MPS_RES_OK) return res;                          \
            /* (_smartptr_)._pbase_set(_pbase_ref);*/                   \
            (_smartptr_)._pxset(reinterpret_cast<char*>(_pbase_ref) + _offset); \
            /*DEBUG_MPS_FIX_AFTER((_smartptr_).pbase_ref(),(_smartptr_).px_ref());*/ \
        }								\
    };
#else
#define MY_MPS_FIX(_smartptr_)						\
    DEBUG_MPS_MESSAGE(boost::format("MY_MPS_FIX of %s@%p pbase: %p  px: %p") % #_smartptr_ % (&(_smartptr_)) % (_smartptr_).pbase_ref() % (_smartptr_).px_ref()); \
    if ( (_smartptr_).pointerp() ) {                                    \
	if ( MPS_FIX1(GC_SCAN_STATE,(_smartptr_).px_ref()) ) {          \
	    mps_res_t res = MPS_FIX2(GC_SCAN_STATE,&(_smartptr_).px_ref()); \
            if (res != MPS_RES_OK) return res;                          \
        }								\
    };
#endif // USE_TAGGED_PTR_P0


#define IGNORE(_ptr_)
#define SMART_PTR_FIX(_ptr_) {MY_MPS_FIX(_ptr_);};
#define HANDLE_POINTER_CTYPE(_ptr_) { if (_ptr_) _ptr_->onHeapScanGCRoots(GC_SCAN_ARGS_PASS); };
#define HANDLE_POINTER_TO_SMART_PTR(_ptr_) { if (_ptr_) {SMART_PTR_FIX(*_ptr_);};}
#define HANDLE_CXXRECORD_CTYPE(_var_) { _var_.onHeapScanGCRoots(GC_SCAN_ARGS_PASS); };
#define MAYBE_HANDLE_POINTER_CTYPE(_ptr_) { IMPLEMENT_MEF(BF("Handle MAYBE_HANDLE_POINTER_CTYPE"));};
#define HANDLE_MAYBE_INTERESTING_CTYPE(_ptr_) {IMPLEMENT_MEF(BF("HANDLE_MAYBE_INTERESTING_CTYPE macro undefined"));};


#define MAYBE_HANDLE_CLASS_TEMPLATE_SPECIALIZATION_CTYPE(_val_) {IMPLEMENT_MEF(BF("Handle MAYBE_HANDLE_CLASS_TEMPLATE_SPECIALIZATION_CTYPE"));};

#define WEAK_SMART_PTR_FIX(_ptr_) {MY_MPS_FIX(_ptr_);};

#define GCHOLDER_STRINGMAP_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("GCHOLDER_STRINGMAP",_map_); \
	if ( (_map_).size()>0 ) {					\
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.second); }}}
#define GCHOLDER_VECTOR0_FIX(_vec_) {					\
	DEBUG_MPS_CONTAINER("GCHOLDER_VECTOR0",_vec_);			\
	if ( (_vec_).size()>0 ) {                                       \
            DEBUG_MPS_MESSAGE(BF("Started GCHOLDER_VECTOR0_FIX"));      \
            for ( auto& __itr_gc_safe : _vec_) { SMART_PTR_FIX(__itr_gc_safe);} \
            DEBUG_MPS_MESSAGE(BF("Done GCHOLDER_VECTOR0_FIX"));         \
        }}
#define GCHOLDER_SYMBOLMAP_FIX(_cont_) { \
	DEBUG_MPS_CONTAINER("GCHOLDER_SYMBOLMAP",_cont_);	\
	if ( (_cont_).size()>0 ) {				\
            for ( auto& __itr_gc_safe : _cont_ ) { SMART_PTR_FIX(__itr_gc_safe.first); SMART_PTR_FIX(__itr_gc_safe.second); }}}
#define GCHOLDER_UNORDEREDSET_FIX(_cont_) { \
	DEBUG_MPS_CONTAINER("GCHOLDER_UNORDEREDSET",_cont_);	\
	if ( (_cont_).size()>0 ) {				\
            for ( auto& __itr_gc_safe : _cont_ ) { SMART_PTR_FIX(__itr_gc_safe);}}}

#define GCHOLDER_INDEXEDSYMBOLMAP_FIX(_map_) {					\
	DEBUG_MPS_CONTAINER("GCHOLDER_INDEXEDSYMBOLMAP",_map_); \
	if ( (_map_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.first);}; \
	    for ( auto& v__itr_gc_safe : _map_._Values ) { SMART_PTR_FIX(v__itr_gc_safe);};}}
#define STLVECTOR_FIX(_vec_) { \
	DEBUG_MPS_CONTAINER("STLVECTOR",_vec_); \
	if ( (_vec_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _vec_) { SMART_PTR_FIX(__itr_gc_safe); }}}
#define STLVECTOR_HANDLE_FIX(_vec_) { \
	DEBUG_MPS_CONTAINER("STLVECTOR",_vec_); \
	if ( (_vec_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _vec_) { __itr_gc_safe.onHeapScanGCRoots(GC_SCAN_ARGS_PASS); }}}
#define STLSET_FIX(_set_) { \
	DEBUG_MPS_CONTAINER("STLSET",_set_); \
	if ( (_set_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _set_ ) { SMART_PTR_FIX(__itr_gc_safe); }}}
#define STLMAP_SMART_FIRST_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("STLMAP_SMART_FIRST",_map_); \
	if ( (_map_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.first); }}}
#define STLMAP_SMART_SECOND_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("STLMAP_SMART_SECOND",_map_); \
	if ( (_map_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.second); }}}
#define STLMAP_SMART_FIRST_SECOND_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("STLMAP_SMART_FIRST_SECOND",_map_); \
	if ( (_map_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _map_) { SMART_PTR_FIX(__itr_gc_safe.first);SMART_PTR_FIX(__itr_gc_safe.second); }}}

#define STLMAP_TEMPLATE_SCANNER_FIX(_map_) {                      \
	DEBUG_MPS_CONTAINER("STLMAP_TEMPLATE_SCANNER_FIX",_map_); \
	if ( (_map_).size() > 0 ) { \
	    for ( auto __itr_gc_safe = _map_.begin(); __itr_gc_safe != _map_.end(); ++__itr_gc_safe) { gctools::stl_onHeapScanGCRoots(__itr_gc_safe,GC_SCAN_ARGS_PASS); }}}



#define STLMULTIMAP_SMART_FIRST_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("STLMULTIMAP_SMART_FIRST",_map_); \
	if ( (_map_).size() > 0 ) { \
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.first); }}}
#define STLMULTIMAP_SMART_SECOND_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("STLMULTIMAP_SMART_SECOND",_map_);		\
	if ( (_map_).size() > 0 ) {					\
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.second); }}}
#define STLMULTIMAP_SMART_FIRST_SECOND_FIX(_map_) { \
	DEBUG_MPS_CONTAINER("STLMULTIMAP_SMART_FIRST_SECOND",_map_);	\
	if ( (_map_).size() > 0 ) {					\
	    for ( auto& __itr_gc_safe : _map_ ) { SMART_PTR_FIX(__itr_gc_safe.first);SMART_PTR_FIX(__itr_gc_safe.second); }}}
#if 0
#define STL_VECTOR_REQUIRED_ARGUMENT_FIX(_vec_) { \
	DEBUG_MPS_CONTAINER("STD_VECTOR_REQUIRED_ARGUMENT",_vec_);	\
	if ( (_vec_).size() > 0 ) {					\
            for ( auto& __itr_gc_safe : _vec_ ) { SMART_PTR_FIX(__itr_gc_safe._ArgTarget); }}
#define STL_VECTOR_OPTIONAL_ARGUMENT_FIX(_vec_) { \
	DEBUG_MPS_CONTAINER("STD_VECTOR_OPTIONAL_ARGUMENT",_vec_);	\
	if ( (_vec_).size()>0 ) {					\
	    for ( auto& __itr = _vec_.begin(); __itr != _vec_.end(); ++__itr ) \
	    {								\
		SMART_PTR_FIX(__itr->_ArgTarget);			\
		SMART_PTR_FIX(__itr->_Default);				\
		SMART_PTR_FIX(__itr->_Sensor._ArgTarget);		\
	    }}}
#define REST_ARGUMENT_FIX(_arg_) {		\
	SMART_PTR_FIX(_arg_._ArgTarget);	\
    }
#define STL_VECTOR_KEYWORD_ARGUMENT_FIX(_vec_) { \
	DEBUG_MPS_CONTAINER("STD_VECTOR_KEYWORD_ARGUMENT",_vec_);	\
	if ( (_vec_).size() > 0 ) {					\
	    for ( auto& __itr = _vec_.begin(); __itr != _vec_.end(); ++__itr ) \
	    {								\
		SMART_PTR_FIX(__itr->_Keyword);				\
		SMART_PTR_FIX(__itr->_ArgTarget);			\
		SMART_PTR_FIX(__itr->_Default);				\
		SMART_PTR_FIX(__itr->_Sensor._ArgTarget);		\
	    }}}
#define STL_VECTOR_AUX_ARGUMENT_FIX(_vec_) {				\
	DEBUG_MPS_CONTAINER("STD_VECTOR_AUX_ARGUMENT",_vec_);		\
	if ( (_vec_).size() > 0 ) {					\
	    for ( auto& __itr = _vec_.begin(); __itr != _vec_.end(); ++__itr ) \
	    {								\
		SMART_PTR_FIX(__itr->_ArgTarget);			\
		SMART_PTR_FIX(__itr->_Expression);			\
	    }}}
#endif


extern "C" {

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
