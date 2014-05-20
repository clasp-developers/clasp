#ifndef _brcl_mpsGarbageCollection_H
#define _brcl_mpsGarbageCollection_H

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



    class GCObject
    {
    public:
	bool isNil() const { return false;};
	bool isUnbound() const { return false;};
	bool isObject() const { return true;};
        virtual ~GCObject() {};
    };

    extern const char* obj_name(GCKindEnum kind);


/*! This template class wraps a lisp object and defines the kind of the lisp object */

    /*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
    template <class OT>
    class GCInfo {
    public:
        static GCKindEnum const Kind = KIND_null;
    };



    typedef union Header_u *Header_t;

    /* Specialize this for every Header_s and set _Kind to its appropriate enum */
    struct Kind_s {
#ifdef CONFIG_VAR_COOL
        uintptr_t       _StartIndicator;
#endif
	GCKindEnum	_Kind;
#ifdef DEBUG_MPS_AMS_POOL
        uintptr_t       _OwnerInfo;
        void setOwner(void* ptr) { this->_OwnerInfo = reinterpret_cast<uintptr_t>(ptr);};
        void setOwner(uintptr_t val) { this->_OwnerInfo = val;};
#endif
    };
    struct Pad1_s : public Kind_s {};
    struct Pad_s : public Kind_s {
	size_t		_Size;
    };
    struct Fwd2_s : public Kind_s {
	Header_t	_Fwd;
    };
    struct Fwd_s : public Kind_s {
	Header_t	_Fwd;
	size_t		_Size;
    };
    typedef union Header_u {
	Kind_s	kind;
	Pad1_s	pad1;
	Pad_s	pad;
	Fwd2_s	fwd2;
	Fwd_s	fwd;
    } Header_s;

#ifdef CONFIG_VAR_COOL
#define DEBUG_MARK_BLOCK_START(header) header->kind._StartIndicator = 0xdeadbeef
#else
#define DEBUG_MARK_BLOCK_START(header)
#endif

};

#define ALIGNMENT alignof(gctools::Fwd_s)
#define ALIGN_UP(size) \
  (((size) + ALIGNMENT - 1) & ~(ALIGNMENT - 1))

template <class T> inline size_t sizeof_with_header() {return ALIGN_UP(sizeof(T))+ALIGN_UP(sizeof(gctools::Header_s));};
template <> inline size_t sizeof_with_header<gctools::Kind_s>() { return ALIGN_UP(sizeof(gctools::Kind_s)); };
template <> inline size_t sizeof_with_header<gctools::Pad1_s>() { return ALIGN_UP(sizeof(gctools::Pad1_s)); };
template <> inline size_t sizeof_with_header<gctools::Pad_s>() { return ALIGN_UP(sizeof(gctools::Pad_s)); };
template <> inline size_t sizeof_with_header<gctools::Fwd_s>() { return ALIGN_UP(sizeof(gctools::Fwd_s)); };
template <> inline size_t sizeof_with_header<gctools::Fwd2_s>() { return ALIGN_UP(sizeof(gctools::Fwd2_s)); };

/* Align size upwards and ensure that it's big enough to store a
 * forwarding pointer. */
#define ALIGN(size)                                \
    (ALIGN_UP(size) >= ALIGN_UP(sizeof_with_header<gctools::Fwd_s>())	\
     ? ALIGN_UP(size)                              \
     : ALIGN_UP(sizeof_with_header<gctools::Fwd_s>()))


#define SIZEOF_WITH_HEADER(_class_) sizeof_with_header<_class_>()



namespace gctools {

#define NON_MOVING_POOL_ALLOCATION_POINT _global_automatic_mark_sweep_allocation_point

    extern mps_arena_t _global_arena;
    extern mps_pool_t _global_ams_pool;
    extern mps_pool_t _global_amc_pool;

    extern mps_ap_t _global_automatic_mostly_copying_allocation_point;
    extern mps_ap_t _global_automatic_mark_sweep_allocation_point;


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


#ifndef RUNNING_GC_BUILDER
#ifdef NON_MOVING_CLASSES
#include GARBAGE_COLLECTION_INCLUDE
#endif // NON_MOVING_CLASSES
#endif





#if 0
#define GC_RESERVE_MUTABLE_VECTOR_ALLOCATE(_T_,_SIZE_,_ADDR_) IMPLEMENT_ME();
#define GC_RESERVE_MUTABLE_VECTOR_RESIZE(_T_,_OLD_,_NEW_SIZE_,_NEW_) IMPLEMENT_ME();
#endif





#define _GC_RESERVE_BEGIN_BASE(_class_,_obj_) \
    mem::smart_ptr<_class_> _obj_;           \
    {                                        \
    mps_addr_t __reserve_addr(0);            \
    do {

#define _GC_RESERVE_GET_BASE(_class_,_kind_,_obj_)                \
    mps_res_t __gc_res = mps_reserve(&__reserve_addr,gctools::allocation_point<_class_>::get(), sizeof_with_header<_class_>()); \
    if ( __gc_res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Could not allocate %s") % #_class_ ); \
    gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(__reserve_addr); \
    DEBUG_MARK_BLOCK_START(header);                                     \
    header->kind._Kind = (gctools::GCKindEnum)(_kind_);                 \
    void* __obj_addr = BASE_TO_OBJ_PTR(__reserve_addr);                         \
    DEBUG_MPS_ALLOCATION(__reserve_addr,__obj_addr,sizeof_with_header<_class_>(),_kind_); \
    _obj_ = new (__obj_addr) /* Followed by the class in EVERY usage of GC_RESERVE_GET_BASE */

#define _GC_RESERVE_END_BASE(_class_) }while (!mps_commit(gctools::allocation_point<_class_>::get(),__reserve_addr,sizeof_with_header<_class_>())); 

#define _GC_RESERVE_FINALIZE() mps_finalize(gctools::_global_arena,&__reserve_addr)

#define _GC_RESERVE_END_FINALyes_INITno(_class_,_obj_) _GC_RESERVE_END_BASE(_class_); _GC_RESERVE_FINALIZE(); } POLL_SIGNALS()
#define _GC_RESERVE_END_FINALno_INITno(_class_,_obj_) _GC_RESERVE_END_BASE(_class_); } POLL_SIGNALS()
#define _GC_RESERVE_END_FINALyes_INITyes(_class_,_obj_) _GC_RESERVE_END_BASE(_class_); _GC_RESERVE_FINALIZE(); _obj_->initialize(); } POLL_SIGNALS()


#define GC_RESERVE_BEGIN(_class_,_obj_) _GC_RESERVE_BEGIN_BASE(_class_,_obj_)
#define GC_RESERVE_GET(_class_,_obj_) _GC_RESERVE_GET_BASE(_class_,_class_::static_Kind,_obj_) _class_();
#define GC_RESERVE_GET_VARIADIC(_class_,_obj_,...) _GC_RESERVE_GET_BASE(_class_,_class_::static_Kind,_obj_) _class_(__VA_ARGS__);
#define GC_RESERVE_END(_class_,_obj_) _GC_RESERVE_END_FINALyes_INITyes(_class_,_obj_)
#define GC_RESERVE_END_FINALno_INITno(_class_,_obj_) _GC_RESERVE_END_FINALno_INITno(_class_,_obj_)
#define GC_RESERVE_END_FINALyes_INITno(_class_,_obj_) _GC_RESERVE_END_FINALyes_INITno(_class_,_obj_)

#define GC_RESERVE(_class_,_obj_) GC_RESERVE_BEGIN(_class_,_obj_) GC_RESERVE_GET(_class_,_obj_) GC_RESERVE_END(_class_,_obj_)
#define GC_RESERVE_VARIADIC(_class_,_obj_,...) GC_RESERVE_BEGIN(_class_,_obj_) GC_RESERVE_GET_VARIADIC(_class_,_obj_,__VA_ARGS__) GC_RESERVE_END(_class_,_obj_)


#define GC_COPY_BEGIN(_class_,_obj_) _GC_RESERVE_BEGIN_BASE(_class_,_obj_)
#define GC_COPY_GET(_class_,_obj_,_orig_) GC_RESERVE_GET_VARIADIC(_class_,_obj_,_orig_)
#define GC_COPY_END(_class_,_obj_) _GC_RESERVE_END_FINALyes_INITno(_class_,_obj_)
#define GC_COPY(_class_,_obj_,_orig_) GC_COPY_BEGIN(_class_,_obj_) GC_COPY_GET(_class_,_obj_,_orig_) GC_COPY_END(_class_,_obj_)






/* ------------------------------------------------------------
   ------------------------------------------------------------

   Macros for fixing pointers managed by GC

   ------------------------------------------------------------
*/



#define GC_IGNORE(_ptr_)  {} /* do nothing - it's not a pointer or container of pointers */

/*! Return the block address of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) ((_smartptr_).pbase_ref())
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void*>(dynamic_cast<const void*>(_ptr_)))



#define BASE_TO_OBJ_PTR(_gcptr_) reinterpret_cast<void*>(reinterpret_cast<char*>(_gcptr_)+ALIGN_UP(sizeof(gctools::Header_s)))
#define OBJ_TO_BASE_PTR(_objptr_) reinterpret_cast<void*>(reinterpret_cast<char*>(_objptr_)-ALIGN_UP(sizeof(gctools::Header_s)))



#define CONTAINER_FIX(_cptr_)                                           \
    if (_cptr_) {                                                       \
        mps_addr_t addr = OBJ_TO_BASE_PTR(_cptr_);                      \
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
#error "HANDLE undef USE_TAGGED_PTR_P0"
#endif // USE_TAGGED_PTR_P0



#if 0
// This was for the AMC pool using the px pointer to dynamic_downcast to void*
// This requires a dereferencing of px and the MPS library protects the memory and
// so we have to unshield it
#define MY_MPS_FIX(_smartptr_)						\
    if ( (_smartptr_).pointerp() ) {					\
	if ( MPS_FIX1(GC_SCAN_STATE,reinterpret_cast<void*>((_smartptr_).px_ref())) ) { \
	    Seg _seg;						\
	    bool gotAddr = SegOfAddr(&_seg,_global_arena,(_smartptr_).px_ref());	\
	    if ( !gotAddr ) THROW_HARD_ERROR(BF("Could not get SegOfAddr for address: %p") % (_smartptr_).px_ref() ); \
	    ShieldRaise(_global_arena,_seg);					\
	    void* _obj_addr = dynamic_cast<void*>((_smartptr_).px_ref()); \
	    ShieldCover(gctools::_global_arena,_seg);                   \
	    void* _base = OBJ_TO_BASE_PTR(_obj_addr);			\
	    int _offset = reinterpret_cast<char*>((_smartptr_).px_ref()) - reinterpret_cast<char*>(_base); \
	    DEBUG_MPS_FIX_BEFORE(_base,_obj_addr,reinterpret_cast<void*>((_smartptr_).px_ref()),_offset); \
	    mps_res_t res = MPS_FIX2(GC_SCAN_STATE,&_base);		\
	    if (res != MPS_RES_OK) return res;				\
	    void* _newAddr = reinterpret_cast<void*>(reinterpret_cast<char*>(_base) + _offset); \
	    (_smartptr_)._gcset(_newAddr);				\
	    DEBUG_MPS_FIX_AFTER(_base,reinterpret_cast<void*>((_smartptr_).px_ref())); \
	}								\
    };
#endif
#define MY_MPS_FIX_OLD(_smartptr_)						\
    if ( (_smartptr_).pointerp() ) {					\
	if ( MPS_FIX1(GC_SCAN_STATE,reinterpret_cast<void*>((_smartptr_).px_ref())) ) { \
	    void* _obj_addr = dynamic_cast<void*>((_smartptr_).px_ref()); \
	    void* _base = OBJ_TO_BASE_PTR(_obj_addr);			\
	    int _offset = reinterpret_cast<char*>((_smartptr_).px_ref()) - reinterpret_cast<char*>(_base); \
	    DEBUG_MPS_FIX_BEFORE(_base,_obj_addr,reinterpret_cast<void*>((_smartptr_).px_ref()),_offset); \
	    mps_res_t res = MPS_FIX2(GC_SCAN_STATE,&_base);		\
	    if (res != MPS_RES_OK) return res;				\
	    void* _newAddr = reinterpret_cast<void*>(reinterpret_cast<char*>(_base) + _offset); \
	    (_smartptr_)._gcset(_newAddr);				\
	    DEBUG_MPS_FIX_AFTER(_base,reinterpret_cast<void*>((_smartptr_).px_ref())); \
	}								\
    };
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
    void initialize_kinds();

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
