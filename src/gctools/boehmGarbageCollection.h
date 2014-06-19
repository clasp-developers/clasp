#ifndef _clasp_boehmGarbageCollection_H
#define _clasp_boehmGarbageCollection_H










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


    class GCObject {
    public:
	GCObject& operator=(const GCObject&) { return *this; };
    };


    typedef enum { KIND_null } GCKindEnum; // minimally define this GCKind



    typedef enum {  BoehmClassKind, BoehmLispKind, BoehmContainerKind, BoehmStringKind } BoehmKind;

#ifdef USE_BOEHM_MEMORY_MARKER
    extern int globalBoehmMarker;
#endif
    class Header_s {
    public:
        Header_s(const char* name, BoehmKind k) : ValidStamp(0xDEADBEEF)
                              , TypeidName(name), Kind(k)
#ifdef USE_BOEHM_MEMORY_MARKER
                              , Marker(globalBoehmMarker)
#endif
        {};
    private:
        uintptr_t       ValidStamp;
        const char*     TypeidName;
        BoehmKind       Kind;
#ifdef USE_BOEHM_MEMORY_MARKER
        int             Marker;
#endif
    public:
        bool isValid() const { return this->ValidStamp == 0xDEADBEEF; };
        const char* name() const { return this->TypeidName; };
        BoehmKind kind() const { return this->Kind; };
        bool markerMatches(int m) const {
#ifdef USE_BOEHM_MEMORY_MARKER
            if ( m ) { return this->Marker==m;} else return true;
#else
            return true;
#endif
        }
        static size_t HeaderSize() { return sizeof(Header_s);};
    };

    class TemplatedHeader_s : public Header_s
    {
    public:
        TemplatedHeader_s(const char* name, BoehmKind k) : Header_s(name,k) {};
    };

    constexpr size_t Alignment() { return AlignmentT<Header_s>(); };
    constexpr size_t AlignUp(size_t size) { return AlignUpT<Header_s>(size);};


    template <class T> inline size_t sizeof_with_header() { return AlignUp(sizeof(T))+AlignUp(sizeof(Header_s));};
    template <class T> inline size_t sizeof_with_templated_header() { return AlignUp(sizeof(T))+AlignUp(sizeof(TemplatedHeader_s));};



#if 0
    template <typename T>
    const char* baseObjectName(T obj)
    {
        gctools::base_ptr base(obj);
        if ( base.pointerp() ) {
            const char* name = reinterpret_cast<const char*>(base.px_ref());
            return name;
        }
        if (base.nilp()) {
            return "NIL";
        };
        if (base.unboundp()) {
            return "UNBOUND";
        }
        if (base.deletedp()) {
            return "DELETED";
        }
        return "UNKNOWN-object";
    }
#endif




};




namespace gctools {

    inline void* ClientPtrToBasePtr(void* mostDerived)
    {
        void* ptr = reinterpret_cast<char*>(mostDerived) - AlignUp(sizeof(Header_s));
        return ptr;
    }

    template <typename T>
    inline T* BasePtrToMostDerivedPtr(void* base)
    {
        T* ptr = reinterpret_cast<T*>(reinterpret_cast<char*>(base) + AlignUp(sizeof(Header_s)));
        return ptr;
    }

};



namespace gctools
{
    /*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
    int initializeMemoryManagement( MainFunctionType startup, int argc, char* argv[], void* dummy );

};





#endif // _clasp_boehmGarbageCollection_H
