#ifndef clbind_wrappers_H
#define clbind_wrappers_H

#include "core/foundation.h"
#include "core/wrappedPointer.h"
#include <core/instance.h>
#include <clbind/adapter.fwd.h>
#include "clbind/derivable.h"
#include <clbind/inheritance.h>

namespace clbind
{





    template <class OT,class WT>
    gctools::smart_ptr<OT> RP_Create_wrapper()
    {_G();
        GC_ALLOCATE(OT,wrapper);
	return wrapper;
    }

    template<typename T>
    struct no_deleter {
        static void deleter(T* p) {};
    };

    template<typename T>
    struct new_deleter {
        static void deleter(T* p) {
            delete p;
        };
    };

    template <typename T, typename = void>
    struct maybe_release {
        static void call(T& obj) {
//            printf("%s:%d - no release to call\n", __FILE__, __LINE__);
        }
    };


    template <typename T>
    struct maybe_release<T,decltype(std::declval<T>().release(), void(0))>
    {
        static void call(T& obj) {
//            printf("%s:%d - calling release\n", __FILE__, __LINE__);
            obj.release();
        }
    };


    struct is_deletable_impl {
        template<class T, class U = decltype(delete std::declval<T>())> static std::true_type test(int);
        template<class> static std::false_type test(...);
    };

    template<class T> struct is_deletable : decltype(is_deletable_impl::test<T>(0)) {};

    template <typename OT, bool Deletable>
    struct maybe_delete {
        static void doit(OT* ptr) { delete ptr;};
    };

    template <typename OT>
    struct maybe_delete<OT,false> {
        static void doit(OT* ptr) {};
    };




    /*! Wrappers wrap external pointers - 
      The wrapper does not own the pointer unless the HolderType is a std::unique_ptr or some other
      smart_ptr type */
    template <class OT, class HolderType = OT* >
    class Wrapper : public core::WrappedPointer_O /*, public gctools::GC_MergeKinds*/ {
    public:
        typedef Wrapper<OT,HolderType>  WrapperType;
	typedef	OT	                ExternalType;
    public: // Do NOT declare any smart_ptr's or weak_smart_ptr's here!!!!
	HolderType 	externalPtr_gc_ignore;
        OT*             nakedPtr_gc_ignore; // weak pointer
        class_id        _classId;
    public:
        Wrapper(OT* naked, class_id cid) : externalPtr_gc_ignore(naked), nakedPtr_gc_ignore(naked), _classId(cid) {
//            printf("\n%s:%d - ctor for Wrapper@%p HolderType=%s OT*=%p adapter@%p cid=%lu  symbol=%s\n", __FILE__, __LINE__, this, typeid(HolderType).name(),this->nakedPtr,clbind::support_adapterAddress<ExternalType>(this->nakedPtr), cid, _rep_(reg::globalClassIdToClassSymbol[cid]).c_str() );
        };


        void* mostDerivedPointer() const { return (void*)(this->nakedPtr_gc_ignore);};

        virtual class_id classId() const { return this->_classId;};

        /*! Release the pointer - invalidate the wrapper and return the pointer */
        virtual void pointerDelete() {
            if ( this->nakedPtr_gc_ignore != NULL ) {
                maybe_delete<OT,is_deletable<OT>::value>::doit(this->nakedPtr_gc_ignore);
                this->nakedPtr_gc_ignore = NULL;
                maybe_release<HolderType>::call(this->externalPtr_gc_ignore);
            }
        }


	static gctools::smart_ptr<WrapperType> create(OT* naked,class_id classId) {
            GC_ALLOCATE_VARIADIC(WrapperType,obj,naked,classId);
            core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
            obj->setInstanceClassUsingSymbol(classSymbol);
	    return obj;
	}

	static gctools::smart_ptr<WrapperType> create(const OT& val,class_id classId) {
            OT* naked = new OT(val);
            GC_ALLOCATE_VARIADIC(WrapperType,obj,naked,classId);
            core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
            obj->setInstanceClassUsingSymbol(classSymbol);
	    return obj;
	}

    public:
        bool validp() const { return this->nakedPtr_gc_ignore!=NULL; };
        void throwIfInvalid() const {
            if (!this->validp()) {
                SIMPLE_ERROR(BF("The wrapper is invalid"));
            }
        };

        /*! Release the pointer - invalidate the wrapper and return the pointer */
        virtual void* pointerRelease() {
            if ( this->nakedPtr_gc_ignore != NULL ) {
                void* ptr = const_cast<typename std::remove_const<OT>::type*>(this->nakedPtr_gc_ignore);
                this->nakedPtr_gc_ignore = NULL;
                maybe_release<HolderType>::call(this->externalPtr_gc_ignore);
                return ptr;
            }
            return NULL;
        }



        void initializeSlots(int numberOfSlots)
        {
            this->throwIfInvalid();
            clbind::support_initializeSlots<ExternalType>(numberOfSlots,this->nakedPtr_gc_ignore);
        }


        core::T_sp instanceSigSet()
        {
            this->throwIfInvalid();
            return clbind::support_instanceSigSet<ExternalType>(this->nakedPtr_gc_ignore);
        }

        core::T_sp instanceSig() const
        {
            this->throwIfInvalid();
            return clbind::support_instanceSig<ExternalType>(this->nakedPtr_gc_ignore);
        }


        core::T_sp instanceRef(int idx) const
        {
            this->throwIfInvalid();
            return clbind::support_instanceRef<ExternalType>(idx,this->nakedPtr_gc_ignore);
        }

        core::T_sp instanceSet(int idx, core::T_sp val)
        {
            this->throwIfInvalid();
            return clbind::support_instanceSet<ExternalType>(idx,val,this->nakedPtr_gc_ignore);
        }



        virtual void* castTo(class_id cid) const {
            this->throwIfInvalid();
            std::pair<void*,int> res = globalCastGraph->cast(const_cast<typename std::remove_const<OT>::type*>(this->nakedPtr_gc_ignore)     // ptr
                                                             , this->_classId    // src
                                                             , cid              // target
                                                             , this->_classId    // dynamic_id
                                                             , this->nakedPtr_gc_ignore // dynamic_ptr
                );
            return res.first;
        }
            
	explicit Wrapper() {
//            printf("\n%s:%d - explicit ctor for Wrapper@%p\n", __FILE__, __LINE__, this );
        };
	virtual ~Wrapper() {
//            TRACE();
//            printf("\n%s:%d - dtor for Wrapper@%p HolderType=%s OT*=%p adapter@%p cid=%lu  symbol=%s\n", __FILE__, __LINE__, this, typeid(HolderType).name(),this->nakedPtr_gc_ignore,clbind::support_adapterAddress<ExternalType>(this->nakedPtr_gc_ignore), this->classId, _rep_(reg::globalClassIdToClassSymbol[this->classId]).c_str() );
            

        };
    };




};


namespace translate {


    template <typename T>
    struct debug_deleter {
        void operator()(T* p) {
            printf("%s:%d:%s Deleting object of type %s\n", __FILE__,__LINE__,__FUNCTION__,typeid(T).name());
            delete p;
        };
    };



    /*! Translate pointers that I adopt */
    template <typename T>
    class to_object<const T*&,translate::adopt_pointer> {
    public:
        typedef std::unique_ptr<const T/*,debug_deleter<T>*/ >       HolderType;
        typedef clbind::Wrapper<const T,HolderType>                  WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(const T* ptr) {
            if ( ptr == NULL ) {
                return _Nil<core::T_O>();
            }
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    /*! Translate pointers that I dont adopt */
    template <typename T>
    class to_object<const T*&,translate::dont_adopt_pointer> {
    public:
        typedef clbind::Wrapper<const T>                  WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(const T* ptr) {
            if ( ptr == NULL ) {
                return _Nil<core::T_O>();
            }
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };



    /*! Translate pointers that I adopt */
    template <typename T>
    class to_object<T*,translate::adopt_pointer> {
    public:
        typedef std::unique_ptr<T/*,debug_deleter<T>*/ >       HolderType;
        typedef clbind::Wrapper<T,HolderType>                  WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(T* ptr) {
            if ( ptr == NULL ) {
                return _Nil<core::T_O>();
            }
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };


    template <typename T>
    class to_object<T*,translate::dont_adopt_pointer> {
    public:
        typedef clbind::Wrapper<T,T*>      WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(T* ptr) {
            if ( ptr == NULL ) {
                return _Nil<core::T_O>();
            }
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    /*! Translate pointers that I adopt */
    template <typename T>
    class to_object<T* const,translate::adopt_pointer> {
    public:
        typedef std::unique_ptr<T/*,debug_deleter<T>*/ >       HolderType;
        typedef clbind::Wrapper<T,HolderType>                  WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(T* const ptr) {
            if ( ptr == NULL ) {
                return _Nil<core::T_O>();
            }
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };


    template <typename T>
    class to_object<T* const,translate::dont_adopt_pointer> {
    public:
        typedef clbind::Wrapper<T,T* const>      WrapperType;
        static core::T_sp convert(T* const ptr) {
            if ( ptr == NULL ) {
                return _Nil<core::T_O>();
            }
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    template <typename T>
    class to_object<T&,translate::dont_adopt_pointer> {
    public:
        typedef clbind::Wrapper<T,T*>      WrapperType;
        static core::T_sp convert(T& val) {
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(&val,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    /*! Translate return by value into wrapped pointers that I adopt */
    template <typename T>
    class to_object<T,translate::adopt_pointer> {
    public:
        typedef std::unique_ptr<T>       HolderType;
        typedef clbind::Wrapper<T,HolderType>                  WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(const T& val) {
            T* ptr = new T(val);
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    /*! Translate return by value into wrapped pointers that I adopt
      Ignore the translate::dont_adopt_pointer policy - we always adopt return by value values*/
    template <typename T>
    class to_object<T,translate::dont_adopt_pointer> {
    public:
        typedef std::unique_ptr<T>       HolderType;
        typedef clbind::Wrapper<T,HolderType>                  WrapperType;     
        typedef WrapperType GivenType;
        static core::T_sp convert(const T& val) {
            T* ptr = new T(val);
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    /*! Translate return by value into wrapped pointers that I adopt */
    template <typename T>
    class to_object<const T,translate::adopt_pointer> {
    public:
        typedef std::unique_ptr<T>       HolderType;
        typedef clbind::Wrapper<T,HolderType>                  WrapperType;
        typedef WrapperType GivenType;
        static core::T_sp convert(const T& val) {
            T* ptr = new T(val);
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };

    /*! Translate return by value into wrapped pointers that I adopt
      Ignore the translate::dont_adopt_pointer policy - we always adopt return by value values*/
    template <typename T>
    class to_object<const T,translate::dont_adopt_pointer> {
    public:
        typedef std::unique_ptr<T>       HolderType;
        typedef clbind::Wrapper<T,HolderType>                  WrapperType;     
        typedef WrapperType GivenType;
        static core::T_sp convert(const T& val) {
            IMPLEMENT_MEF(BF("This doesn't make sense - copy but don't adopt pointer???"));
            T* ptr = new T(val);
            gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr,reg::registered_class<T>::id);
            return wrapper;
        }
    };






    /*! from_object translators
     *
     *
     *
     *
     */




    template <typename T>
    struct from_object<T*> {
        typedef T* DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) {
            if ( o.nilp() ) {
                this->_v = static_cast<T*>(NULL);
                return;
            } else if ( core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>() ) {
                this->_v = o.as<core::WrappedPointer_O>()->cast<T>();
                return;
            } else if ( core::Pointer_sp pp = o.asOrNull<core::Pointer_O>() ) {
                this->_v = static_cast<T*>(pp->ptr());
                return;
            } else if ( clbind::Derivable<T>* dp = dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) ) {
                this->_v = dp->pointerToAlienWithin();
                return;
            }

#if 1
            clbind::Derivable<T>* dtptr = dynamic_cast<clbind::Derivable<T>*>(o.px_ref());
            printf("%s:%d In from_object<T*>(core::T_sp o)\n", __FILE__, __LINE__ );
            printf("dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) = %p (SHOULD NOT BE NULL!!!)\n", dynamic_cast<clbind::Derivable<T>*>(o.px_ref()));
            printf("o.px_ref() = %p\n", o.px_ref());
            printf("typeid(T*)@%p  typeid(T*).name=%s\n", &typeid(T*),typeid(T*).name());
            printf("typeid(o.px_ref())@%p  typeid(o.px_ref()).name=%s\n", &typeid(o.px_ref()),typeid(o.px_ref()).name());
            printf("typeid(clbind::Derivable<T>*)@%p   typeid(clbind::Derivable<T>*).name() = %s\n", &typeid(clbind::Derivable<T>*), typeid(clbind::Derivable<T>*).name());
            printf("dynamic_cast<void*>(o.px_ref()) = %p\n", dynamic_cast<void*>(o.px_ref()));
            printf("Invoking o.px_ref()->describe(); /* A virtual function */\n");
            o.px_ref()->describe();
#endif
            SIMPLE_ERROR(BF("Could not convert %s of RTTI type %s to %s") % _rep_(o) % typeid(o).name() % typeid(T*).name() );
        }
    };



    template <typename T>
    struct from_object<const T*&> {
        typedef const T* DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) {
            if ( o.nilp() ) {
                this->_v = static_cast<T*>(NULL);
                return;
            } else if ( core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>() ) {
                this->_v = o.as<core::WrappedPointer_O>()->cast<T>();
                return;
            } else if ( core::Pointer_sp pp = o.asOrNull<core::Pointer_O>() ) {
                this->_v = static_cast<T*>(pp->ptr());
                return;
            }
            SIMPLE_ERROR(BF("Could not convert %s of RTTI type %s to %s") % _rep_(o) % typeid(o).name() % typeid(T*&).name() );
        }
    };



    /*! If the argument is a pure-out-value then don't use the passed to initialize _v */
    template <typename T>
    struct from_object<const T*&,std::false_type> {
        typedef const T* DeclareType;
        DeclareType _v;
        from_object(const core::T_sp& o) : _v(NULL) {
        };
    };






    template <typename T>
    struct from_object<const T&> {
        typedef const T& DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) : _v(*(from_object<T*>(o)._v)) {};
    };


    template <typename T>
    struct from_object<T&> {
        typedef T& DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) : _v(*(from_object<T*>(o)._v)) {};
    };


    template <typename T>
    struct from_object<T> {
        typedef T DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) : _v(*(from_object<T*>(o)._v)) {};
    };





};


#endif
