#ifndef clbind_derivable_H
#define clbind_derivable_H

namespace clbind {




    /*! Derivables are template classes that inherit from core::Instance_O and wrap Alien classes */
    template <class Alien >
    class Derivable : public core::Instance_O
                    , public Alien
    {
    public:
        struct metadata_gc_do_not_move {}; // All classes derived from Derivable must be put in the non-moving pool
    public:
// Used by template program to indicate that CommonLisp classes can be derived from this class
        typedef int enable_derivable; 
    public:
        typedef Derivable<Alien>        DerivableType;
	typedef	Alien	                AlienType;
    public:

        template <typename...Args>
	static gctools::smart_ptr<DerivableType> create(Args&&...args) {
            GC_ALLOCATE_VARIADIC(DerivableType,obj,std::forward<Args>(args)...);
	    return obj;
	}
            
	explicit Derivable() {
//            printf("\n%s:%d - explicit ctor for Derivable@%p\n", __FILE__, __LINE__, this );
        };


        Alien* pointerToAlienWithin() { return static_cast<Alien*>(this);}

        bool cxxAdapterClassP() const { return true;};


        void describe()
        {
            printf("#<Derivable>@%p\n", this);
            printf("typeid(this) --> %p\n", &typeid(this));
            printf("dynamic_cast to void* --> %p\n", dynamic_cast<void*>(this));
            printf("dynamic_cast to T_O* -->  %p\n", dynamic_cast<core::T_O*>(this));
            printf("dynamic_cast to Derivable<Alien>* --> %p\n", dynamic_cast<Derivable<Alien>*>(this));

            printf("alien pointer = %p\n", this->pointerToAlienWithin() );
            printf("isgf %d\n", this->_isgf);
            printf("_Class: %s\n", _rep_(this->_Class).c_str());
            for (int i(0); i<this->_Slots.size(); ++i ) {
                printf("_Slots[%d]: %s\n", i, _rep_(this->_Slots[i]).c_str());
            }
        }



    };





};


#define DERIVABLE_TRANSLATE(oclass)                                     \
    TRANSLATE(oclass);                                                  \
    namespace translate {                                               \
        template <> struct from_object<oclass*> {                       \
        typedef oclass* DeclareType;                                    \
        DeclareType _v;                                                 \
        from_object(core::T_sp o) : _v(o.as<oclass>().px_ref()) {};     \
    };};
    

#endif

