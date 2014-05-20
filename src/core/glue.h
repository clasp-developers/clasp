#ifndef glue_H
#define	glue_H




template <class to_class, class from_class>
inline mem::smart_ptr<to_class> safe_downcast(const mem::smart_ptr<from_class>& c)
{
    if ( c.nilp() ) return _Nil<to_class>();
    mem::smart_ptr<to_class> dc = mem::dynamic_pointer_cast<to_class>(c);
    if ( dc.pointerp() ) return dc;
    lisp_throwUnexpectedType(c,to_class::static_classSymbol());
    return _Nil<to_class>();
}

template <>
inline mem::smart_ptr<core::T_O> safe_downcast(const mem::smart_ptr<core::T_O>& c)
{
    return c;
}

template <>
inline mem::smart_ptr<core::Cons_O> safe_downcast(const mem::smart_ptr<core::Cons_O>& c)
{
    return c;
}


template <>
inline mem::smart_ptr<core::Symbol_O> safe_downcast(const mem::smart_ptr<core::Symbol_O>& c)
{
    return c;
}





/*! Downcast multiple_values<
 */


template <class to_class, class from_class>
inline mem::multiple_values<to_class> safe_downcast(const mem::multiple_values<from_class>& c)
{
    if ( c.nilp() ) return mem::multiple_values<to_class>(_Nil<to_class>(),c.number_of_values());
    mem::multiple_values<to_class> dc = mem::dynamic_pointer_cast<to_class>(c);
    if ( dc.pointerp() ) return dc;
    lisp_throwUnexpectedType(c,to_class::static_classSymbol());
    // dummy return */
    return mem::multiple_values<to_class>(_Nil<to_class>(),1);
}

template <>
inline mem::multiple_values<core::T_O> safe_downcast(const mem::multiple_values<core::T_O>& c)
{
    return c;
}

template <>
inline mem::multiple_values<core::Cons_O> safe_downcast(const mem::multiple_values<core::Cons_O>& c)
{
    return c;
}


template <>
inline mem::multiple_values<core::Symbol_O> safe_downcast(const mem::multiple_values<core::Symbol_O>& c)
{
    return c;
}


#define T_P const core::T_sp&



namespace translate
{
    /*! The second template argument can be std::true_type if the value passed to the ctor should be used
      to construct the value or std::false_type if an initialization value should be used */
    template <class oClass,typename Doit=std::true_type>
	struct from_object
	{								
#if 0
	    typedef	mem::smart_ptr<oClass>	DeclareType;	
	    DeclareType _v;
	    from_object(T_P o) : _v(o.as<oClass>()) {};
#endif
	};


    
    struct dont_adopt_pointer {};
    struct adopt_pointer {};
    /*! to_object takes a class to convert to an T_sp type and a template parameter
      that specifies if the pointer should be adopted or not adopted */
    template <class oClass, class AdoptPolicy=dont_adopt_pointer>
    struct to_object : public gctools::StackBoundClass
    {								
    };


    template <class T>
    struct to_object<mem::smart_ptr<T> >
    {
        static core::T_sp convert(const mem::smart_ptr<T>& o)
        {
            return o;
        }
    };


};

#define	__FROM_OBJECT_CONVERTER(oClass) 				\
    namespace translate {						\
	template <> struct from_object<mem::smart_ptr<oClass>,std::true_type > \
	{								\
	    typedef	mem::smart_ptr<oClass>	ExpectedType;		\
	    typedef	mem::smart_ptr<oClass>	DeclareType;		\
	    DeclareType _v;						\
	    from_object(T_P o) : _v(o.as<oClass>()) {}          \
	};								\
    };



#define	__TO_OBJECT_CONVERTER(oClass)                                   \
    namespace translate {                                               \
	template <> struct to_object<mem::smart_ptr<oClass> >           \
	{                                                               \
	    typedef	mem::smart_ptr<oClass>	GivenType;              \
	    static core::T_sp convert(GivenType o) 			\
	    {_G();							\
		return o;                                               \
	    }								\
	};								\
    };




#define DECLARE_ENUM_SYMBOL_TRANSLATOR(enumType,psid) \
    namespace translate {						\
	template <>							\
        struct from_object<enumType,std::true_type>                              \
	{								\
	    typedef     enumType ExpectedType;				\
	    typedef     enumType DeclareType;				\
	    DeclareType _v;						\
	    from_object(T_P o) : _v(static_cast<DeclareType>(core::lisp_lookupEnumForSymbol(psid,o.as<core::Symbol_O>()))) {}; \
        }								\
	};								\
	template <>							\
	    struct to_object<enumType>					\
	{								\
	    typedef     enumType GivenType;		\
	    static core::T_sp convert(enumType e)	\
	    {_G();							\
		return core::lisp_lookupSymbolForEnum(psid,(int)(e)); \
	    }								\
	};								\
    };


#define	STREAMIO(classo) \
    std::ostream & operator<<(std::ostream&os, mem::smart_ptr<classo> p)	\
    {			\
	THROW_HARD_ERROR(boost::format("Illegal operator<<"));	\
	return os; \
    }




#define	TRANSLATE(classo)						\
    __FROM_OBJECT_CONVERTER(classo);					\
    __TO_OBJECT_CONVERTER(classo);


//    STREAMIO(classo);

#endif
