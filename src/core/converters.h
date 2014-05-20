#ifndef	converters_H
#define converters_H




// ------------------------------------------------------------
//
//  Type conversion from T_sp to T_sp
//
//
//
template <typename T_return>
struct from_object
{
    typedef	T_return	ExpectedType;
    typedef	T_return	DeclareType;
#if 0
    static ExpectedType convert(core::T_sp o)
    {_G();
	stringstream ss;
	ss << "Called generic from_object for (";
#ifdef __GNUG__
	size_t len;
	int s;
	char* demangled = abi::__cxa_demangle(typeid(T_return).name(),0,&len,&s);
	ss << demangled;
#else
	ss << typeid(T_return).name();
#endif
	ss << ") should not be invoked, define a more specific one";
	SIMPLE_ERROR(BF("%s") % ss.str() );
    }
#endif
};



#endif // converters_H
