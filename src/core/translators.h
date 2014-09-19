#ifndef	core_translators_H
#define core_translators_H


//
// Type translators
//
//   From object translators to and from Plain Old Data types
//

#include "predicates.h"
#include "clasp_gmpxx.h"
#include "glue.h"
#include "pointer.h"
#include "str.fwd.h"

namespace translate 
{
#if 0
    template <>
    struct	from_object<long int,std::true_type>
    {
	typedef	long int ExpectedType;
	typedef	long int DeclareType;
	DeclareType _v;
#if 0
        from_object(core::T_sp* oP,std::true_type) : _v((*o)->as<core::Integer_O>()->as<long int>())
        from_object() : _v(0) {};
        void set(core::T_sp o) { this->_v = o->as<core::Integer_O>()->as_LongLongInt();};
	from_object(core::T_sp o)
	{_G();
	    if ( core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>() )
	    {
		this->_v = (long int)(fn->get());
		return;
	    }
	    SIMPLE_ERROR(BF("Add support to convert other types to long int"));
	}
#endif
    };
#endif

    template <>
    struct	from_object<uint,std::true_type>
    {
	uint    _v;
        from_object(T_P o)
        {
            if (o.fixnump()) {
                _v = o.asFixnum();
                return;
            } else if (core::Integer_sp i = o.asOrNull<core::Integer_O>()) {
                _v = i->as_uint();
                return;
            }
	    SIMPLE_ERROR(BF("Add support to convert type: %s to uint") % _rep_(o));
	}
    };


    template <>
    struct	from_object<int,std::true_type>
    {
	typedef	int		DeclareType;
	DeclareType _v;
        from_object(T_P o)
        {
            if (o.fixnump()) {
                _v = o.asFixnum();
                return;
            } else if (core::Integer_sp i = o.asOrNull<core::Integer_O>()) {
                _v = i->as_int();
                return;
            }
	    SIMPLE_ERROR(BF("Add support to convert type: %s to int") % _rep_(o));
	}
    };



#if 0
    template <>
    struct	from_object<core::LongLongInt,std::true_type>
    {
	typedef	core::LongLongInt		ExpectedType;
	typedef	core::LongLongInt		DeclareType;
	DeclareType _v;
        from_object() : _v(0) {};
        void set(core::T_sp o) { this->_v = o.as<core::Integer_O>()->as_LongLongInt();};
	from_object(core::T_sp o)
	{_G();
	    if ( core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>() )
	    {
		this->_v = fn->get();
		return;
	    }
	    SIMPLE_ERROR(BF("Add support to convert other types to LongLongInt"));
	}
    };

#endif


    template <>
    struct	from_object<unsigned long long,std::true_type>
    {
	typedef	unsigned long long		ExpectedType;
	typedef	unsigned long long		DeclareType;
	DeclareType _v;
	from_object(T_P o)
        {
            if ( core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>() )
	    {
                Fixnum f = fn->get();
                ASSERTF(f>=0,BF("Cannot convert %s to unsigned long long") % _rep_(o));
		this->_v = f;
		return;
	    } else if (core::Integer_sp io = o.asOrNull<core::Integer_O>() )
            {
                core::LongLongInt lli = io->as_LongLongInt();
                ASSERT(lli >= 0);
                this->_v = lli;
                return;
            }
	    SIMPLE_ERROR(BF("Add support to convert other types to unsigned long long"));
        }
    };


#if 1

    template <>
    struct	from_object<unsigned long,std::true_type>
    {
	typedef	unsigned long ExpectedType;
	typedef	unsigned long DeclareType;
	DeclareType _v;
	inline void set(core::T_sp o)
	{_G();
	    if ( core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>() )
	    {
		this->_v = fn->get();
		return;
	    }
	    SIMPLE_ERROR(BF("Add support to convert other types to unsigned long long"));
	}
        from_object() : _v(0) {};
        from_object(core::T_sp o) { this->set(o);};
    };

#endif


    template <>
    struct	from_object<double,std::true_type>
    {
	typedef	double	ExpectedType;
	typedef	double	DeclareType;
	DeclareType _v;
        from_object(T_P o)
        {
	    if ( core::Number_sp vv = o.asOrNull<core::Number_O>() )
	    {
		this->_v = vv->as_double();
		return;
	    }
	    SIMPLE_ERROR(BF("Add support to convert other types to double"));
	}
    };

    template <>
    struct	from_object<bool,std::true_type>
    {
	typedef	bool	ExpectedType;
	typedef	bool	DeclareType;
	DeclareType _v;
        from_object(T_P o) : _v(!o.nilp()) {};
    };

    template <>
    struct	from_object<bool*,std::false_type>
    {
	typedef	bool*	DeclareType;
        bool            _val;
	DeclareType     _v;
        from_object(T_P o) : _val(false), _v(&_val) {};
    };



    template <>
    struct to_object<bool*,translate::dont_adopt_pointer>
    {
	typedef	bool*	GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
            if (*v) { return core::lisp_true(); };
            return _Nil<core::T_O>();
        }
    };

    template <>
    struct to_object<bool*&,translate::dont_adopt_pointer>
    {
	typedef	bool*	GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
            if (*v) { return core::lisp_true(); };
            return _Nil<core::T_O>();
        }
    };



    template <>
	struct	to_object<void*>
    {
	typedef	void*	GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
            return core::Pointer_O::create(v);
	}
    };


    template <>
	struct	to_object<bool>
    {
	typedef	bool 		GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    if ( v ) return core::lisp_true();
	    return _Nil<core::T_O>();
	}
    };


    template <>
	struct	to_object<double>
    {
	typedef	double GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::DoubleFloat_sp oi = core::DoubleFloat_O::create(v);
	    return(oi);
	}
    };


    template <>
	struct	to_object<uint>
    {
	typedef	uint GivenType;
	static core::T_sp convert(uint v)
	{_G();
	    core::Integer_sp oi = core::Integer_O::create(v);
	    return oi;
	}
    };



    template <>
	struct	to_object<const unsigned int>
    {
	typedef	const unsigned int GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::Integer_sp oi = core::Integer_O::create((uint)v);
	    return oi;
	}
    };



    template <>
	struct	to_object<long unsigned int>
    {
	typedef	long unsigned int GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::Integer_sp oi = core::Integer_O::create(mpz_class(v));
	    return oi;
	}
    };

#if __SIZEOF_LONG_LONG__ <= 8
    template <>
	struct	to_object<unsigned long long>
    {
	typedef	unsigned long long GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	  core::Integer_sp oi = core::Integer_O::create((uint64_t)v);
	    return oi;
	}
    };
#endif

    template <>
	struct	to_object<mpz_class>
    {
	typedef	mpz_class GivenType;
	static core::T_sp convert(const GivenType& v)
	{_G();
	    core::Integer_sp oi = core::Integer_O::create(v);
	    return oi;
	}
    };


    template <>
	struct	to_object<const mpz_class&>
    {
	typedef	const mpz_class& 	GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::Integer_sp oi = core::Integer_O::create(v);
	    return oi;
	}
    };


    template <>
	struct	to_object<long int>
    {
	typedef	long int GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::Integer_sp oi = core::Integer_O::create(mpz_class(v));
	    return oi;
	}
    };



    template <>
	struct	to_object<int>
    {
	typedef	int GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::Fixnum_sp oi = core::Fixnum_O::create(v);
	    return oi;
	}
    };





//  String translators





    template <>
    struct	from_object<const string&,std::true_type>
    {
	typedef	string			DeclareType;
	DeclareType _v;
        from_object(T_P o) : _v(str_get(o)) {};
    };



    template <>
    struct	from_object<string,std::true_type>
    {
	typedef	string		DeclareType;
	DeclareType _v;
        from_object(T_P o) : _v(str_get(o)) {};
    };


    template <>
    struct	from_object<string&,std::true_type>
    {
	typedef	string		DeclareType;
	DeclareType _v;
        from_object(T_P o) : _v(str_get(o)) {};
    };

    template <>
    struct	from_object<string&,std::false_type>
    {
	typedef	string		DeclareType;
	DeclareType _v;
        from_object(T_P o) : _v("") {};
    };





    template <>
    struct	to_object<string,translate::adopt_pointer>
    {
	typedef	string 		GivenType;
	static core::T_sp convert(const string& v)
	{_G();
	    core::T_sp oi = core::str_create(v);
	    return oi;
	}
    };

    template <>
    struct	to_object<string,translate::dont_adopt_pointer>
    {
	typedef	string 		GivenType;
	static core::T_sp convert(const string& v)
	{_G();
	    core::T_sp oi = core::str_create(v);
	    return oi;
	}
    };

#if 0
    template <>
	struct	to_object<const string>
    {
	typedef	core::Str_sp		ExpectedType;
	typedef	core::Str_sp		DeclareType;
	static core::T_sp convert(const string& v)
	{_G();
	    core::T_sp oi = core::str_create(v);
	    return Values(oi);
	}
    };
#endif


    template <>
	struct	to_object<const std::string&>
    {
	typedef	const std::string& GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::T_sp oi = core::str_create(v);
	    return oi;
	}
    };

    template <>
	struct	to_object<std::string&>
    {
	typedef	std::string& GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::T_sp oi = core::str_create(v);
	    return oi;
	}
    };



    template <>
	struct	to_object<const char*>
    {
	typedef	const char* GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    core::T_sp oi = core::str_create(v);
	    return oi;
	}
    };









};




#endif

