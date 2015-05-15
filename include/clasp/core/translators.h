/*
    File: translators.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef	core_translators_H
#define core_translators_H


//
// Type translators
//
//   From object translators to and from Plain Old Data types
//

#include <clasp/core/predicates.h>
#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/glue.h>
#include <clasp/core/pointer.h>
#include <clasp/core/str.fwd.h>

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
    from_object(core::T_sp o) : _v(clasp_to_uint(gc::As<core::Integer_sp>(o))) {};
    };


    template <>
    struct	from_object<int,std::true_type>
    {
	typedef	int		DeclareType;
	DeclareType _v;
    from_object(core::T_sp o) : _v(clasp_to_int(gc::As<core::Integer_sp>(o))) {};
    };

    template <>
	struct	from_object<gc::Fixnum,std::true_type>
    {
	typedef	gc::Fixnum		DeclareType;
	DeclareType _v;
    from_object(core::T_sp o) : _v(clasp_to_fixnum(core::Fixnum_sp(o))) {};
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
    from_object(T_P o) : _v(clasp_to_unsigned_long_long(gc::As<core::Integer_sp>(o))) {}
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
	    if ( o.fixnump() ) // core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>() )
	    {
#ifdef USE_HEAP_FIXNUM
		this->_v = unbox_fixnum(o);
#else
		this->_v = o.unsafe_fixnum();
#endif
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
    from_object(core::T_sp o) : _v(clasp_to_double(gc::As<core::Number_sp>(o))) {};
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
	    core::Fixnum_sp oi = core::make_fixnum(v);
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
