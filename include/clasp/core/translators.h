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

// PATTERN FOR FROM_OBJECT TRANSLATORS:
//
// Fixnum not_fixnum_error(core::T_sp o) {
//     TYPE_ERROR(o,cl::_sym_fixnum);
// }
//
// template <>
// struct from_object<unsigned long, std::true_type> {
//     typedef unsigned long ExpectedType;
//     typedef unsigned long DeclareType;
//
//     DeclareType _v;
//     from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
// };



#ifndef core_translators_H
#define core_translators_H

//
// Type translators
//
//   From object translators to and from Plain Old Data types
//

#include <cstdint>
#include <utility>

#include <clasp/core/predicates.h>
#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/glue.h>
#include <clasp/core/pointer.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.fwd.h>

namespace translate {

  // FROM_OBJECT TRANSLATORS

  // template <>
  // struct from_object< short, std::true_type >
  // {
  //   typedef short DeclareType;

  //   DeclareType _v;
  // from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  // };

  // template <>
  //   struct from_object< unsigned short, std::true_type >
  // {
  //   typedef unsigned short DeclareType;

  //   DeclareType _v;
  // from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  // };

  // template <>
  //   struct from_object< int, std::true_type >
  // {
  //   typedef int DeclareType;

  //   DeclareType _v;
  // from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  // };

  // template <>
  //   struct from_object< unsigned int, std::true_type >
  // {
  //   typedef unsigned int DeclareType;

  //   DeclareType _v;
  // from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  // };

#if defined(_TARGET_OS_DARWIN) || defined(_TARGET_OS_FREEBSD)
  // linux doesn't like these because they clash with int64_t and uint64_t
  template <>
    struct from_object< long, std::true_type >
  {
    typedef int DeclareType;
    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : clasp_to_long(o) ) {};
  };

  template <>
    struct from_object< unsigned long, std::true_type >
  {
    typedef unsigned long DeclareType;
    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : clasp_to_ulong(o)) {};
  };
#endif
  
template <>
  struct from_object< long long, std::true_type >
{
   typedef long long DeclareType;

   DeclareType _v;
 from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::clasp_to_longlong( o ) ) {};
 };

 template <>
   struct from_object< unsigned long long, std::true_type >
 {
   typedef unsigned long long DeclareType;

    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::clasp_to_ulonglong( o ) ) {};
 };

  template <>
    struct from_object< int8_t, std::true_type >
  {
    typedef int8_t DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::not_fixnum_error( o ) ) {};
  };

  template <>
    struct from_object< uint8_t, std::true_type >
  {
    typedef uint8_t DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::not_fixnum_error( o ) ) {};
  };

  template <>
    struct from_object< int16_t, std::true_type >
  {
    typedef int16_t DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::not_fixnum_error( o ) ) {};
  };

  template <>
    struct from_object< uint16_t, std::true_type >
  {
    typedef uint16_t DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::not_fixnum_error( o ) ) {};
  };

  template <>
    struct from_object< int32_t, std::true_type >
  {
    typedef int32_t DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::not_fixnum_error( o ) ) {};
  };

  template <>
    struct from_object< uint32_t, std::true_type >
  {
    typedef uint32_t DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : core::not_fixnum_error( o ) ) {};
  };

#if defined (_TARGET_OS_LINUX) 
  template <>
    struct from_object< int64_t, std::true_type >
  {
    typedef int64_t DeclareType;

    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : clasp_to_int64( o ) ) {};
  };
#endif
  
#if defined (_TARGET_OS_LINUX) 
  template <>
    struct from_object< uint64_t, std::true_type >
  {
    typedef uint64_t DeclareType;

    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.fixnump() ? o.unsafe_fixnum() : clasp_to_uint64( o ) ) {};
  };
#endif

  
  template <>
    struct from_object< float, std::true_type >
  {
    typedef float DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( core::clasp_to_float( gc::As<core::Number_sp>(o) ) ) {};
  };

  template <>
    struct from_object< double, std::true_type >
  {
    typedef double DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( core::clasp_to_double( gc::As<core::Number_sp>(o) ) ) {};
  };

  template <>
    struct from_object< long double, std::true_type >
  {
    typedef long double DeclareType;

    DeclareType _v;
    from_object( core::T_sp o ) : _v( core::clasp_to_long_double( gc::As<core::Number_sp>(o) ) ) {};
  };

  template <>
    struct from_object< bool, std::true_type >
  {
    typedef bool DeclareType;

    DeclareType _v;
  from_object( core::T_sp o ) : _v( !o.nilp() ){};
  };

  template <>
    struct from_object< core::T_O *, std::true_type >
  {
    typedef core::T_O * DeclareType;

    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.raw_() ) {};
  };

  template <>
    struct from_object< void *, std::true_type >
  {
    typedef void * DeclareType;
    DeclareType _v;
  from_object( core::T_sp o ) : _v(core::lisp_to_void_ptr(o)) {};
  };

  template <>
    struct from_object< bool *, std::false_type >
  {
    typedef bool *DeclareType;

    DeclareType _v;
    bool _val;
  from_object( T_P o ) : _val( false ), _v( &_val ){};
  };

  template <>
    struct from_object< bool *, std::true_type >
  {
    typedef bool *DeclareType;

    DeclareType _v;
    bool _val;
  from_object( T_P o ) : _val( true ), _v( &_val ){};
  };

  // TO_OBJECT TRANSLATORS

  template <>
    struct to_object< bool *, translate::dont_adopt_pointer>
  {
    typedef bool * DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      if ( *v )
      {
        return core::lisp_true();
      };
      return _Nil<core::T_O>();
    }
  };

  template <>
    struct to_object< bool *&, translate::dont_adopt_pointer>
  {
    typedef bool * DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      if ( *v )
      {
        return core::lisp_true();
      };
      return _Nil<core::T_O>();
    }
  };

  template <>
    struct to_object< void * >
  {
    typedef void * DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      return core::lisp_from_void_ptr(v);
    }
  };

  template <>
    struct to_object< bool >
  {
    typedef bool DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      if ( v )
        return core::lisp_true();
      return _Nil<core::T_O>();
    }
  };

  // THIS FN IS ALREADY DEFINED IN CHARACTER.H
  // template <>
  //   struct to_object< char >
  // {
  //   typedef char DeclareType;
  //   static core::T_sp convert( DeclareType v )
  //   {
  //     return ( core::clasp_make_character( v ) );
  //   }
  // };

  template <>
    struct to_object< unsigned char >
  {
    typedef unsigned char DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      return ( core::clasp_make_fixnum( v ) );
    }
  };

  template <>
    struct to_object< short >
  {
    typedef short DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return ( oi );
    }
  };

  template <>
    struct to_object< unsigned short >
  {
    typedef unsigned short DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return ( oi );
    }
  };

  template <>
    struct to_object< int >
  {
    typedef int DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return ( oi );
    }
  };

  template <>
    struct to_object< unsigned int >
  {
    typedef unsigned int DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< const unsigned int >
  {
    typedef const unsigned int DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< long >
  {
    typedef long DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create( static_cast<gctools::Fixnum>(v) );
      return ( oi );
    }
  };

  template <>
    struct to_object< unsigned long >
  {
    typedef unsigned long DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create( static_cast<gctools::Fixnum>(v) );
      return ( oi );
    }
  };


#if __SIZEOF_LONG_LONG__ <= 8

  template <>
    struct to_object< long long >
  {
    typedef long long DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create( (int64_t)v );
      return ( oi );
    }
  };

  template <>
    struct to_object< unsigned long long >
  {
    typedef unsigned long long DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create( (uint64_t)v );
      return ( oi );
    }
  };

#endif

  template <>
    struct to_object< float >
  {
    typedef double DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::SingleFloat_sp oi = core::clasp_make_single_float( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< double >
  {
    typedef double DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::DoubleFloat_sp oi = core::clasp_make_double_float( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< long double >
  {
    typedef double DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::DoubleFloat_sp oi = core::DoubleFloat_O::create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< mpz_class >
  {
    typedef mpz_class DeclareType;
    static core::T_sp convert( const DeclareType &v )
    {
      core::Integer_sp oi = core::Integer_O::create(v);
      return ( oi );
    }
  };

  template <>
    struct to_object< const mpz_class & >
  {
    typedef const mpz_class & DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::Integer_sp oi = core::Integer_O::create(v);
      return ( oi );
    }
  };

  //  String translators

  template <>
    struct from_object< const std::string &, std::true_type >
  {
    typedef std::string DeclareType;
    DeclareType _v;
  from_object( T_P o ) : _v( string_get_std_string( o ) ){};
  };

  template <>
    struct from_object< std::string, std::true_type >
  {
    typedef std::string DeclareType;
    DeclareType _v;
  from_object( T_P o ) : _v( string_get_std_string( o ) ){};
  };

  template <>
    struct from_object< std::string &, std::true_type >
  {
    typedef std::string DeclareType;
    DeclareType _v;
  from_object( T_P o ) : _v( string_get_std_string( o ) ){};
  };

  template <>
    struct from_object< std::string &, std::false_type >
  {
    typedef std::string DeclareType;
    DeclareType _v;
  from_object( T_P o ) : _v( "" ){};
  };

  template <>
    struct to_object< std::string, translate::adopt_pointer >
  {
    static core::T_sp convert( const std::string &v )
    {
      core::T_sp oi = core::str_create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< std::string, translate::dont_adopt_pointer >
  {
    typedef std::string & DeclareType;
    static core::T_sp convert( DeclareType & v )
    {
      core::T_sp oi = core::str_create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< const std::string & >
  {
    typedef const std::string & DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::T_sp oi = core::str_create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< std::string & >
  {
    typedef std::string & DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::T_sp oi = core::str_create( v );
      return ( oi );
    }
  };

  template <>
    struct to_object< const char * >
  {
    typedef const char * DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::T_sp oi = core::str_create( v );
      return ( oi );
    }
  };
};



namespace translate {

#if 0
  template <typename TX, typename TY>
    struct to_object< std::pair<const TX&, const TY&> 
  {
    typedef std::pair<const TX&, const TY&>
    static core::T_sp convert( DeclareType v )
    {
      core::T_sp oi = core::Cons_O::create(translate::to_object<const TX&>::convert(v.first),
                                           translate::to_object<const TY&>::convert(v.second));
      return ( oi );
    }
  };
#endif



  template <typename TX, typename TY>
    struct from_object< std::pair<const TX&, gctools::smart_ptr<TY> > >
  {
    typedef std::pair<const TX&, gctools::smart_ptr<TY>> DeclareType;
    DeclareType _v;
  from_object( core::T_sp o ) : _v( from_object<const TX&>(core::oCar(o))._v, core::oCdr(o)) {};
  };

    template <typename TX, typename TY>
      struct to_object< std::pair<const TX&, gctools::smart_ptr<TY>>>
  {
    typedef std::pair<const TX&, gctools::smart_ptr<TY>> DeclareType;
    static core::T_sp convert( DeclareType v )
    {
      core::T_sp oi = core::Cons_O::create(to_object<const TX&>::convert(v.first),v.second);
      return ( oi );
    }
  };

    
};
#endif
