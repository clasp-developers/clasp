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
#ifndef core_translators_H
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
#include <clasp/core/numbers.h>
#include <clasp/core/fli.h>

namespace translate {

  // === FROM-OBJECT ===

  // --- POINTER ---

  template <>
    struct from_object< core::T_O*, std::true_type > {

    typedef core::T_O* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( o.raw_() ) {};
  };

  // --- NUMBERS ---

  template <>
    struct from_object< short, std::true_type > {
    typedef short DeclareType;
    DeclareType _v;
    from_object(core::T_sp o) : _v( core::clasp_to_short( o )){};
  };

  template <>
    struct from_object< unsigned short, std::true_type > {
    typedef unsigned short DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_ushort( gc::As<core::Integer_sp>( o ) )){};
  };

  template <>
  struct from_object< int, std::true_type >
  {
    typedef int DeclareType;
    DeclareType _v;
  from_object( core::T_sp o ) : _v( core::clasp_to_int( o ))
    {
      // fprintf( stderr, "*** translate::from_object: _v = %d\n",
      //          _v );
      // fflush( stderr );
    };
  };

  template <>
    struct from_object< unsigned int, std::true_type > {
    typedef unsigned int DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_uint( o ))
    {
    };
  };

  template <>
    struct from_object< gc::Fixnum, std::true_type > {
    typedef gc::Fixnum DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_fixnum(core::Fixnum_sp(o))){};
  };

  template <>
    struct from_object< unsigned long, std::true_type > {
    typedef unsigned long DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_ulong( o )){};
  };

  template <>
    struct from_object< long long, std::true_type > {
    typedef long long DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_longlong( o )){};
  };

  template <>
    struct from_object< unsigned long long, std::true_type > {
    typedef unsigned long long DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_ulonglong( o )){};
  };

  template <>
    struct from_object< int8_t, std::true_type > {
    typedef int8_t DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_int8( o )){};
  };

  template <>
    struct from_object< uint8_t, std::true_type > {
    typedef uint8_t DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_uint8( o )){};
  };

  // template <>
  //   struct from_object< int16_t, std::true_type > {
  //   typedef int16_t DeclareType;
  //   DeclareType _v;
  // from_object(core::T_sp o) : _v( clasp_to_int16( o )){};
  // };

  // template <>
  //   struct from_object< uint16_t, std::true_type > {
  //   typedef uint16_t DeclareType;
  //   DeclareType _v;
  // from_object(core::T_sp o) : _v( clasp_to_uint16( o )){};
  // };

  // template <>
  //   struct from_object< int32_t, std::true_type > {
  //   typedef int32_t DeclareType;
  //   DeclareType _v;
  // from_object(core::T_sp o) : _v( clasp_to_int32( o )){};
  // };

  // template <>
  //   struct from_object< uint32_t, std::true_type > {
  //   typedef uint32_t DeclareType;
  //   DeclareType _v;
  // from_object(core::T_sp o) : _v( clasp_to_uint32( o )){};
  // };

  // template <>
  //   struct from_object< int64_t, std::true_type > {
  //   typedef int64_t DeclareType;
  //   DeclareType _v;
  // from_object(core::T_sp o) : _v( clasp_to_int64( o )){};
  // };

  // template <>
  //   struct from_object< uint64_t, std::true_type > {
  //   typedef uint64_t DeclareType;
  //   DeclareType _v;
  // from_object(core::T_sp o) : _v( clasp_to_uint64( o )){};
  // };

  template <>
    struct from_object<float, std::true_type> {
    typedef float DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_float(gc::As<core::Number_sp>(o))){};
  };

  template <>
    struct from_object<double, std::true_type> {
    typedef double DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_double(gc::As<core::Number_sp>(o))){};
  };

  template <>
    struct from_object<long double, std::true_type> {
    typedef long double DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( core::clasp_to_long_double(gc::As<core::Number_sp>(o))){};
  };

  template <>
    struct from_object<bool, std::true_type> {
    typedef bool DeclareType;
    DeclareType _v;
  from_object(T_P o) : _v(!o.nilp()){};
  };

  template <>
    struct from_object<bool *, std::false_type> {
    typedef bool *DeclareType;
    bool _val;
    DeclareType _v;
  from_object(T_P o) : _val(false), _v(&_val){};
  };

  template <>
    struct from_object<void *, std::true_type> {
    typedef void *DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v( ( gc::As< clasp_ffi::ForeignData_sp> ( o  ))->ptr() ){};
  };

  // === TO-OBJECT ===

  template <>
    struct to_object<bool> {
    typedef bool GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      if (v)
        return core::lisp_true();
      return _Nil<core::T_O>();
    }
  };

  template <>
    struct to_object<bool *, translate::dont_adopt_pointer> {
    typedef bool *GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      if (*v) {
        return core::lisp_true();
      };
      return _Nil<core::T_O>();
    }
  };

  template <>
    struct to_object<bool *&, translate::dont_adopt_pointer> {
    typedef bool *GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      if (*v) {
        return core::lisp_true();
      };
      return _Nil<core::T_O>();
    }
  };

  template <>
    struct to_object<void *> {
    typedef void *GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      return core::Pointer_O::create(v);
    }
  };

  template <>
    struct to_object<short> {
    typedef short GivenType;
    static core::T_sp convert(GivenType v) {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return oi;
    }
  };

  template <>
    struct to_object<unsigned short> {
    typedef unsigned short GivenType;
    static core::T_sp convert(GivenType v) {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return oi;
    }
  };

  template <>
    struct to_object<int> {
    typedef int GivenType;
    static core::T_sp convert(GivenType v) {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return oi;
    }
  };

  template <>
    struct to_object<unsigned int> {
    typedef unsigned int GivenType;
    static core::T_sp convert(GivenType v) {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return oi;
    }
  };

  template <>
    struct to_object<const unsigned int> {
    typedef const unsigned int GivenType;
    static core::T_sp convert(GivenType v) {
      core::Integer_sp oi = core::Integer_O::create((gc::Fixnum)v);
      return oi;
    }
  };

  template <>
    struct to_object<int8_t> {
    typedef int8_t GivenType;
    static core::T_sp convert(GivenType v) {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return oi;
    }
  };

  template <>
    struct to_object<uint8_t> {
    typedef uint8_t GivenType;
    static core::T_sp convert(GivenType v) {
      core::Fixnum_sp oi = core::make_fixnum(v);
      return oi;
    }
  };

  template <>
    struct to_object<long> {
    typedef long GivenType;
    static core::T_sp convert(GivenType v) {
      core::Integer_sp oi = core::Integer_O::create(mpz_class(v));
      return oi;
    }
  };

  template <>
    struct to_object<unsigned long> {
    typedef unsigned long GivenType;
    static core::T_sp convert(GivenType v) {
      core::Integer_sp oi = core::Integer_O::create(mpz_class(v));
      return oi;
    }
  };

  template <>
    struct to_object<long long> {
    typedef long long GivenType;
    static core::T_sp convert(GivenType v) {
      mpz_class z = (unsigned long int)v;
      core::Integer_sp oi = core::Integer_O::create(z);
      return oi;
    }
  };

  template <>
    struct to_object<unsigned long long> {
    typedef unsigned long long GivenType;
    static core::T_sp convert(GivenType v) {
      mpz_class z = (unsigned long int)v;
      core::Integer_sp oi = core::Integer_O::create(z);
      return oi;
    }
  };

  template <>
    struct to_object<mpz_class> {
    typedef mpz_class GivenType;
    static core::T_sp convert(const GivenType &v) {
      core::Integer_sp oi = core::Integer_O::create(v);
      return oi;
    }
  };

  template <>
    struct to_object<const mpz_class &> {
    typedef const mpz_class &GivenType;
    static core::T_sp convert(GivenType v) {
      core::Integer_sp oi = core::Integer_O::create(v);
      return oi;
    }
  };

  template <>
    struct to_object<float> {
    typedef float GivenType;
    static core::T_sp convert(GivenType v) {
      core::SingleFloat_sp oi = core::make_single_float(v);
      return oi;
    }
  };

  template <>
    struct to_object<double> {
    typedef double GivenType;
    static core::T_sp convert(GivenType v) {
      core::DoubleFloat_sp oi = core::DoubleFloat_O::create(v);
      return oi;
    }
  };

  template <>
    struct to_object<long double> {
    typedef long double GivenType;
    static core::T_sp convert(GivenType v) {
      core::LongFloat_sp oi = core::LongFloat_O::create(v);
      return oi;
    }
  };

  // *** STRING TRANSLATION ***

  // --- FROM-OBJECT STRING TRANSLATORS ---

  template <>
    struct from_object<const string &, std::true_type> {
    typedef string DeclareType;
    DeclareType _v;
  from_object(T_P o) : _v(str_get(o)){};
  };

  template <>
    struct from_object<string, std::true_type> {
    typedef string DeclareType;
    DeclareType _v;
  from_object(T_P o) : _v(str_get(o)){};
  };

  template <>
    struct from_object<string &, std::true_type> {
    typedef string DeclareType;
    DeclareType _v;
  from_object(T_P o) : _v(str_get(o)){};
  };

  template <>
    struct from_object<string &, std::false_type> {
    typedef string DeclareType;
    DeclareType _v;
  from_object(T_P o) : _v(""){};
  };

  // --- TO-OBJECT STRING TRANSLATORS ---

  template <>
    struct to_object<string, translate::adopt_pointer> {
    typedef string GivenType;
    static core::T_sp convert(const string &v) {
      _G();
      core::T_sp oi = core::str_create(v);
      return oi;
    }
  };

  template <>
    struct to_object<string, translate::dont_adopt_pointer> {
    typedef string GivenType;
    static core::T_sp convert(const string &v) {
      _G();
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
    struct to_object<const std::string &> {
    typedef const std::string &GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      core::T_sp oi = core::str_create(v);
      return oi;
    }
  };

  template <>
    struct to_object<std::string &> {
    typedef std::string &GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      core::T_sp oi = core::str_create(v);
      return oi;
    }
  };

  template <>
    struct to_object<const char *> {
    typedef const char *GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      core::T_sp oi = core::str_create(v);
      return oi;
    }
  };
};

#endif
