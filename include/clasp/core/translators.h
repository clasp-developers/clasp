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

#include <clasp/core/predicates.h>
#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/glue.h>
#include <clasp/core/pointer.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.fwd.h>

namespace translate {

  // TYPE ERRORS

  core::Fixnum not_fixnum_error( core::T_sp o )
  {
    TYPE_ERROR( o, cl::_sym_fixnum );
  }

  // FROM_OBJECT TRANSLATORS

  template <>
  struct from_object< short, std::true_type >
  {
    typedef short ExpectedType;
    typedef short DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  };

  template <>
  struct from_object< unsigned short, std::true_type >
  {
    typedef unsigned short ExpectedType;
    typedef unsigned short DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  };

  template <>
  struct from_object< int, std::true_type >
  {
    typedef int ExpectedType;
    typedef int DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  };

  template <>
  struct from_object< unsigned int, std::true_type >
  {
    typedef unsigned int ExpectedType;
    typedef unsigned int DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  };

  template <>
  struct from_object< long, std::true_type >
  {
    typedef int ExpectedType;
    typedef int DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  };

  template <>
  struct from_object< unsigned long, std::true_type >
  {
    typedef unsigned long ExpectedType;
    typedef unsigned long DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : not_fixnum_error(o) ) {};
  };

  template <>
  struct from_object< long long, std::true_type >
  {
    typedef int ExpectedType;
    typedef int DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : clasp_to_longlong( o ) ) {};
  };

  template <>
  struct from_object< unsigned long long, std::true_type >
  {
    typedef unsigned long long ExpectedType;
    typedef unsigned long long DeclareType;

    DeclareType _v;
    from_object() : _v( o.fixnump() ? o.unsafe_fixnum() : clasp_to_ulonglong( o ) ) {};
  };








template <>
struct from_object<uint, std::true_type> {
  uint _v;
 from_object(core::T_sp o) : _v(core::clasp_to_uint(gc::As<core::Integer_sp>(o))){};
 };

 template <>
   struct from_object<core::T_O*, std::true_type> {
   typedef core::T_O* DeclareType;
   DeclareType _v;
 from_object(core::T_sp o) : _v(o.raw_()) {};
 };


 template <>
   struct from_object<int, std::true_type> {
   typedef int DeclareType;
   DeclareType _v;
 from_object(core::T_sp o) : _v(core::clasp_to_int(gc::As<core::Integer_sp>(o))){};
 };


 template <>
   struct from_object<unsigned long, std::true_type> {
   typedef unsigned long ExpectedType;
   typedef unsigned long DeclareType;
   DeclareType _v;
   inline void set(core::T_sp o) {
     if (o.fixnump()) {
       this->_v = (unsigned long)o.unsafe_fixnum();
       return;
     }
     TYPE_ERROR(o,cl::_sym_fixnum);
   }
 from_object() : _v(0){};
   from_object(core::T_sp o) { this->set(o); };
 };

 template <>
   struct from_object<double, std::true_type> {
   typedef double ExpectedType;
   typedef double DeclareType;
   DeclareType _v;
 from_object(core::T_sp o) : _v(core::clasp_to_double(gc::As<core::Number_sp>(o))){};
 };

 template <>
   struct from_object<bool, std::true_type> {
   typedef bool ExpectedType;
   typedef bool DeclareType;
   DeclareType _v;
 from_object(T_P o) : _v(!o.nilp()){};
 };

 template <>
   struct from_object<unsigned char, std::true_type> {
   typedef unsigned char ExpectedType;
   typedef unsigned char DeclareType;
   DeclareType _v;
 from_object(core::T_sp o) : _v(o.unsafe_character()){};
 };

 template <>
   struct from_object<unsigned char, std::false_type> {
   typedef unsigned char ExpectedType;
   typedef unsigned char DeclareType;
   DeclareType _v;
 from_object(core::T_sp o) : _v(o.unsafe_character()){};
 };

 template <>
   struct from_object<bool *, std::false_type> {
   typedef bool *DeclareType;
   bool _val;
   DeclareType _v;
 from_object(T_P o) : _val(false), _v(&_val){};
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
   struct to_object<unsigned char> {
   typedef unsigned char GivenType;
   static core::T_sp convert(GivenType v) {
     return core::clasp_make_character(v);
   }
 };

 template <>
   struct to_object<double> {
   typedef double GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     core::DoubleFloat_sp oi = core::DoubleFloat_O::create(v);
     return (oi);
   }
 };

 template <>
   struct to_object<uint> {
   typedef uint GivenType;
   static core::T_sp convert(uint v) {
     core::Integer_sp oi = core::Integer_O::create((gc::Fixnum)v);
     return oi;
   }
 };

 template <>
   struct to_object<const unsigned int> {
   typedef const unsigned int GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     core::Integer_sp oi = core::Integer_O::create((gc::Fixnum)v);
     return oi;
   }
 };

 template <>
   struct to_object<long unsigned int> {
   typedef long unsigned int GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     core::Integer_sp oi = core::Integer_O::create(mpz_class(v));
     return oi;
   }
 };

#if __SIZEOF_LONG_LONG__ <= 8
 template <>
   struct to_object<unsigned long long> {
   typedef unsigned long long GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     mpz_class z = (unsigned long int)v;
     core::Integer_sp oi = core::Integer_O::create(z);
     return oi;
   }
 };
#endif

 template <>
   struct to_object<mpz_class> {
   typedef mpz_class GivenType;
   static core::T_sp convert(const GivenType &v) {
     _G();
     core::Integer_sp oi = core::Integer_O::create(v);
     return oi;
   }
 };

 template <>
   struct to_object<const mpz_class &> {
   typedef const mpz_class &GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     core::Integer_sp oi = core::Integer_O::create(v);
     return oi;
   }
 };

 template <>
   struct to_object<long int> {
   typedef long int GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     core::Integer_sp oi = core::Integer_O::create(mpz_class(v));
     return oi;
   }
 };

 template <>
   struct to_object<int> {
   typedef int GivenType;
   static core::T_sp convert(GivenType v) {
     _G();
     core::Fixnum_sp oi = core::make_fixnum(v);
     return oi;
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
   struct to_object<ushort> {
   typedef ushort GivenType;
   static core::T_sp convert(GivenType v) {
     core::Fixnum_sp oi = core::make_fixnum(v);
     return oi;
   }
 };

//  String translators

 template <>
   struct from_object<const string &, std::true_type> {
   typedef string DeclareType;
   DeclareType _v;
 from_object(T_P o) : _v(string_get_std_string(o)){};
 };

 template <>
   struct from_object<string, std::true_type> {
   typedef string DeclareType;
   DeclareType _v;
 from_object(T_P o) : _v(string_get_std_string(o)){};
 };

 template <>
   struct from_object<string &, std::true_type> {
   typedef string DeclareType;
   DeclareType _v;
 from_object(T_P o) : _v(string_get_std_string(o)){};
 };

 template <>
   struct from_object<string &, std::false_type> {
   typedef string DeclareType;
   DeclareType _v;
 from_object(T_P o) : _v(""){};
 };

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
