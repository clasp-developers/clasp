#pragma once
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

//
// Type translators
//
//   From object translators to and from Plain Old Data types
//

#include <cstdint>
#include <utility>
#include <concepts> // std::integral

#include <clasp/core/predicates.h>
#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/glue.h>
#include <clasp/core/pointer.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bignum.h> // clasp_to_integral
#include <clasp/core/array.fwd.h>

namespace translate {

// FROM_OBJECT TRANSLATORS

template <std::integral I> struct from_object<I> {
  typedef I DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_integral<I>(o)) {};
};

template <std::integral I> struct from_object<const I&> {
  typedef I DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_integral<I>(o)) {};
};

template <> struct from_object<float> {
  typedef float DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_float(gc::As<core::Number_sp>(o))) {};
};

template <> struct from_object<const float&> {
  typedef float DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_float(gc::As<core::Number_sp>(o))) {};
};

template <> struct from_object<double> {
  typedef double DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_double(gc::As<core::Number_sp>(o))) {};
};

template <> struct from_object<const double&> {
  typedef double DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_double(gc::As<core::Number_sp>(o))) {};
};

template <> struct from_object<long double> {
  typedef long double DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_long_double(gc::As<core::Number_sp>(o))) {};
};

template <> struct from_object<const long double&> {
  typedef long double DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::clasp_to_long_double(gc::As<core::Number_sp>(o))) {};
};

template <> struct from_object<bool> {
  typedef bool DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(!o.nilp()) {};
};

template <> struct from_object<const bool&> {
  typedef bool DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(!o.nilp()) {};
};

template <> struct from_object<bool&> {
  typedef bool DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(!o.nilp()) {};
  ~from_object(){/*non trivial*/};
};

template <> struct from_object<core::T_O*> {
  typedef core::T_O* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.raw_()) {};
};

template <> struct from_object<void*> {
  typedef void* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(core::lisp_to_void_ptr(o)) {};
};

template <> struct from_object<bool*> {
  typedef bool* DeclareType;
  DeclareType _v;
  bool _val;
  from_object(T_P o) : _v(&_val), _val(true) {};
};

// TO_OBJECT TRANSLATORS

template <> struct to_object<bool*, translate::dont_adopt_pointer> {
  typedef bool* DeclareType;
  static core::T_sp convert(DeclareType v) {
    if (*v) {
      return core::lisp_true();
    };
    return nil<core::T_O>();
  }
};

template <> struct to_object<bool*&, translate::dont_adopt_pointer> {
  typedef bool* DeclareType;
  static core::T_sp convert(DeclareType v) {
    if (*v) {
      return core::lisp_true();
    };
    return nil<core::T_O>();
  }
};

template <> struct to_object<void*> {
  typedef void* DeclareType;
  static core::T_sp convert(DeclareType v) { return core::lisp_from_void_ptr(v); }
};

template <> struct to_object<bool> {
  typedef bool DeclareType;
  static core::T_sp convert(DeclareType v) { return v ? core::lisp_true() : nil<core::T_O>(); }
};

template <> struct to_object<const bool&> {
  typedef const bool& DeclareType;
  static core::T_sp convert(DeclareType v) { return v ? core::lisp_true() : nil<core::T_O>(); }
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

template <std::integral I> struct to_object<I> {
  typedef I DeclareType;
  static core::T_sp convert(DeclareType v) { return core::Integer_O::create(v); }
};

template <std::integral I> struct to_object<const I&> {
  typedef const I& DeclareType;
  static core::T_sp convert(DeclareType v) { return core::Integer_O::create(v); }
};

template <> struct to_object<float> {
  typedef float DeclareType;
  static core::T_sp convert(DeclareType v) { return core::clasp_make_single_float(v); }
};

template <> struct to_object<const float&> {
  typedef const float& DeclareType;
  static core::T_sp convert(DeclareType v) { return core::clasp_make_single_float(v); }
};

template <> struct to_object<double> {
  typedef double DeclareType;
  static core::T_sp convert(DeclareType v) { return core::clasp_make_double_float(v); }
};

template <> struct to_object<const double&> {
  typedef const double& DeclareType;
  static core::T_sp convert(DeclareType v) { return core::clasp_make_double_float(v); }
};

template <> struct to_object<long double> {
  typedef long double DeclareType;
  static core::T_sp convert(DeclareType v) {
    return core::clasp_make_double_float(static_cast<double>(v));
  }
};

template <> struct to_object<const long double&> {
  typedef const long double& DeclareType;
  static core::T_sp convert(DeclareType v) {
    return core::clasp_make_double_float(static_cast<double>(v));
  }
};

template <> struct to_object<mpz_class> {
  typedef mpz_class DeclareType;
  static core::T_sp convert(const DeclareType& v) { return core::Integer_O::create(v); }
};

template <> struct to_object<const mpz_class&> {
  typedef const mpz_class& DeclareType;
  static core::T_sp convert(DeclareType v) { return core::Integer_O::create(v); }
};

//  String translators

template <> struct from_object<const std::string&> {
  typedef std::string DeclareType;
  DeclareType _v;
  from_object(T_P o) : _v(string_get_std_string(o)){};
};

template <> struct from_object<std::string> {
  typedef std::string DeclareType;
  DeclareType _v;
  from_object(T_P o) : _v(string_get_std_string(o)){};
};

template <> struct from_object<std::string&> {
  typedef std::string DeclareType;
  DeclareType _v;
  from_object(T_P o) : _v(string_get_std_string(o)){};
  ~from_object(){/*non trivial*/};
};

template <> struct to_object<std::string, translate::adopt_pointer> {
  static core::T_sp convert(const std::string& v) {
    core::T_sp oi = core::str_create(v);
    return (oi);
  }
};

template <> struct to_object<std::string, translate::dont_adopt_pointer> {
  typedef std::string& DeclareType;
  static core::T_sp convert(DeclareType& v) {
    core::T_sp oi = core::str_create(v);
    return (oi);
  }
};

template <> struct to_object<const std::string&> {
  typedef const std::string& DeclareType;
  static core::T_sp convert(DeclareType v) {
    core::T_sp oi = core::str_create(v);
    return (oi);
  }
};

template <> struct to_object<std::string&> {
  typedef std::string& DeclareType;
  static core::T_sp convert(DeclareType v) {
    core::T_sp oi = core::str_create(v);
    return (oi);
  }
};

template <> struct to_object<const char*> {
  typedef const char* DeclareType;
  static core::T_sp convert(DeclareType v) {
    core::T_sp oi = core::str_create(v);
    return (oi);
  }
};
}; // namespace translate

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

template <typename TX, typename TY> struct from_object<std::pair<const TX&, gctools::smart_ptr<TY>>> {
  typedef std::pair<const TX&, gctools::smart_ptr<TY>> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(from_object<const TX&>(core::oCar(o))._v, core::oCdr(o)){};
};

template <typename TX, typename TY> struct to_object<std::pair<const TX&, gctools::smart_ptr<TY>>> {
  typedef std::pair<const TX&, gctools::smart_ptr<TY>> DeclareType;
  static core::T_sp convert(DeclareType v) {
    core::T_sp oi = core::Cons_O::create(to_object<const TX&>::convert(v.first), v.second);
    return (oi);
  }
};

}; // namespace translate
