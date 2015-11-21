/*
    File: numbers.h
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
#ifndef _core_numbers_H //[
#define _core_numbers_H

#include <clasp/core/clasp_gmpxx.h>
#include <math.h>
#include <limits.h>
#pragma GCC diagnostic push
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/archive/tmpdir.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/assume_abstract.hpp>
#pragma GCC diagnostic pop

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/bignum.fwd.h>
#include <clasp/core/numbers.fwd.h>
#include <clasp/core/numerics.h>

#define CLASP_PI_D 3.14159265358979323846264338327950288
#define CLASP_PI_L 3.14159265358979323846264338327950288l
#define CLASP_PI2_D 1.57079632679489661923132169163975144
#define CLASP_PI2_L 1.57079632679489661923132169163975144l

namespace cl {
extern core::Symbol_sp _sym_Integer_O; // CL:INTEGER
};

namespace core {
typedef enum { number_Fixnum = 0,
               number_Bignum = 1,
               number_Ratio = 2,
               number_ShortFloat = 3,
               number_SingleFloat = 4,
               number_DoubleFloat = 5,
               number_LongFloat = 6,
               number_Complex = 7,
               number_NUM = 8 } NumberType;

template <typename T>
gc::smart_ptr<T> immediate_fixnum(Fixnum f) {
  return gc::make_tagged_fixnum<core::Fixnum_I>(f);
};
template <typename T>
gc::smart_ptr<T> immediate_single_float(float f) {
  return gc::make_tagged_single_float<core::SingleFloat_I>(f);
};

template <typename FLOAT>
inline FLOAT _log1p(FLOAT x) {
  IMPLEMENT_MEF(BF("Implement specialized log1p for basic float type"));
}

template <>
inline float _log1p<float>(float x) {
  float u = (float)1 + x;
  if (u == 1) {
    return (float)0;
  }
  return (logf(u) * x) / (u - (float)1);
}

template <>
inline double _log1p<double>(double x) {
  double u = (double)1 + x;
  if (u == 1) {
    return (double)0;
  }
  return (log(u) * x) / (u - (double)1);
}

#ifdef CLASP_LONG_FLOAT
template <>
inline LongFloat _log1p<LongFloat>(LongFloat x) {
  LongFloat u = (LongFloat)1 + x;
  if (u == 1) {
    return (LongFloat)0;
  }
  return (logl(u) * x) / (u - (LongFloat)1);
}
#endif

bool clasp_zerop(Number_sp num);
bool clasp_plusp(Real_sp num);
bool clasp_minusp(Real_sp num);
bool clasp_evenp(Integer_sp num);
bool clasp_oddp(Integer_sp num);
Number_sp clasp_abs(Number_sp num);
Number_sp clasp_signum(Number_sp num);
Number_sp clasp_one_plus(Number_sp num);
Number_sp clasp_one_minus(Number_sp num);
Number_sp clasp_negate(Number_sp num);
bool clasp_float_nan_p(Float_sp num);
bool clasp_float_infinity_p(Float_sp num);
NumberType clasp_t_of(Number_sp num);
Integer_sp clasp_shift(Integer_sp num, int bits);
gc::Fixnum clasp_integer_length(Integer_sp x);
mpz_class clasp_to_mpz(Integer_sp x);
cl_index clasp_to_size(Integer_sp x);
uint32_t clasp_to_uint32_t(Integer_sp x);
Fixnum_sp clasp_make_fixnum(gc::Fixnum i);
SingleFloat_sp clasp_make_single_float(float d);
DoubleFloat_sp clasp_make_double_float(double d);
Number_sp clasp_log1_complex_inner(Number_sp r, Number_sp i);
};

namespace core {

typedef double LongFloat;

Number_sp contagen_add(Number_sp na, Number_sp nb);
Number_sp contagen_sub(Number_sp na, Number_sp nb);
Number_sp contagen_mul(Number_sp na, Number_sp nb);
Number_sp contagen_div(Number_sp na, Number_sp nb);
int basic_compare(Number_sp na, Number_sp nb);

SMART(Number);
class Number_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, Number_O, "number");

public:
  static Number_sp create(double val);
  static Number_sp create(gc::Fixnum val);
  //	static Number_sp create(size_t val);
public:
  virtual NumberType number_type_() const { SUBIMP(); };
  //	int number_type_int() const { return (int)(clasp_t_of(this->asSmartPtr()));;};
  //	virtual	string	valueAsString_() const;
  //	virtual Number_sp copy() const { _OF(); SUBCLASS_MUST_IMPLEMENT();}
  //	virtual T_sp deepCopy() const { return this->copy();};
  //virtual T_sp shallowCopy() const { return this->copy();};
  virtual Number_sp signum_() const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  virtual Number_sp reciprocal_() const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  virtual Number_sp abs_() const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  virtual T_sp floor(Number_sp divisor) const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  virtual T_sp ffloor(Number_sp divisor) const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  virtual bool equal(T_sp obj) const;

  virtual Number_sp log1_() const { SUBIMP(); };
  virtual Number_sp log1p_() const;

  virtual Number_sp sqrt_() const { SUBIMP(); };

  /*! Add one to the number */
  virtual Number_sp onePlus_() const { SUBIMP(); };
  /*! Subtrace one from the number */
  virtual Number_sp oneMinus_() const { SUBIMP(); };

  // math routines shared by all numbers
  virtual bool zerop_() const { SUBIMP(); }
  virtual Number_sp negate_() const { SUBIMP(); };

  virtual Number_sp exp_() const { SUBIMP(); };

  virtual bool operator<(T_sp obj) const;
  virtual bool operator<=(T_sp obj) const;
  virtual bool operator>(T_sp obj) const;
  virtual bool operator>=(T_sp obj) const;

  virtual gc::Fixnum as_int_() const { SUBIMP(); }
  virtual uint as_uint_() const { SUBIMP(); }
  virtual Bignum as_mpz_() const { SUBIMP(); }
  virtual LongLongInt as_LongLongInt_() const { SUBIMP(); };
  virtual float as_float_() const { SUBIMP(); };
  virtual double as_double_() const { SUBIMP(); }
  virtual LongFloat as_long_float_() const { SUBIMP(); };

  virtual Number_sp conjugate_() const { SUBIMP(); };
  virtual Number_sp sin_() const { SUBIMP(); };
  virtual Number_sp cos_() const { SUBIMP(); };
  virtual Number_sp tan_() const { SUBIMP(); };
  virtual Number_sp sinh_() const { SUBIMP(); };
  virtual Number_sp cosh_() const { SUBIMP(); };
  virtual Number_sp tanh_() const { SUBIMP(); };

  virtual void sxhash_(HashGenerator &hg) const { SUBIMP(); };
  DEFAULT_CTOR_DTOR(Number_O);
};

SMART(Real);
class Real_O : public Number_O {
  LISP_BASE1(Number_O);
  LISP_CLASS(core, ClPkg, Real_O, "real");

public:
  virtual double as_double_() const { SUBIMP(); };

  // functions shared by all Real
  virtual bool plusp_() const { SUBIMP(); };
  virtual bool minusp_() const { SUBIMP(); };

  virtual Number_sp conjugate_() const;

  DEFAULT_CTOR_DTOR(Real_O);
};

SMART(Rational);
class Rational_O : public Real_O {
  LISP_BASE1(Real_O);
  LISP_CLASS(core, ClPkg, Rational_O, "rational");

public:
  static Rational_sp create(mpz_class const &num, mpz_class const &denom);
  static Rational_sp create(Integer_sp num, Integer_sp denom);

public:
  virtual gc::Fixnum as_int_() const { SUBIMP(); };
  virtual Number_sp log1_() const;
  virtual Number_sp log1p_() const;
  //	virtual Number_sp sqrt_() const;
  virtual Number_sp exp_() const;

  virtual Number_sp sin_() const;
  virtual Number_sp cos_() const;
  virtual Number_sp tan_() const;
  virtual Number_sp sinh_() const;
  virtual Number_sp cosh_() const;
  virtual Number_sp tanh_() const;

  DEFAULT_CTOR_DTOR(Rational_O);
};

SMART(Integer);
class Integer_O : public Rational_O {
  LISP_BASE1(Rational_O);
  LISP_CLASS(core, ClPkg, Integer_O, "integer");

public:
  /*! Return a Cons (integer low high) */
  static T_sp makeIntegerType(gc::Fixnum low, gc::Fixnum high);
  static Integer_sp create(const mpz_class &v);
  static Integer_sp create(gctools::Fixnum v);
  static Integer_sp create(const string &v) {
    return Integer_O::create(v.c_str());
  };
  static Integer_sp create(const char *v) {
    if (v[0] == '+') {
      // Skip leading +
      mpz_class zv(&v[1]);
      return create(zv);
    }
    mpz_class zv(v);
    return create(zv);
  };
//	static Integer_sp create(size_t v); // unsigned
//	static Integer_sp create(uint v);
#ifndef _TARGET_OS_LINUX
  static Integer_sp create(uint64_t v);
#endif
  static Integer_sp create(float f);
  static Integer_sp create(double f);
  static Integer_sp createLongFloat(LongFloat f);

public:
  virtual bool evenp_() const { SUBIMP(); };
  virtual bool oddp_() const { SUBIMP(); };

  virtual gc::Fixnum bit_length_() const { SUBIMP(); };

  /*! Return the value shifted by BITS bits.
	  If BITS < 0 shift right, if BITS >0 shift left. */
  virtual Integer_sp shift_(gc::Fixnum bits) const { SUBIMP(); };

  virtual uint64_t as_uint64_() const;
  virtual unsigned long long as_unsigned_long_long_() const { SUBIMP(); };
  virtual void __write__(T_sp strm) const;
  Integer_O(){};
  virtual ~Integer_O(){};
};
};


namespace core {

Fixnum_sp make_fixnum(gc::Fixnum x);
gc::Fixnum get_fixnum(Fixnum_sp x);

class Fixnum_dummy_O : public Integer_O {
  LISP_BASE1(Integer_O);
  LISP_CLASS(core, ClPkg, Fixnum_dummy_O, "fixnum");
#if 0

    public:
	friend class boost::serialization::access;
    public:
    private:
	gctools::Fixnum	_Value;
    public:
	static Fixnum_sp createFn(gctools::Fixnum nm);
    public:
	//	static int number_of_bits();
    public:
	// gc::Fixnum get() const { return this->_Value; };
	// Switch to this in unbox_fixnum() impl while doing source-to-source translation
	gc::Fixnum get_() const { return this->_Value; };

    public:
	NumberType number_type_() const { return number_Fixnum;};
	//	virtual Number_sp copy() const;
	string __repr__() const;
	Number_sp abs_() const { return make_fixnum(std::abs(this->_Value)); };
	Number_sp signum_() const;

	// math routines shared by all numbers
	virtual bool zerop_() const { return this->_Value == 0; };
	virtual Number_sp negate_() const { return make_fixnum(-this->_Value);};

	// Shared by real
	virtual bool plusp_() const { return this->_Value > 0; };
	virtual bool minusp_() const { return this->_Value < 0; };
	virtual bool evenp_() const { return !(this->_Value&1); };
	virtual bool oddp_() const { return (this->_Value&1);};

	virtual	bool eql_(T_sp obj) const;

	virtual Number_sp onePlus_() const
	{
	    if ( this->_Value == gctools::most_positive_fixnum) {
		Bignum bn(this->_Value);
		bn = bn + 1;
		return Integer_O::create(bn);
	    } else {
		return make_fixnum(this->_Value+1);
	    }
	};

	virtual Number_sp oneMinus_() const
	{
	    if ( this->_Value == gctools::most_negative_fixnum ) {
		Bignum bn(this->_Value);
		bn = bn - 1;
		return Integer_O::create(bn);
	    } else {
		return make_fixnum(this->_Value-1);
	    }
	};

    public:
	//	virtual	string	valueAsString_() const { stringstream ss; ss<<this->_Value;return ss.str();};
	//	virtual	void	setFromString( const string& strVal ) { this->_Value = atoi(strVal.c_str());};

	int bit_length_() const;
	/*! Return the value shifted by BITS bits.
	  If BITS < 0 shift right, if BITS >0 shift left. */
	Integer_sp shift_(gc::Fixnum bits) const;

	string asChar_() const;
	virtual gc::Fixnum as_int_() const;
	virtual uint64_t as_uint64_() const;
	virtual uint as_uint_() const;
	virtual Bignum as_mpz_() const;
	virtual LongLongInt as_LongLongInt_() const;
	virtual float as_float_() const;
	virtual double as_double_() const;
	virtual LongFloat as_long_float_() const;
	virtual unsigned long long as_unsigned_long_long_() const;
	void sxhash_(HashGenerator& hg) const;

    Fixnum_dummy_O(gc::Fixnum f) : _Value(f) {};
    Fixnum_dummy_O() : _Value(0) {};
#endif
};
inline Fixnum_sp make_fixnum(gc::Fixnum x) { return gc::make_tagged_fixnum<core::Fixnum_I>(x); };
inline gc::Fixnum unbox_fixnum(Fixnum_sp x) { return x.unsafe_fixnum(); };
};

namespace core {

SMART(Float);
class Float_O : public Real_O {
  LISP_BASE1(Real_O);
  LISP_CLASS(core, ClPkg, Float_O, "float");

public:
  virtual Integer_sp castToInteger() const { SUBIMP(); };

  virtual bool isnan_() const { SUBIMP(); };
  virtual bool isinf_() const { SUBIMP(); };

  DEFAULT_CTOR_DTOR(Float_O);
};

SMART(ShortFloat);
class ShortFloat_O : public Float_O {
  LISP_BASE1(Float_O);
  LISP_CLASS(core, ClPkg, ShortFloat_O, "ShortFloat");

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
private:
  float _Value;

public:
  static ShortFloat_sp create(float nm) {
    _G();
    GC_ALLOCATE(ShortFloat_O, sf);
    sf->_Value = nm;
    return sf;
  };

public:
  NumberType number_type_() const { return number_ShortFloat; };
  float get() const { return this->_Value; };
  void sxhash_(HashGenerator &hg) const;
  //	virtual Number_sp copy() const;
  Number_sp signum_() const;
  string __repr__() const;
  Number_sp abs_() const;
  bool isnan_() const { return this->_Value != this->_Value; }; // NaN is supposed to be the only value that != itself!
  bool isinf_() const { return isinf(this->_Value); };

public:
  //	virtual	string	valueAsString_() const;
  //	virtual	void	setFromString( const string& strVal );
  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const;
  virtual Number_sp reciprocal_() const;

  // math routines shared by all numbers
  virtual bool zerop_() const { return this->_Value == 0.0; };
  virtual Number_sp negate_() const { return ShortFloat_O::create(-this->_Value); };

  virtual Number_sp onePlus_() const { return ShortFloat_O::create(this->_Value + 1.0); };
  virtual Number_sp oneMinus_() const { return ShortFloat_O::create(this->_Value - 1.0); };

  // shared by real
  virtual bool plusp_() const { return this->_Value > 0.0; };
  virtual bool minusp_() const { return this->_Value < 0.0; };

  virtual float as_float_() const;
  virtual double as_double_() const;
  virtual LongFloat as_long_float_() const;

  Integer_sp castToInteger() const;

  DEFAULT_CTOR_DTOR(ShortFloat_O);
};

class SingleFloat_dummy_O : public Float_O {
  LISP_BASE1(Float_O);
  LISP_CLASS(core, ClPkg, SingleFloat_dummy_O, "SingleFloat");

public:
#if 0
	static SingleFloat_sp create(float nm)
	{_G();
            GC_ALLOCATE(SingleFloat_O,sf);
            sf->_Value = nm;
	    return sf;
	};
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	float	_Value;
    public:
    public:
	NumberType number_type_() const { return number_SingleFloat;};
	void sxhash_(HashGenerator& hg) const;
	float get() const { return this->_Value;};
	string __repr__() const;
	//	virtual Number_sp copy() const;
	Number_sp signum_() const;
	Number_sp abs_() const;
	bool isnan_() const {return this->_Value != this->_Value;}; // NaN is supposed to be the only value that != itself!!!!
    public:
	//	virtual	string	valueAsString_() const;
	//	virtual	void	setFromString( const string& strVal );
	//	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql_(T_sp obj) const;

	// math routines shared by all numbers
	virtual bool zerop_() const { return this->_Value == 0.0; };
	virtual Number_sp negate_() const { return make_single_float(-this->_Value);};

	// shared by real
	virtual bool plusp_() const { return this->_Value > 0.0; };
	virtual bool minusp_() const { return this->_Value < 0.0; };

	//	virtual Number_sp log1_() const;
	//	virtual Number_sp log1p_() const;
	virtual Number_sp sqrt_() const;

	virtual Number_sp onePlus_() const { return create(this->_Value+1.0);};
	virtual Number_sp oneMinus_() const { return create(this->_Value-1.0);};

	virtual float as_float_() const;
	virtual double as_double_() const;
	virtual LongFloat as_long_float_() const;

	virtual Number_sp reciprocal_() const;
	virtual Number_sp exp_() const;

	virtual Number_sp sin_() const;
	virtual Number_sp cos_() const;
	virtual Number_sp tan_() const;
	virtual Number_sp sinh_() const;
	virtual Number_sp cosh_() const;
	virtual Number_sp tanh_() const;

	//Integer_sp castToInteger() const;

	DEFAULT_CTOR_DTOR(SingleFloat_O);
#endif
};

inline SingleFloat_sp make_single_float(float x) { return gc::make_tagged_single_float<core::SingleFloat_I>(x); };
inline float unbox_single_float(SingleFloat_sp x) { return x.unsafe_single_float(); };
};

namespace core {
SMART(DoubleFloat);
class DoubleFloat_O : public Float_O {
  LISP_BASE1(Float_O);
  LISP_CLASS(core, ClPkg, DoubleFloat_O, "double-float");

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
private:
  double _Value;

public:
  static DoubleFloat_sp create(double nm) {
    _G();
    GC_ALLOCATE(DoubleFloat_O, v);
    v->set(nm);
    return v;
  };

public:
  NumberType number_type_() const { return number_DoubleFloat; };
  void sxhash_(HashGenerator &hg) const;
  //	virtual Number_sp copy() const;
  string __repr__() const;
  void set(double val) { this->_Value = val; };
  double get() const { return this->_Value; };
  Number_sp signum_() const;
  Number_sp abs_() const { return DoubleFloat_O::create(fabs(this->_Value)); };
  bool isnan_() const { return this->_Value != this->_Value; }; // NaN is supposed to be the only value that != itself!!!!
  bool isinf_() const { return isinf(this->_Value); };

public:
  //	virtual	string	valueAsString_() const;
  //	virtual	void	setFromString( const string& strVal );
  virtual bool eql_(T_sp obj) const;

  // math routines shared by all numbers
  bool zerop_() const { return this->_Value == 0.0; };
  virtual Number_sp negate_() const { return DoubleFloat_O::create(-this->_Value); };

  // Shared by real
  bool plusp_() const { return this->_Value > 0.0; };
  bool minusp_() const { return this->_Value < 0.0; };

  virtual Number_sp reciprocal_() const;
  virtual Number_sp sqrt_() const;

  virtual Number_sp onePlus_() const { return create(this->_Value + 1.0); };
  virtual Number_sp oneMinus_() const { return create(this->_Value - 1.0); };

  virtual Number_sp log1_() const;
  virtual Number_sp log1p_() const;

  virtual float as_float_() const;
  virtual double as_double_() const;
  virtual LongFloat as_long_float_() const;

  Integer_sp castToInteger() const;

  virtual Number_sp exp_() const;

  virtual Number_sp sin_() const;
  virtual Number_sp cos_() const;
  virtual Number_sp tan_() const;
  virtual Number_sp sinh_() const;
  virtual Number_sp cosh_() const;
  virtual Number_sp tanh_() const;

  DEFAULT_CTOR_DTOR(DoubleFloat_O);
};
};
template <>
struct gctools::GCInfo<core::DoubleFloat_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
SMART(LongFloat);
class LongFloat_O : public Float_O {
  LISP_BASE1(Float_O);
  LISP_CLASS(core, ClPkg, LongFloat_O, "LongFloat");

public:
private:
  //  LongFloat _Value;
public:
  static DoubleFloat_sp create(LongFloat nm) {
    return DoubleFloat_O::create(nm);
  };
#if 0
    static LongFloat_sp create(LongFloat nm) {
    return DoubleFloat_O::create(nm);
    DEPRECIATED();
    GC_ALLOCATE(LongFloat_O, v);
    v->_Value = nm;
    return v;
  };
#endif
public:
  NumberType number_type_() const { return number_LongFloat; };
#if 0
  void sxhash_(HashGenerator & hg) const;
  double get() const { return this->_Value; };
  LongFloat &ref() { return this->_Value; };
  string __repr__() const;
  //	virtual Number_sp copy() const;
  Number_sp signum_() const;
  Number_sp abs_() const;
  bool isnan_() const { return this->_Value != this->_Value; }; // NaN is supposed to be the only value that != itself!!!!
  bool isinf_() const { return isinf(this->_Value); };
public:
  //	virtual	string	valueAsString_() const;
  //	virtual	void	setFromString( const string& strVal );
  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const;

  // math routines shared by all numbers
  bool zerop_() const { return this->_Value == 0.0; };
  virtual Number_sp negate_() const { return LongFloat_O::create(-this->_Value); };

  // shared by real
  bool plusp_() const { return this->_Value > 0.0; };
  bool minusp_() const { return this->_Value < 0.0; };

  virtual Number_sp reciprocal_() const;

  virtual Number_sp sqrt_() const;

  virtual Number_sp onePlus_() const { return create(this->_Value + 1.0); };
  virtual Number_sp oneMinus_() const { return create(this->_Value - 1.0); };

  virtual float as_float_() const;
  virtual double as_double_() const;
  virtual LongFloat as_long_float_() const;

  Integer_sp castToInteger() const;

#ifdef CLASP_LONG_FLOAT
  virtual Number_sp log1_() const;
  virtual Number_sp log1p_() const;

  virtual Number_sp exp_() const;
  virtual Number_sp sin_() const;
  virtual Number_sp cos_() const;
  virtual Number_sp tan_() const;
  virtual Number_sp sinh_() const;
  virtual Number_sp cosh_() const;
  virtual Number_sp tanh_() const;
#endif
#endif
  DEFAULT_CTOR_DTOR(LongFloat_O);
};
};

namespace core {
SMART(Complex);
class Complex_O : public Number_O {
  LISP_BASE1(Number_O);
  LISP_CLASS(core, ClPkg, Complex_O, "complex");

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
GCPRIVATE:
  Real_sp _real;
  Real_sp _imaginary;

public:
  static Complex_sp create(double r, double i) {
    _G();
    GC_ALLOCATE(Complex_O, v);
    v->_real = DoubleFloat_O::create(r);
    v->_imaginary = DoubleFloat_O::create(i);
    return v;
  };
  static Complex_sp create(Real_sp r, Real_sp i) {
    _G();
    GC_ALLOCATE(Complex_O, v);
    v->_real = r;
    v->_imaginary = i;
    return v;
  }

public:
  NumberType number_type_() const { return number_Complex; };

  Real_sp real() const { return this->_real; };
  Real_sp imaginary() const { return this->_imaginary; };

  void setf_realpart(Real_sp r) { this->_real = r; };
  void setf_imagpart(Real_sp i) { this->_imaginary = i; };

  void sxhash_(HashGenerator &hg) const;
  //	virtual Number_sp copy() const;
  string __repr__() const;
  Number_sp signum_() const;
  Number_sp abs_() const;
  bool isnan_() const;

public:
  //	virtual	string	valueAsString_() const;
  //	virtual	void	setFromString( const string& str);
  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const;

  // math routines shared by all numbers
  bool zerop_() const { return (clasp_zerop(this->_real) && clasp_zerop(this->_imaginary)); };

  virtual Number_sp negate_() const { return Complex_O::create(gc::As<Real_sp>(clasp_negate(this->_real)),
                                                               gc::As<Real_sp>(clasp_negate(this->_imaginary))); };

  virtual Number_sp log1_() const;
  virtual Number_sp log1p_() const;

  virtual Number_sp onePlus_() const { return create(gc::As<Real_sp>(clasp_one_plus(this->_real)),
                                                     this->_imaginary); };
  virtual Number_sp oneMinus_() const { return create(gc::As<Real_sp>(clasp_one_minus(this->_real)),
                                                      this->_imaginary); };

  Number_sp sqrt_() const;

  virtual Number_sp exp_() const;

  virtual Number_sp sin_() const;
  virtual Number_sp cos_() const;
  virtual Number_sp tan_() const;
  virtual Number_sp sinh_() const;
  virtual Number_sp cosh_() const;
  virtual Number_sp tanh_() const;

  virtual Number_sp conjugate_() const;

  DEFAULT_CTOR_DTOR(Complex_O);
};

SMART(Ratio);
class Ratio_O : public Rational_O {
  LISP_BASE1(Rational_O);
  LISP_CLASS(core, ClPkg, Ratio_O, "ratio");

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
GCPRIVATE:
  Integer_sp _numerator;
  Integer_sp _denominator;

public:
  static Ratio_sp create(Integer_sp num, Integer_sp denom) {
    _G();
    GC_ALLOCATE(Ratio_O, v);
    if (clasp_to_mpz(denom) < 0) {
      v->_numerator = gc::As<Integer_sp>(clasp_negate(num));
      v->_denominator = gc::As<Integer_sp>(clasp_negate(denom));
    } else {
      v->_numerator = num;
      v->_denominator = denom;
    }
    return v;
  };
  static Ratio_sp create(mpz_class const &num, mpz_class const &denom) {
    _G();
    GC_ALLOCATE(Ratio_O, r);
    r->_numerator = Integer_O::create(num);
    r->_denominator = Integer_O::create(denom);
    return r;
  }
  static Ratio_sp create(const char *str) {
    _G();
    GC_ALLOCATE(Ratio_O, r);
    r->setFromString(str);
    return r;
  }

public:
  NumberType number_type_() const { return number_Ratio; };

  virtual bool zerop_() const { return clasp_zerop(this->_numerator); };
  virtual Number_sp negate_() const { return Ratio_O::create(clasp_negate(this->_numerator), this->_denominator); };

  Integer_sp numerator() const { return this->_numerator; };
  Integer_sp denominator() const { return this->_denominator; };
  Integer_sp num() const { return this->_numerator; };
  Integer_sp den() const { return this->_denominator; };
  mpz_class numerator_as_mpz() const;
  mpz_class denominator_as_mpz() const;

  void sxhash_(HashGenerator &hg) const;
  //	virtual Number_sp copy() const;
  string __repr__() const;
  Number_sp signum_() const;
  Number_sp abs_() const;
  bool isnan_() const;

public:
  //	virtual	string	valueAsString_() const;
  void setFromString(const string &str);
  virtual bool eql_(T_sp obj) const;

  Number_sp onePlus_() const { return create(gc::As<Integer_sp>(contagen_add(this->_numerator, this->_denominator)), this->_denominator); };
  Number_sp oneMinus_() const { return create(gc::As<Integer_sp>(contagen_sub(this->_numerator, this->_denominator)), this->_denominator); };

  virtual float as_float_() const;
  virtual double as_double_() const;
  virtual LongFloat as_long_float_() const;

  // functions shared by all Real

  bool plusp_() const {
    return clasp_plusp(this->_numerator);
  }

  bool minusp_() const {
    return clasp_minusp(this->_numerator);
  }

  DEFAULT_CTOR_DTOR(Ratio_O);
};

void clasp_deliver_fpe(int status);

inline Number_sp clasp_plus(Number_sp na, Number_sp nb) { return contagen_add(na, nb); };
inline Number_sp clasp_minus(Number_sp na, Number_sp nb) { return contagen_sub(na, nb); };
inline Number_sp clasp_times(Number_sp na, Number_sp nb) { return contagen_mul(na, nb); };
inline Number_sp clasp_divide(Number_sp na, Number_sp nb) { return contagen_div(na, nb); };

inline int clasp_number_compare(Number_sp x, Number_sp y) { return basic_compare(x, y); };

Number_sp clasp_atan2(Number_sp x, Number_sp y);

inline Number_sp float_sqrt(float f) {
  if (f < 0.0) {
    return Complex_O::create(clasp_make_single_float(0.0), clasp_make_single_float(sqrtf(-f)));
  } else {
    return clasp_make_single_float(sqrtf(f));
  }
}

inline Number_sp clasp_log1(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    if (f < 0)
      return clasp_log1_complex_inner(x, clasp_make_fixnum(0));
    return clasp_make_single_float(logf(f));
  } else if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    if (::isnan(f))
      return x;
    if (f < 0)
      return clasp_log1_complex_inner(x, clasp_make_fixnum(0));
    return clasp_make_single_float(logf(f));
  }
  return x->log1_();
}

inline Number_sp clasp_log1p(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    if (f < -1)
      return clasp_log1_complex_inner(clasp_one_plus(x), clasp_make_fixnum(0));
    return clasp_make_single_float(_log1p(f));
  } else if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    if (::isnan(f))
      return x;
    if (f < -1)
      return clasp_log1_complex_inner(clasp_one_plus(x), clasp_make_fixnum(0));
    return clasp_make_single_float(_log1p(f));
  }
  return x->log1p_();
};

Number_sp cl_expt(Number_sp x, Number_sp y);

Integer_sp clasp_ash(Integer_sp x, int bits);

inline gctools::Fixnum clasp_fixnum(Number_sp x) {
  return unbox_fixnum(Fixnum_sp(x));
}

inline float clasp_single_float(Number_sp x) {
  return unbox_single_float(x);
}

inline double clasp_double_float(Number_sp x) {
  return gc::As<DoubleFloat_sp>(x)->get();
}

#ifdef CLASP_LONG_FLOAT
inline LongFloat clasp_long_float(Number_sp x) {
  return x.as<LongFloat_O>()->get();
}
#endif

inline Ratio_sp clasp_make_ratio(Integer_sp num, Integer_sp denom) {
  return Ratio_O::create(num, denom);
}

inline Fixnum_sp clasp_make_fixnum(gc::Fixnum i) {
  return make_fixnum(i);
}

inline Integer_sp _clasp_float_to_integer(float d) {
  return Integer_O::create(d);
}

inline Integer_sp _clasp_double_to_integer(double d) {
  return Integer_O::create(d);
}

inline Integer_sp _clasp_long_float_to_integer(LongFloat d) {
  return Integer_O::create(d);
}

inline Integer_sp _clasp_long_double_to_integer(LongFloat d) {
  return Integer_O::create(d);
}

inline SingleFloat_sp clasp_make_single_float(float d) {
  return gc::make_tagged_single_float<core::SingleFloat_I>(d);
}

inline DoubleFloat_sp clasp_make_double_float(double d) {
  return DoubleFloat_O::create(d);
}

#ifdef CLASP_LONG_FLOAT
inline LongFloat_sp clasp_make_long_float(LongFloat d) {
  return LongFloat_O::create(d);
}
#endif

#define clasp_return2(ENV, n1, n2) return Values(n1, n2);
#define CLASP_REAL_TYPE_P(y) (gc::IsA<Real_sp>(y))

#define CLASP_FIXNUMP(n) (gc::IsA<Fixnum_sp>(n))

/*! In num_co.cc */
Real_mv clasp_floor1(Real_sp x);
Real_mv clasp_floor2(Real_sp x, Real_sp y);
Real_mv clasp_ceiling1(Real_sp x);
Real_mv clasp_ceiling2(Real_sp x, Real_sp y);

Real_mv clasp_truncate1(Real_sp x);
Real_mv clasp_truncate2(Real_sp x, Real_sp y);

Real_mv clasp_round1(Real_sp x);
Real_mv clasp_round2(Real_sp x, Real_sp y);

Real_sp clasp_max2(Real_sp x, Real_sp y);
Real_sp clasp_min2(Real_sp x, Real_sp y);

#define clasp_lowereq(x, y) (clasp_number_compare((x), (y)) <= 0)
#define clasp_greatereq(x, y) (clasp_number_compare((x), (y)) >= 0)
#define clasp_lower(x, y) (clasp_number_compare((x), (y)) < 0)
#define clasp_greater(x, y) (clasp_number_compare((x), (y)) > 0)

unsigned char clasp_toUint8(T_sp n);
signed char clasp_toSignedInt8(T_sp n);
cl_index clasp_toSize(T_sp f);

Integer_sp cl_logior(List_sp integers);
Integer_sp cl_logand(List_sp integers);

gctools::Fixnum fixint(T_sp x);

/*! Initialize all math functions here */
void initialize_numbers();

}; // namespace core

TRANSLATE(core::Number_O);   // superclass T_O
TRANSLATE(core::Real_O);     // superclass Number_O
TRANSLATE(core::Rational_O); // superclass Real_O
TRANSLATE(core::Integer_O);  // superclass Rational_O
TRANSLATE(core::Fixnum_O);   // superclass Integer_O
#if 0
TRANSLATE(core::SignedByte_O);	// superclass Integer_O
TRANSLATE(core::UnsignedByte_O);// superclass SignedByte_O
TRANSLATE(core::Bit_O);		// superclass UnsignedByte_O
#endif
TRANSLATE(core::Float_O); // superclass Real_O
TRANSLATE(core::ShortFloat_O);
TRANSLATE(core::SingleFloat_O);
TRANSLATE(core::DoubleFloat_O); // superclass DoubleFloat_O
#ifdef CLASP_LONG_FLOAT
TRANSLATE(core::LongFloat_O);
#endif
TRANSLATE(core::Ratio_O);   // superclass Rational_O
TRANSLATE(core::Complex_O); // superclass Number_O

namespace core {

inline bool clasp_plusp(Real_sp num) {
  if (num.fixnump()) {
    return num.unsafe_fixnum() > 0;
  } else if (num.single_floatp()) {
    return num.unsafe_single_float() > 0.0;
  }
  return num->plusp_();
}

inline bool clasp_minusp(Real_sp num) {
  if (num.fixnump()) {
    return num.unsafe_fixnum() < 0;
  } else if (num.single_floatp()) {
    return num.unsafe_single_float() < 0.0;
  }
  return num->minusp_();
}

inline bool clasp_evenp(Integer_sp num) {
  if (num.fixnump()) {
    return (num.unsafe_fixnum() % 2) == 0;
  }
  return num->evenp_();
}

inline bool clasp_oddp(Integer_sp num) {
  if (num.fixnump()) {
    return (num.unsafe_fixnum() % 2) == 1;
  }
  return num->oddp_();
}

inline Number_sp clasp_abs(Number_sp num) {
  if (num.fixnump()) {
    return immediate_fixnum<Number_O>(std::abs(num.unsafe_fixnum()));
  } else if (num.single_floatp()) {
    return immediate_single_float<Number_O>(std::fabs(num.unsafe_single_float()));
  }
  return num->abs_();
}

inline Number_sp clasp_signum(Number_sp num) {
  if (num.fixnump()) {
    Fixnum fn = num.unsafe_fixnum();
    if (fn == 0)
      return immediate_fixnum<Number_O>(0);
    if (fn > 0)
      return immediate_fixnum<Number_O>(1);
    return immediate_fixnum<Number_O>(-1);
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    if (fl == 0.0)
      return immediate_single_float<Number_O>(0.0);
    if (fl < 0.0)
      return immediate_single_float<Number_O>(-1.0);
    return immediate_single_float<Number_O>(1.0);
  }
  return num->signum_();
}

inline Number_sp clasp_one_plus(Number_sp num) {
  if (num.fixnump()) {
    return immediate_fixnum<Number_O>(num.unsafe_fixnum() + 1);
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    fl += 1.0;
    return immediate_single_float<Number_O>(fl);
  }
  return num->onePlus_();
}

inline Number_sp clasp_one_minus(Number_sp num) {
  if (num.fixnump()) {
    return immediate_fixnum<Number_O>(num.unsafe_fixnum() - 1);
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    fl -= 1.0;
    return immediate_single_float<Number_O>(fl);
  }
  return num->oneMinus_();
}

inline bool clasp_zerop(Number_sp num) {
  if (num.fixnump()) {
    return num.unsafe_fixnum() == 0;
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    return fl == 0.0;
  }
  return num->zerop_();
}

inline Number_sp clasp_negate(Number_sp num) {
  if (num.fixnump()) {
    return immediate_fixnum<Number_O>(-num.unsafe_fixnum());
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    fl = -fl;
    return immediate_single_float<Number_O>(fl);
  }
  return num->negate_();
}

inline NumberType clasp_t_of(Number_sp n) {
  if (n.fixnump()) {
    return number_Fixnum;
  } else if (n.single_floatp()) {
    return number_SingleFloat;
  }
  return n->number_type_();
}

inline Integer_sp clasp_shift(Integer_sp n, int bits) {
  if (n.fixnump()) {
    if (bits < 0) {
      Fixnum y = n.unsafe_fixnum();
      bits = -bits;
      if (bits >= gc::fixnum_bits) {
        y = (y < 0) ? -1 : 0;
      } else {
        y >>= bits;
      }
      return immediate_fixnum<Number_O>(y);
    } else {
      Bignum val(n.unsafe_fixnum());
      Bignum res;
      mpz_mul_2exp(res.get_mpz_t(), val.get_mpz_t(), bits);
      return Integer_O::create(res);
    }
  }
  return n->shift_(bits);
}

inline gc::Fixnum clasp_integer_length(Integer_sp x) {
  if (x.fixnump()) {
    Fixnum i(x.unsafe_fixnum());
    Fixnum count = 0;
    if (i < 0)
      i = ~i;
    for (; i && (count < FIXNUM_BITS); i >>= 1, count++)
      ;
    return count;
  }
  return x->bit_length_();
}

inline int clasp_to_int(Integer_sp x) {
  if (x.fixnump()) {
    Fixnum fn = x.unsafe_fixnum();
    if (fn < gc::most_negative_int || fn >= gc::most_positive_int) {
      TYPE_ERROR(x, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(gc::most_negative_int), make_fixnum(gc::most_positive_int)));
    }
    return (uint)fn;
  }
  return x->as_uint_();
}

inline uint clasp_to_uint(Integer_sp x) {
  if (x.fixnump()) {
    Fixnum fn = x.unsafe_fixnum();
    if (fn < 0 || fn >= gc::most_positive_uint) {
      TYPE_ERROR(x, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(gc::most_positive_uint)));
    }
    return (uint)fn;
  }
  return x->as_uint_();
}

inline uint64_t clasp_to_uint64(Integer_sp x) {
  if (x.fixnump()) {
    Fixnum fn = x.unsafe_fixnum();
    if (fn >= 0 & fn <= gc::most_positive_uint64) {
      return (uint64_t)fn;
    }
    mpz_class z = clasp_create_mpz_class(gc::most_positive_uint64);
    TYPE_ERROR(x, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), Integer_O::create(z)));
  }
  return x->as_uint64_();
}

inline mpz_class clasp_to_mpz(Integer_sp x) {
  if (x.fixnump()) {
    Fixnum fn = x.unsafe_fixnum();
    mpz_class z = fn;
    return z;
  }
  return x->as_mpz_();
}
inline unsigned long long clasp_to_unsigned_long_long(Integer_sp i) {
  if (i.fixnump()) {
    gc::Fixnum f = i.unsafe_fixnum();
    if (f >= 0 && f <= gc::most_positive_unsigned_long_long) {
      return (unsigned long long)f;
    }
    // unsigned long int must == unsigned long long int
    mpz_class z = clasp_create_mpz_class(gc::most_positive_unsigned_long_long);
    TYPE_ERROR(i, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0),
                                     Integer_O::create(z)));
  }
  return i->as_unsigned_long_long_();
};

inline Fixnum clasp_to_fixnum(Integer_sp i) {
  if (i.fixnump()) {
    gc::Fixnum f = i.unsafe_fixnum();
    if (f >= gc::most_negative_fixnum && f <= gc::most_positive_fixnum) {
      return f;
    }
    TYPE_ERROR(i, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(gc::most_negative_fixnum), make_fixnum(gc::most_positive_fixnum)));
  }
  return i->as_int_();
};

inline cl_index clasp_to_size(Integer_sp i) {
  if (i.fixnump()) {
    gc::Fixnum f = i.unsafe_fixnum();
    if (f >= 0 && f <= gc::most_positive_fixnum) {
      return f;
    }
    TYPE_ERROR(i, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(gc::most_positive_fixnum)));
  }
  gc::Fixnum f = i->as_int_();
  if (f >= 0)
    return f;
  TYPE_ERROR(i, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(gc::most_positive_fixnum)));
};

inline uint32_t clasp_to_uint32_t(Integer_sp i) {
  if (i.fixnump()) {
    gc::Fixnum f = i.unsafe_fixnum();
    if (f >= 0 && f <= gc::most_positive_uint32) {
      return f;
    }
    TYPE_ERROR(i, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(gc::most_positive_uint32)));
  }
  TYPE_ERROR(i, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(gc::most_positive_uint32)));
};

inline float clasp_to_float(Number_sp x) {
  if (x.fixnump()) {
    float d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    float d = x.unsafe_single_float();
    return d;
  }
  return x->as_float_();
};
inline double clasp_to_double(Number_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    double d = x.unsafe_single_float();
    return d;
  }
  return x->as_double_();
};
inline LongFloat clasp_to_long_float(Number_sp x) { return x->as_long_float_(); };
inline LongFloat clasp_to_long_double(Number_sp x) { return x->as_long_float_(); };

inline Number_sp clasp_sqrt(Number_sp z) {
  if (z.fixnump()) {
    float f = z.unsafe_fixnum();
    return float_sqrt(f);
  } else if (z.single_floatp()) {
    float f = z.unsafe_single_float();
    return float_sqrt(f);
  }
  return z->sqrt_();
}

inline Number_sp clasp_reciprocal(Number_sp x) {
  if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    return clasp_make_single_float(1.0 / f);
  }
  return x->reciprocal_();
}

inline Number_sp clasp_exp(Number_sp x) {
  if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    return clasp_make_single_float(expf(f));
  }
  return x->exp_();
}

inline Number_sp clasp_sin(Number_sp x) {
  if (x.single_floatp())
    return clasp_make_single_float(sinf(x.unsafe_single_float()));
  return x->sin_();
}
inline Number_sp clasp_cos(Number_sp x) {
  if (x.single_floatp())
    return clasp_make_single_float(cosf(x.unsafe_single_float()));
  return x->cos_();
}
inline Number_sp clasp_tan(Number_sp x) {
  if (x.single_floatp())
    return clasp_make_single_float(tanf(x.unsafe_single_float()));
  return x->tan_();
}

inline Number_sp clasp_sinh(Number_sp x) {
  if (x.single_floatp())
    return clasp_make_single_float(sinhf(x.unsafe_single_float()));
  return x->sinh_();
}
inline Number_sp clasp_cosh(Number_sp x) {
  if (x.single_floatp())
    return clasp_make_single_float(coshf(x.unsafe_single_float()));
  return x->cosh_();
}
inline Number_sp clasp_tanh(Number_sp x) {
  if (x.single_floatp())
    return clasp_make_single_float(tanhf(x.unsafe_single_float()));
  return x->tanh_();
}

inline Number_sp clasp_conjugate(Number_sp x) {
  if (x.fixnump())
    return x;
  if (x.single_floatp())
    return x;
  return x->conjugate_();
}

inline bool clasp_float_nan_p(Float_sp num) {
  if (num.single_floatp()) {
    float f = num.unsafe_single_float();
    return f != f;
  }
  return num->isnan_();
}

inline bool clasp_float_infinity_p(Float_sp num) {
  if (num.single_floatp()) {
    float f = num.unsafe_single_float();
    return isinf(f);
  }
  return num->isnan_();
}
};

#endif //]
