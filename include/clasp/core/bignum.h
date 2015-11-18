/*
    File: bignum.h
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
#ifndef _core_bignum_H_
#define _core_bignum_H_

#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bignum.fwd.h>

namespace core {

struct BignumExportBuffer {
  BignumExportBuffer() : buffer(NULL), bufferSize(0){};
  ~BignumExportBuffer() {
    if (this->buffer)
      free(this->buffer);
  };
  unsigned int *buffer = NULL;
  size_t bufferSize = 0;
  unsigned int *getOrAllocate(const mpz_class &bignum, int nail);
};

class Bignum_O : public Integer_O {
  LISP_BASE1(Integer_O);
  LISP_CLASS(core, ClPkg, Bignum_O, "Bignum");
  //	DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(Bignum_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit Bignum_O(core::Class_sp const& mc) : T_O(mc), Integer(mc) {};
        //    virtual ~Bignum_O() {};
public:
  //	void initialize();

private: // instance variables here
  Bignum _value;

public: // Functions here
  static Bignum_sp make(const string &value_in_string);
  static Bignum_sp create(gc::Fixnum i) {
    GC_ALLOCATE(Bignum_O, b);
    b->_value = i;
    return b;
  };
  static Bignum_sp create(mpz_class v) {
    GC_ALLOCATE(Bignum_O, b);
    b->_value = v;
    return b;
  };

public:
  NumberType number_type_() const { return number_Bignum; };

  mpz_class &ref() { return this->_value; };

  string __repr__() const;

  /*! Return true if the number fits in a signed int */
  bool fits_sint_p();

  virtual void increment() { ++this->_value; };
  virtual void decrement() { --this->_value; };
  //virtual Number_sp copy() const;
  string description() const {
    stringstream ss;
    ss << this->_value;
    return ss.str();
  };
  void set(gc::Fixnum val) { this->_value = val; };
  void setFixnum(gctools::Fixnum val) { this->_value = val; };
  Bignum get() const;
  Bignum get_or_if_nil_default(Bignum default_value) const;
  Number_sp abs_() const;
  void increment(gc::Fixnum i) { this->_value += i; };
  int sign() const { return this->_value > 0 ? 1 : (this->_value < 0 ? -1 : 0); };

  virtual bool zerop_() const { return ((this->_value == 0)); }
  virtual bool plusp_() const { return ((this->_value > 0)); }
  virtual bool minusp_() const { return ((this->_value < 0)); }

  virtual Number_sp oneMinus_() const {
    return Integer_O::create(this->_value - 1);
  }
  virtual Number_sp onePlus_() const {
    return Integer_O::create(this->_value + 1);
  }

  virtual gc::Fixnum bit_length_() const;

  /*! Return the value shifted by BITS bits.
	  If BITS < 0 shift right, if BITS >0 shift left. */
  Integer_sp shift_(gc::Fixnum bits) const;

  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const;

public:
  virtual string valueAsString() const {
    stringstream ss;
    ss << this->_value;
    return ss.str();
  };
  virtual void setFromString(const string &strVal);

  virtual gc::Fixnum as_int_() const;
  virtual uint64_t as_uint64_() const;
  string as_uint64_string() const;
  virtual Bignum as_mpz_() const;
  virtual LongLongInt as_LongLongInt_() const;
  virtual unsigned long long as_unsigned_long_long_() const;
  virtual float as_float_() const;
  virtual double as_double_() const;
  virtual LongFloat as_long_float_() const;

  void sxhash_(HashGenerator &hg) const;

  virtual bool evenp_() const { return (mpz_get_ui(this->_value.get_mpz_t()) & 1) == 0; };
  virtual bool oddp_() const { return (mpz_get_ui(this->_value.get_mpz_t()) & 1) != 0; };

  Number_sp log1() const;

}; // Bignum class

}; // core namespace
TRANSLATE(core::Bignum_O);

namespace translate {
template <>
struct from_object<const Bignum &, std::true_type> {
  typedef Bignum DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    _G();
    if (core::Bignum_sp bn = o.asOrNull<core::Bignum_O>()) {
      _v = bn->ref();
      ;
      return;
    }
    SIMPLE_ERROR(BF("Handle conversions of %s to Bignum") % _rep_(o));
  }
};
};

namespace core {

Integer_mv big_ceiling(Bignum_sp a, Bignum_sp b);
Integer_mv big_floor(Bignum_sp a, Bignum_sp b);

inline Integer_sp _clasp_big_register_normalize(Bignum_sp x) {
  return Integer_O::create(x->get());
}

inline Integer_sp _clasp_big_floor(Bignum_sp a, Bignum_sp b, Real_sp *rP) {
  Integer_mv res_mv = big_floor(a, b);
  *rP = Real_sp(res_mv.valueGet(1));
  return res_mv;
};

inline Integer_sp _clasp_big_ceiling(Bignum_sp a, Bignum_sp b, Real_sp *rP) {
  Integer_mv res_mv = big_ceiling(a, b);
  *rP = Real_sp(res_mv.valueGet(1));
  return res_mv;
}

inline double _clasp_big_to_double(Bignum_sp a) {
  return a->as_double_();
}

void clasp_big_register_free(Bignum_sp x);

Integer_sp _clasp_fix_divided_by_big(const Fixnum &x, const Bignum &y);
Integer_sp _clasp_big_divided_by_fix(const Bignum &x, const Fixnum &y);
Integer_sp _clasp_big_divided_by_big(const Bignum &x, const Bignum &y);

Integer_sp _clasp_big_gcd(Bignum_sp x, Bignum_sp y);

#define CLASP_BIGNUM_SIZE(x) ((x)->_mp_size)
#define CLASP_BIGNUM_ABS_SIZE(x) \
  (CLASP_BIGNUM_SIZE(x) < 0 ? -CLASP_BIGNUM_SIZE(x) : CLASP_BIGNUM_SIZE(x))
};

#endif /* _bignum_H_ */
