/*
    File: bignum.cc
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
//#define DEBUG_LEVEL_FULL

#include <boost/format.hpp>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_PKG_NAME(CorePkg,make-bignum);
CL_DEFUN Bignum_sp Bignum_O::make(const string &value_in_string) {
  GC_ALLOCATE(Bignum_O, bn);
  bn->_value = value_in_string;
  return ((bn));
};

Bignum Bignum_O::as_mpz_() const {
  return ((this->_value));
}

LongLongInt Bignum_O::as_LongLongInt_() const {
  LIKELY_if (this->_value.fits_sint_p()) {
    return ((this->_value.get_si()));
  }
  SIMPLE_ERROR(BF("Cannot convert Bignum %s to long long") % this->__repr__());
}

unsigned long long Bignum_O::as_unsigned_long_long_() const {
  if (sizeof(unsigned long long) == sizeof(uint64_t)) {
    return this->as_uint64_();
  }
  SIMPLE_ERROR(BF("Handle unsigned long long != uint64_t"));
  //	TYPE_ERROR(this->asSmartPtr(),Cons_O::createList(cl::_sym_Integer_O,make_fixnum(0),Integer_O::create(gc::most_positive_unsigned_long_long)));
}

void Bignum_O::sxhash_(HashGenerator &hg) const {
  hg.addPart(this->_value);
}

gc::Fixnum Bignum_O::as_int_() const {
  IMPLEMENT_MEF("Implement conversion of Bignum to Fixnum");
  if (this->_value.fits_sint_p()) {
    return ((this->_value.get_si()));
  }
  TYPE_ERROR(this->asSmartPtr(), Cons_O::createList(cl::_sym_Integer_O, make_fixnum(gc::most_negative_int), make_fixnum(gc::most_positive_int)));
}

int64_t Bignum_O::as_int64_() const
{
  size_t sizeinbase2 = mpz_sizeinbase(this->_value.get_mpz_t(),2);

  if ( sizeinbase2 > 64 )
  {
    goto BAD;
  }
  else
  {
    int64_t   val   = 0;
    size_t    count = 0;
    int       sign  = 0;

    int64_t * valP  = (int64_t *)::mpz_export( &val,
                                               &count,
                                               _lisp->integer_ordering()._mpz_import_word_order,
                                               sizeof(int64_t),
                                               _lisp->integer_ordering()._mpz_import_endian,
                                               0,
                                               this->_value.get_mpz_t() );

    sign = mpz_sgn(this->_value.get_mpz_t());
    if ( sign < 0 )
    {
      val = -val;
    }

    return val;
  }

 BAD:

  SIMPLE_ERROR(BF("The value %s won't fit into an int64_t") % _rep_(this->asSmartPtr()));

}

uint64_t Bignum_O::as_uint64_() const
{
  size_t sizeinbase2 = mpz_sizeinbase( this->_value.get_mpz_t(), 2 );

  if ( sizeinbase2 > 64 )
  {
    goto BAD;
  }
  else
  {
    uint64_t   val    = 0;
    size_t     count  = 0;

    uint64_t * valP   = (uint64_t *)::mpz_export( &val,
                                                  &count,
                                                  _lisp->integer_ordering()._mpz_import_word_order,
                                                  sizeof(uint64_t),
                                                  _lisp->integer_ordering()._mpz_import_endian,
                                                  0,
                                                  this->_value.get_mpz_t() );
    return val;
  }

 BAD:

  SIMPLE_ERROR(BF("The value %s won't fit into an uint64_t") % _rep_(this->asSmartPtr()));

}

/*! This helps us debug the as_uint64 function by returning a string representation of the uint64 */
CL_LISPIFY_NAME("core:asUint64String");
CL_DEFMETHOD string Bignum_O::as_uint64_string() const {
  uint64_t ui64 = clasp_to_uint64(this->asSmartPtr());
  stringstream ss;
  ss << ui64;
  return ((ss.str()));
}

CL_LISPIFY_NAME("core:fitsSintP");
CL_DEFMETHOD bool Bignum_O::fits_sint_p() {
  return ((this->_value.fits_sint_p()));
}

// --- TRANSLATION METHODS ---

// -- SHORT --

inline short Bignum_O::as_short() const {
  return static_cast<short>(this->get().get_si());
}

inline unsigned short Bignum_O::as_ushort() const {
  return static_cast<unsigned short>(this->get().get_ui());
}

// -- INT --

inline int Bignum_O::as_int() const {
  return static_cast<int>(this->get().get_si());
}

inline unsigned int Bignum_O::as_uint() const {
  return static_cast<unsigned int>(this->get().get_ui());
}

// --  LONG --

inline long Bignum_O::as_long() const {
  return static_cast<long>(this->get().get_si());
}

inline unsigned long Bignum_O::as_ulong() const {
  return static_cast<unsigned long>(this->get().get_ui());
}

// -- LONG LONG --

inline long long Bignum_O::as_longlong() const {
#ifdef CLASP_MS_WINDOWS_HOST
#error "Add support for windows and long long bignum conversions"
#endif
  return this->as_int64_();
}

inline unsigned long long Bignum_O::as_ulonglong() const {
#ifdef CLASP_MS_WINDOWS_HOST
#error "Add support for windows and unsigned long long bignum conversions"
#endif
  return this->as_uint64_();
}

// -- INT8 --

inline int8_t Bignum_O::as_int8_t() const {
  return static_cast<int8_t>(this->get().get_si());
}

inline uint8_t Bignum_O::as_uint8_t() const {
  return static_cast<uint8_t>(this->get().get_ui());
}

// -- INT16 --

inline int16_t Bignum_O::as_int16_t() const {
  return static_cast<int16_t>(this->get().get_si());
}

inline uint16_t Bignum_O::as_uint16_t() const {
  return static_cast<uint16_t>(this->get().get_ui());
}

// -- INT32 --

inline int32_t Bignum_O::as_int32_t() const {
  return static_cast<int32_t>(this->get().get_si());
}

inline uint32_t Bignum_O::as_uint32_t() const {
  return static_cast<uint32_t>(this->get().get_ui());
}

// -- INT64 --

inline int64_t Bignum_O::as_int64_t() const {
  return static_cast<int64_t>( this->as_int64_() );
}

inline uint64_t Bignum_O::as_uint64_t() const {
  return static_cast<uint64_t>( this->as_uint64_() );
}

// -- CL_INTPTR_T --

inline cl_intptr_t Bignum_O::as_cl_intptr_t() const
{
  if( this->get().get_si() >= 0 )
  {
    return static_cast<cl_intptr_t>( this->get().get_si() );
  }

  SIMPLE_ERROR(BF("Value %llud out of range for type CL_INTPTR_T .") % (unsigned long long) this->get().get_si() );
}

// -- PTRDIFF_T --

inline ptrdiff_t Bignum_O::as_ptrdiff_t() const {
  if( this->get().get_si() >= 0 ) {
    return static_cast<ptrdiff_t>(  this->get().get_si() );
  }
  SIMPLE_ERROR(BF("Value %lld out of range for type PTRDIFF_T .") % (long long) this->get().get_si() );
}

// -- SIZE_T --

inline size_t Bignum_O::as_size_t() const {
  if(( this->get().get_si() >= gc::most_negative_size ) && ( this->get().get_si() <= gc::most_positive_size )) {
    return static_cast<size_t>( this->get().get_si() );
  }

  SIMPLE_ERROR(BF("Value %lld out of range for integer type SIZE_T .") % (long long) this->get().get_si() );
}

// -- SSIZE_T --

inline ssize_t Bignum_O::as_ssize_t() const {
  if(( this->get().get_si() >= gc::most_negative_ssize ) && ( this->get().get_si() <= gc::most_positive_ssize )) {
    return static_cast<ssize_t>( this->get().get_si() );
  }

  SIMPLE_ERROR(BF("Value %lld out of range for integer type SSIZE_T .") % (long long) this->get().get_si() );
}

// --- ---

float Bignum_O::as_float_() const {
  return static_cast<float_t>( (this->_value.get_d()) );
}

double Bignum_O::as_double_() const {
  return static_cast<double>( (this->_value.get_d()) );
}

LongFloat Bignum_O::as_long_float_() const {
  return static_cast<LongFloat>( (this->_value.get_d()) );
}

// --- END OF TRANSLATION METHODS ---

void Bignum_O::setFromString(const string &strVal) {
  this->_value = strVal;
}

gc::Fixnum Bignum_O::bit_length_() const {
  Bignum x = this->_value;
  if (this->sign() < 0) {
    // issue #536
    // from ECL: logxor(2,x,ecl_make_fixnum(-1)); before calling mpz_sizeinbase on x
    mpz_class temp;
    mpz_xor(temp.get_mpz_t(),clasp_to_mpz(clasp_make_fixnum(2)).get_mpz_t(), x.get_mpz_t());
    mpz_class temp1;
    mpz_xor(temp1.get_mpz_t(), temp.get_mpz_t(), clasp_to_mpz(clasp_make_fixnum(-1)).get_mpz_t());
    return mpz_sizeinbase(temp1.get_mpz_t(), 2);
  } else {
    return mpz_sizeinbase(x.get_mpz_t(), 2);
  }
}

/*! Return the value shifted by BITS bits.
      If BITS < 0 shift right, if BITS >0 shift left. */
Integer_sp Bignum_O::shift_(gc::Fixnum bits) const {
  if (bits == 0)
    return this->asSmartPtr();
  Bignum res;
  if (bits < 0) {
    mpz_div_2exp(res.get_mpz_t(), this->_value.get_mpz_t(), -bits);
  } else {
    mpz_mul_2exp(res.get_mpz_t(), this->_value.get_mpz_t(), bits);
  }
  return Integer_O::create(res);
}

string Bignum_O::__repr__() const {
  stringstream ss;
  ss << this->_value;
  return ((ss.str()));
}

Number_sp Bignum_O::signum_() const {
  if (this->zerop_())
    return immediate_fixnum<Number_O>(0);
  else if (this->plusp_())
    return immediate_fixnum<Number_O>(1);
  else
    return immediate_fixnum<Number_O>(-1);
}

Bignum Bignum_O::get() const {
  return ((this->_value));
}

Number_sp Bignum_O::abs_() const {
  GC_ALLOCATE(Bignum_O, cp);
  cp->_value = this->_value * ::sgn(this->_value);
  return ((cp));
}

bool Bignum_O::eql_(T_sp o) const {
  if (o.fixnump()) {
    return (this->_value == clasp_to_mpz(gc::As<Fixnum_sp>(o)));
  } else if (Integer_sp oi = o.asOrNull<Integer_O>()) {
    return (this->_value == clasp_to_mpz(oi));
  }
  return false;
}

Integer_mv big_ceiling(Bignum_sp a, Bignum_sp b) {
  Bignum mpzq, mpzr;
  mpz_cdiv_qr(mpzq.get_mpz_t(),
              mpzr.get_mpz_t(),
              a->ref().get_mpz_t(),
              b->ref().get_mpz_t());
  return Values(Integer_O::create(mpzq), Integer_O::create(mpzr));
}

Integer_mv big_floor(Bignum_sp a, Bignum_sp b) {
  Bignum_sp q = my_thread->bigRegister0();
  Bignum_sp r = my_thread->bigRegister1();
  mpz_fdiv_qr(q->ref().get_mpz_t(), r->ref().get_mpz_t(),
              a->ref().get_mpz_t(), b->ref().get_mpz_t());
  return Values(Integer_O::create(q->get()), Integer_O::create(r->get()));
}

Integer_sp _clasp_big_gcd(Bignum_sp x, Bignum_sp y) {
  Bignum zz;
  mpz_gcd(zz.get_mpz_t(), x->ref().get_mpz_t(), y->ref().get_mpz_t());
  return Bignum_O::create(zz);
}

Integer_sp _clasp_big_divided_by_big(const Bignum &a, const Bignum &b) {
  size_t size_a = CLASP_BIGNUM_ABS_SIZE(a.get_mpz_t());
  size_t size_b = CLASP_BIGNUM_ABS_SIZE(b.get_mpz_t());
  Fixnum size_z = size_a - size_b + 1;
  if (size_z <= 0)
    size_z = 1;
  Bignum z;
  mpz_tdiv_q(z.get_mpz_t(), a.get_mpz_t(), b.get_mpz_t());
  return Integer_O::create(z);
}

Integer_sp _clasp_big_divided_by_fix(const Bignum &x, const Fixnum &y) {
  Bignum by(GMP_LONG(y));
  return _clasp_big_divided_by_big(x, by);
}

Integer_sp _clasp_fix_divided_by_big(const Fixnum &x, const Bignum &y) {
  Bignum bx(GMP_LONG(x));
  return _clasp_big_divided_by_big(bx, y);
}

void clasp_big_register_free(Bignum_sp b) {
  // ECL just returns but we
  // could clear out the bignum register if it's too big
  return;
}

Bignum CStrToBignum(const char *str) {
  Bignum bn = 0;
  for (const unsigned char *cp = (const unsigned char *)str; *cp; ++cp) {
    bn = (bn << 7) | ((*cp) & 0x7f);
  }
  return bn;
}


CL_DEFUN void core__test_bignum_to_int64(Bignum_sp b) {
  size_t sizeinbase2 = mpz_sizeinbase(b->mpz_ref().get_mpz_t(),2);
  printf("%s:%d sizeinbase2 = %lu\n", __FILE__, __LINE__, sizeinbase2);
  if (sizeinbase2>64) goto BAD;
  {
    int64_t val;
    size_t count;
    int64_t* valP = (int64_t *)::mpz_export(&val, &count,
                                            _lisp->integer_ordering()._mpz_import_word_order,
                                            sizeof(int64_t),//_lisp->integer_ordering()._mpz_import_size,
                                            _lisp->integer_ordering()._mpz_import_endian,
                                            0,
                                            b->mpz_ref().get_mpz_t());
    int sgn = mpz_sgn(b->mpz_ref().get_mpz_t());
    if (sgn<0) {
      val = -val;
    }
    SIMPLE_WARN(BF("Converted bignum sgn -> %d  val -> %d\n") % sgn % val);
    return;
  }
 BAD:
  SIMPLE_ERROR(BF("The value %s won't fit into an int64_t") % b);
};
};
