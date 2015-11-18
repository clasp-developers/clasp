#include <clasp/core/foundation.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/bits.h>
#include <clasp/core/wrappers.h>

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_log.c  -- Logical operations on numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

namespace core {

/*
 * BIT OPERATIONS FOR FIXNUMS
 */

static gctools::Fixnum
ior_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (i | j);
}

static gctools::Fixnum
xor_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (i ^ j);
}

static gctools::Fixnum
and_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (i & j);
}

static gctools::Fixnum
eqv_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (~(i ^ j));
}

static gctools::Fixnum
nand_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (~(i & j));
}

static gctools::Fixnum
nor_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (~(i | j));
}

static gctools::Fixnum
andc1_op(gctools::Fixnum i, gctools::Fixnum j) {
  return ((~i) & j);
}

static gctools::Fixnum
andc2_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (i & (~j));
}

static gctools::Fixnum
orc1_op(gctools::Fixnum i, gctools::Fixnum j) {
  return ((~i) | j);
}

static gctools::Fixnum
orc2_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (i | (~j));
}

static gctools::Fixnum
b_clr_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (0);
}

static gctools::Fixnum
b_set_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (-1);
}

static gctools::Fixnum
b_1_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (i);
}

static gctools::Fixnum
b_2_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (j);
}

static gctools::Fixnum
b_c1_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (~i);
}

static gctools::Fixnum
b_c2_op(gctools::Fixnum i, gctools::Fixnum j) {
  return (~j);
}

typedef gctools::Fixnum (*bit_operator)(gctools::Fixnum, gctools::Fixnum);

static bit_operator fixnum_operations[16] = {
    b_clr_op,
    and_op,
    andc2_op,
    b_1_op,
    andc1_op,
    b_2_op,
    xor_op,
    ior_op,
    nor_op,
    eqv_op,
    b_c2_op,
    orc2_op,
    b_c1_op,
    orc1_op,
    nand_op,
    b_set_op};

// ----------------------------------------------------------------------

static void
mpz_ior_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_ior(out->get().get_mpz_t(), i->get().get_mpz_t(), j->get().get_mpz_t());
}

static void
mpz_xor_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_xor(out->get().get_mpz_t(), i->get().get_mpz_t(), j->get().get_mpz_t());
}

static void
mpz_and_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_and(out->get().get_mpz_t(), i->get().get_mpz_t(), j->get().get_mpz_t());
}

static void
mpz_eqv_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_xor(out->get().get_mpz_t(), i->get().get_mpz_t(), j->get().get_mpz_t());
  mpz_com(out->get().get_mpz_t(), out->get().get_mpz_t());
}

static void
mpz_nand_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_and(out->get().get_mpz_t(), i->get().get_mpz_t(), j->get().get_mpz_t());
  mpz_com(out->get().get_mpz_t(), out->get().get_mpz_t());
}

static void
mpz_nor_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_ior(out->get().get_mpz_t(), i->get().get_mpz_t(), j->get().get_mpz_t());
  mpz_com(out->get().get_mpz_t(), out->get().get_mpz_t());
}

static void
mpz_andc1_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_com(out->get().get_mpz_t(), i->get().get_mpz_t());
  mpz_and(out->get().get_mpz_t(), out->get().get_mpz_t(), j->get().get_mpz_t());
}

static void
mpz_orc1_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_com(out->get().get_mpz_t(), i->get().get_mpz_t());
  mpz_ior(out->get().get_mpz_t(), out->get().get_mpz_t(), j->get().get_mpz_t());
}

static void
mpz_andc2_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  /* (i & ~j) = ~((~i) | j) */
  mpz_orc1_op(out, i, j);
  mpz_com(out->get().get_mpz_t(), out->get().get_mpz_t());
}

static void
mpz_orc2_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  /* (i | ~j) = ~((~i) & j) */
  mpz_andc1_op(out, i, j);
  mpz_com(out->get().get_mpz_t(), out->get().get_mpz_t());
}

static void
mpz_b_clr_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_set_si(out->get().get_mpz_t(), 0);
}

static void
mpz_b_set_op(Bignum_sp o, Bignum_sp i, Bignum_sp j) {
  mpz_set_si(o->get().get_mpz_t(), -1);
}

static void
mpz_b_1_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  if (i != out)
    mpz_set(out->get().get_mpz_t(), i->get().get_mpz_t());
}

static void
mpz_b_2_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_set(out->get().get_mpz_t(), j->get().get_mpz_t());
}

static void
mpz_b_c1_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_com(out->get().get_mpz_t(), i->get().get_mpz_t());
}

static void
mpz_b_c2_op(Bignum_sp out, Bignum_sp i, Bignum_sp j) {
  mpz_com(out->get().get_mpz_t(), j->get().get_mpz_t());
}

typedef void (*_clasp_big_binary_op)(Bignum_sp out, Bignum_sp o1, Bignum_sp o2);

static _clasp_big_binary_op bignum_operations[16] = {
    mpz_b_clr_op,
    mpz_and_op,
    mpz_andc2_op,
    mpz_b_1_op,
    mpz_andc1_op,
    mpz_b_2_op,
    mpz_xor_op,
    mpz_ior_op,
    mpz_nor_op,
    mpz_eqv_op,
    mpz_b_c2_op,
    mpz_orc2_op,
    mpz_b_c1_op,
    mpz_orc1_op,
    mpz_nand_op,
    mpz_b_set_op};

// ----------------------------------------------------------------------

#if 0
    static cl_object
    log_op(cl_narg narg, int op, ecl_va_list ARGS)
    {
	cl_object x, y;
	/* FIXME! This can be optimized */
	x = ecl_va_arg(ARGS);
	if (narg-- == 1) {
            assert_type_integer(x);
	} else {
            do {
                y = ecl_va_arg(ARGS);
                x = ecl_boole(op, x, y);
            } while (--narg);
	}
	return x;
    }
#endif

T_sp clasp_boole(int op, T_sp x, T_sp y) {
  if (x.nilp() || y.nilp()) {
    SIMPLE_ERROR(BF("boole cannot accept nil"));
  }
  if (x.fixnump()) {
    Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
    if (y.fixnump()) { //Fixnum_sp fny = y.asOrNull<Fixnum_O>() ) {
      Fixnum_sp fny = gc::As<Fixnum_sp>(y);
      gctools::Fixnum z = fixnum_operations[op](unbox_fixnum(fnx), unbox_fixnum(fny));
      return make_fixnum(z);
    } else if (Bignum_sp bny = y.asOrNull<Bignum_O>()) {
      Bignum_sp x_copy = _lisp->bigRegister0();
      x_copy->setFixnum(unbox_fixnum(fnx));
      (bignum_operations[op])(x_copy, x_copy, bny);
      return _clasp_big_register_normalize(x_copy);
    } else {
      ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 2, y, cl::_sym_integer);
    }
  } else if (Bignum_sp bnx = x.asOrNull<Bignum_O>()) {
    Bignum_sp x_copy = _lisp->bigRegister0();
    if (y.fixnump()) { // Fixnum_sp fny = y.asOrNull<Fixnum_O>() ) {
      Fixnum_sp fny(gc::As<Fixnum_sp>(y));
      Bignum_sp bny = _lisp->bigRegister1();
      bny->setFixnum(unbox_fixnum(fny));
      (bignum_operations[op])(x_copy, bnx, bny);
      clasp_big_register_free(bny);
    } else if (Bignum_sp bny = y.asOrNull<Bignum_O>()) {
      (bignum_operations[op])(x_copy, x, bny);
    } else {
      ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 2, y, cl::_sym_integer);
    }
    return _clasp_big_register_normalize(x_copy);
  } else {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 1, x, cl::_sym_integer);
  }
  return x;
}

#define ARGS_core_bitArrayOp "(op x y &optional r)"
#define DECL_core_bitArrayOp ""
#define DOCS_core_bitArrayOp "bitArrayOp"
T_sp core_bitArrayOp(T_sp o, T_sp tx, T_sp ty, T_sp tr) {
  _G();
  if (o.nilp()) {
    ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 1, o, cl::_sym_fixnum);
  }
  if (tx.nilp() || !gc::IsA<SimpleBitVector_sp>(tx)) {
    ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 2, tx, cl::_sym_BitVector_O);
  }
  if (ty.nilp() || !gc::IsA<SimpleBitVector_sp>(ty)) {
    ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 3, ty, cl::_sym_BitVector_O);
  }
  int opval = unbox_fixnum(gc::As<Fixnum_sp>(o));
  gctools::Fixnum i, j, n, d;
  SimpleBitVector_sp r0;
  bit_operator op;
  bool replace = false;
  int xi, yi, ri;
  byte *xp, *yp, *rp;
  int xo, yo, ro;
  SimpleBitVector_sp x = tx.asOrNull<SimpleBitVector_O>();
  SimpleBitVector_sp y = ty.asOrNull<SimpleBitVector_O>();
  SimpleBitVector_sp r;
  d = x->dimension();
  xp = x->bytes();
  xo = x->offset();
  if (d != y->dimension())
    goto ERROR;
  yp = y->bytes();
  yo = x->offset();
  if (tr == _lisp->_true())
    tr = x;
  if (tr.notnilp()) {
    r = tr.asOrNull<SimpleBitVector_O>();
    if (!r) {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 4, tr, cl::_sym_BitVector_O);
    }
    if (r->dimension() != d)
      goto ERROR;
    i = (r->bytes() - xp) * 8 + (r->offset() - xo);
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      r = _Nil<SimpleBitVector_O>();
      replace = true;
      goto L1;
    }
    i = (r->bytes() - yp) * 8 + (r->offset() - yo);
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      r = _Nil<SimpleBitVector_O>();
      replace = true;
    }
  }
L1:
  if (!r)
    r = SimpleBitVector_O::create(d);
  rp = r->bytes();
  ro = r->offset();
  op = fixnum_operations[opval];
#define set_high(place, nbits, value) \
  (place) = ((place) & ~(-0400 >> (nbits))) | ((value) & (-0400 >> (nbits)));

#define set_low(place, nbits, value) \
  (place) = ((place) & (-0400 >> (8 - (nbits)))) | ((value) & ~(-0400 >> (8 - (nbits))));

#define extract_byte(integer, pointer, index, offset) \
  (integer) = (pointer)[(index)+1] & 0377;            \
  (integer) = ((pointer)[index] << (offset)) | ((integer) >> (8 - (offset)));

#define store_byte(pointer, index, offset, value)               \
  set_low((pointer)[index], 8 - (offset), (value) >> (offset)); \
  set_high((pointer)[(index)+1], offset, (value) << (8 - (offset)));

  //
  if (xo == 0 && yo == 0 && ro == 0) {
    for (n = d / 8, i = 0; i < n; i++)
      rp[i] = (*op)(xp[i], yp[i]);
    if ((j = d % 8) > 0)
      set_high(rp[n], j, (*op)(xp[n], yp[n]));
    if (!replace)
      return r;
  } else {
    for (n = d / 8, i = 0; i <= n; i++) {
      extract_byte(xi, xp, i, xo);
      extract_byte(yi, yp, i, yo);
      if (i == n) {
        if ((j = d % 8) == 0)
          break;
        extract_byte(ri, rp, n, ro);
        set_high(ri, j, (*op)(xi, yi));
      } else
        ri = (*op)(xi, yi);
      store_byte(rp, i, ro, ri);
    }
    if (!replace)
      return r;
  }
  rp = r0->bytes();
  ro = r0->offset();
  for (n = d / 8, i = 0; i <= n; i++) {
    if (i == n) {
      if ((j = d % 8) == 0)
        break;
      extract_byte(ri, rp, n, ro);
      set_high(ri, j, r->bytes()[n]);
    } else
      ri = r->bytes()[i];
    store_byte(rp, i, ro, ri);
  }
  return r0;
ERROR:
  SIMPLE_ERROR(BF("Illegal arguments for bit-array operation."));
}

/*! Copied from ECL */
T_sp
cl_logbitp(Integer_sp p, Integer_sp x) {
  bool i;
  if (p.fixnump()) {
    cl_index n = clasp_to_size(p);
    if (x.fixnump()) {
      gctools::Fixnum y = clasp_fixnum(x);
      if (n >= FIXNUM_BITS) {
        i = (y < 0);
      } else {
        i = ((y >> n) & 1);
      }
    } else {
      i = mpz_tstbit(gc::As<Bignum_sp>(x)->as_mpz_().get_mpz_t(), n);
    }
  } else {
    IMPLEMENT_MEF(BF("Convert the code below to something Clasp can use"));
#if 0
    assert_type_non_negative_integer(p);
    if (CLASP_FIXNUMP(x))
      i = (clasp_fixnum(x) < 0);
    else
      i = (_clasp_big_sign(x) < 0);
#endif
  }
  return i ? _lisp->_true() : _Nil<T_O>();
}

#if 0
    T_sp
    cl_lognot(T_sp x)
    {
	return @logxor(2,x,clasp_make_fixnum(-1));
    }

    static gctools::Fixnum
    count_bits(T_sp x)
    {
	gctools::Fixnum count;

	switch (clasp_t_of(x)) {
	case t_fixnum: {
            gctools::Fixnum i = clasp_fixnum(x);
            gctools::Fixnum j = (i < 0) ? ~i : i;
            for (count=0 ; j ; j >>= 1)
                if (j & 1) count++;
            break;
	}
	case t_bignum:
            if (_clasp_big_sign(x) >= 0)
                count = mpz_popcount(x->get().get_mpz_t());
            else {
                T_sp z = _clasp_big_register0();
                mpz_com(z->get().get_mpz_t(), x->get().get_mpz_t());
                count = mpz_popcount(z->get().get_mpz_t());
                _clasp_big_register_free(z);
            }
            break;
	default:
            FEwrong_type_only_arg(@[logcount], x, @[integer]);
	}
	return count;
    }

/*
  Left shift if w > 0, right shift if w < 0.
*/
    T_sp
    clasp_ash(T_sp x, gctools::Fixnum w)
    {
	T_sp y;

	if (w == 0)
            return(x);
	y = _clasp_big_register0();
	if (w < 0) {
            cl_index bits = -w;
            if (CLASP_FIXNUMP(x)) {
                /* The result of shifting a number further than the number
                 * of digits it has is unpredictable in C. For instance, GCC
                 * on intel masks out all bits of "bits" beyond the 5 and
                 * it may happen that a shift of 37 becomes a shift of 5.
                 * Furthermore, in general, shifting negative numbers leads
                 * to implementation-specific results :-/
                 */
		gctools::Fixnum y = clasp_fixnum(x);
                if (bits >= FIXNUM_BITS) {
                    y = (y < 0)? -1 : 0;
                } else {
                    y >>= bits;
                }
                return clasp_make_fixnum(y);
            }
            mpz_div_2exp(y->get().get_mpz_t(), x->get().get_mpz_t(), bits);
	} else {
            if (CLASP_FIXNUMP(x)) {
                _clasp_big_set_fixnum(y, clasp_fixnum(x));
                x = y;
            }
            mpz_mul_2exp(y->get().get_mpz_t(), x->get().get_mpz_t(), (unsigned long)w);
	}
	return _clasp_big_register_normalize(y);
    }

    int
    clasp_fixnum_bit_length(gctools::Fixnum i)
    {
	int count;
	if (i < 0)
            i = ~i;
	for (count = 0; i && (count < FIXNUM_BITS); i >>= 1, count++)
            ;
	return count;
    }

    @(defun logior (&rest nums)
      @
      if (narg == 0)
      @(return clasp_make_fixnum(0))
      /* INV: log_op() checks types and outputs first argument as default. */
      @(return log_op(narg, CLASP_BOOLIOR, nums))
      @)

    @(defun logxor (&rest nums)
      @
      if (narg == 0)
      @(return clasp_make_fixnum(0))
      /* INV: log_op() checks types and outputs first argument as default. */
      @(return log_op(narg, CLASP_BOOLXOR, nums))
      @)

    @(defun logand (&rest nums)
      @
      if (narg == 0)
      @(return clasp_make_fixnum(-1))
      /* INV: log_op() checks types and outputs first argument as default. */
      @(return log_op(narg, CLASP_BOOLAND, nums))
      @)

    @(defun logeqv (&rest nums)
      @
      if (narg == 0)
      @(return clasp_make_fixnum(-1))
      /* INV: log_op() checks types and outputs first argument as default. */
      @(return log_op(narg, CLASP_BOOLEQV, nums))
      @)

    T_sp
    cl_lognand(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLNAND, x, y))
            }

    T_sp
    cl_lognor(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLNOR, x, y))
            }

    T_sp
    cl_logandc1(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLANDC1, x, y))
            }

    T_sp
    cl_logandc2(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLANDC2, x, y))
            }

    T_sp
    cl_logorc1(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLORC1, x, y))
            }

    T_sp
    cl_logorc2(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLORC2, x, y))
            }

    static int
    coerce_to_logical_operator(T_sp o)
    {
	gctools::Fixnum op;
	op = clasp_to_fix(o);
	if (op < 0 || op > CLASP_BOOLSET)
            FEerror("~S is an invalid logical operator.", 1, o);
	return op;
    }

    T_sp
    cl_boole(T_sp o, T_sp x, T_sp y)
    {
	/* INV: log_op2() checks types */
	@(return clasp_boole(coerce_to_logical_operator(o), x, y))
            }


    T_sp
    cl_ash(T_sp x, T_sp y)
    {
	T_sp r;
	int sign_x;

        assert_type_integer(x);
	assert_type_integer(y);
	if (CLASP_FIXNUMP(y))
            r = clasp_ash(x, clasp_fixnum(y));
	else {
            /*
              bit position represented by bignum is probably
              out of our address space. So, result is returned
              according to sign of integer.
	    */
            if (CLASP_FIXNUMP(x))
                if (clasp_fixnum_minusp(x))
                    sign_x = -1;
                else if (x == clasp_make_fixnum(0))
                    sign_x = 0;
                else
                    sign_x = 1;
            else
                sign_x = _clasp_big_sign(x);
            if (_clasp_big_sign(y) < 0)
                if (sign_x < 0)
                    r = clasp_make_fixnum(-1);
                else
                    r = clasp_make_fixnum(0);
            else if (sign_x == 0)
                r = x;
            else
                FEerror("Insufficient memory.", 0);
	}
	@(return r)
            }

    T_sp
    cl_logcount(T_sp x)
    {
	@(return clasp_make_fixnum(count_bits(x)))
            }

    cl_index
    clasp_integer_length(T_sp x)
    {
	int count;
	gctools::Fixnum i;

	switch (clasp_t_of(x)) {
	case t_fixnum:
            i = clasp_fixnum(x);
            count = clasp_fixnum_bit_length(i);
            break;
	case t_bignum:
            if (_clasp_big_sign(x) < 0)
                x = cl_lognot(x);
            count = mpz_sizeinbase(x->get().get_mpz_t(), 2);
            break;
	default:
            FEwrong_type_only_arg(@[integer-length], x, @[integer]);
	}
	return count;
    }

    T_sp
    cl_integer_length(T_sp x)
    {
	@(return clasp_make_fixnum(clasp_integer_length(x)))
            }

#endif

#define ARGS_cl_boole "(op arg1 arg2)"
#define DECL_cl_boole ""
#define DOCS_cl_boole "boole"
T_sp cl_boole(T_sp op, T_sp arg1, T_sp arg2) {
  _G();
  if (op.nilp()) {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 1, op, cl::_sym_integer);
  }
  Fixnum_sp fnop = gc::As<Fixnum_sp>(op);
  return clasp_boole(unbox_fixnum(fnop), arg1, arg2);
};

void initialize_bits() {
  SYMBOL_EXPORT_SC_(ClPkg, boole_1);
  SYMBOL_EXPORT_SC_(ClPkg, boole_2);
  SYMBOL_EXPORT_SC_(ClPkg, boole_and);
  SYMBOL_EXPORT_SC_(ClPkg, boole_andc1);
  SYMBOL_EXPORT_SC_(ClPkg, boole_andc2);
  SYMBOL_EXPORT_SC_(ClPkg, boole_c1);
  SYMBOL_EXPORT_SC_(ClPkg, boole_c2);
  SYMBOL_EXPORT_SC_(ClPkg, boole_clr);
  SYMBOL_EXPORT_SC_(ClPkg, boole_eqv);
  SYMBOL_EXPORT_SC_(ClPkg, boole_ior);
  SYMBOL_EXPORT_SC_(ClPkg, boole_nand);
  SYMBOL_EXPORT_SC_(ClPkg, boole_nor);
  SYMBOL_EXPORT_SC_(ClPkg, boole_orc1);
  SYMBOL_EXPORT_SC_(ClPkg, boole_orc2);
  SYMBOL_EXPORT_SC_(ClPkg, boole_set);
  SYMBOL_EXPORT_SC_(ClPkg, boole_xor);

  cl::_sym_boole_1->defconstant(make_fixnum(boole_1));
  cl::_sym_boole_2->defconstant(make_fixnum(boole_2));
  cl::_sym_boole_and->defconstant(make_fixnum(boole_and));
  cl::_sym_boole_andc1->defconstant(make_fixnum(boole_andc1));
  cl::_sym_boole_andc2->defconstant(make_fixnum(boole_andc2));
  cl::_sym_boole_c1->defconstant(make_fixnum(boole_c1));
  cl::_sym_boole_c2->defconstant(make_fixnum(boole_c2));
  cl::_sym_boole_clr->defconstant(make_fixnum(boole_clr));
  cl::_sym_boole_eqv->defconstant(make_fixnum(boole_eqv));
  cl::_sym_boole_ior->defconstant(make_fixnum(boole_ior));
  cl::_sym_boole_nand->defconstant(make_fixnum(boole_nand));
  cl::_sym_boole_nor->defconstant(make_fixnum(boole_nor));
  cl::_sym_boole_orc1->defconstant(make_fixnum(boole_orc1));
  cl::_sym_boole_orc2->defconstant(make_fixnum(boole_orc2));
  cl::_sym_boole_set->defconstant(make_fixnum(boole_set));
  cl::_sym_boole_xor->defconstant(make_fixnum(boole_xor));

  ClDefun(boole);
  CoreDefun(bitArrayOp);
  af_def(ClPkg, "logbitp", &cl_logbitp);
};
};
