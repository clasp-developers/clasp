#include <clasp/core/foundation.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bformat.h>
#include <clasp/core/array.h>
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

typedef enum {
    b_clr_op_id=0,
    and_op_id  =1,
    andc2_op_id=2,
    b_1_op_id  =3,
    andc1_op_id=4,
    b_2_op_id  =5,
    xor_op_id  =6,
    ior_op_id  =7,
    nor_op_id  =8,
    eqv_op_id  =9,
    b_c2_op_id =10,
    orc2_op_id =11,
    b_c1_op_id =12,
    orc1_op_id =13,
    nand_op_id =14,
    b_set_op_id =15} bit_op_id;

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
                x = ecl__boole(op, x, y);
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
      Bignum_sp x_copy = my_thread->bigRegister0();
      x_copy->setFixnum(unbox_fixnum(fnx));
      (bignum_operations[op])(x_copy, x_copy, bny);
      return _clasp_big_register_normalize(x_copy);
    } else {
      ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 2, y, cl::_sym_integer);
    }
  } else if (Bignum_sp bnx = x.asOrNull<Bignum_O>()) {
    Bignum_sp x_copy = my_thread->bigRegister0();
    if (y.fixnump()) { // Fixnum_sp fny = y.asOrNull<Fixnum_O>() ) {
      Fixnum_sp fny(gc::As<Fixnum_sp>(y));
      Bignum_sp bny = my_thread->bigRegister1();
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


#if BIT_ARRAY_BYTE_SIZE==8
CL_LAMBDA(op x y &optional r);
CL_DECLARE();
CL_DOCSTRING("bitArrayOp");
CL_DEFUN T_sp core__bit_array_op(T_sp o, T_sp tx, T_sp ty, T_sp tr) {
  int opval = unbox_fixnum(gc::As<Fixnum_sp>(o));
  gctools::Fixnum i, j, n, d;
  SimpleBitVector_sp r0;
  size_t startr0 = 0;
  bit_operator op;
  bool replace = false;
  int xi, yi, ri;
  byte8_t *xp, *yp, *rp;
  int xo, yo, ro;
  AbstractSimpleVector_sp ax;
  size_t startx, endx;
  AbstractSimpleVector_sp ay;
  size_t starty, endy;
  Array_sp array_x = gc::As<Array_sp>(tx);
  array_x->asAbstractSimpleVectorRange(ax, startx, endx);
  SimpleBitVector_sp x = gc::As_unsafe<SimpleBitVector_sp>(ax);
  Array_sp array_y = gc::As<Array_sp>(ty);
  array_y->asAbstractSimpleVectorRange(ay, starty, endy);
  SimpleBitVector_sp y = gc::As_unsafe<SimpleBitVector_sp>(ay);
  SimpleBitVector_sp r;
  size_t startr, endr;
  d = (endx - startx); // x->arrayTotalSize();
  xp = x->bytes();
  xo = startx; // x->offset();
  if (d != array_y->arrayTotalSize())
    goto ERROR;
  yp = y->bytes();
  yo = starty; // y->offset();
  if (tr == _lisp->_true())
    tr = x;
  if (tr.notnilp()) {
    AbstractSimpleVector_sp ar;
    Array_sp array_r = gc::As<Array_sp>(tr);
    array_r->asAbstractSimpleVectorRange(ar, startr, endr);
    r = gc::As_unsafe<SimpleBitVector_sp>(ar);
    if (!r) {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 4, tr, cl::_sym_SimpleBitVector_O);
    }
    if (endr-startr != d) //(r->arrayTotalSize() != d)
      goto ERROR;
//    i = (r->bytes() - xp) * 8 + (r->offset() - xo);
    i = (r->bytes()-xp)*8+startr-xo;
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      startr0 = startr;
      tr = _Nil<T_O>();
      replace = true;
      goto L1;
    }
    // i = (r->bytes() - yp) * 8 + (r->offset() - yo);
    i = (r->bytes() - yp) * 8 + (startr - yo);
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      startr0 = startr;
      tr = _Nil<T_O>();
      replace = true;
    }
  }
L1:
  if (tr.nilp()) {
    startr = 0;
    endr = d;
    r = SimpleBitVector_O::make(d);
  }
  rp = r->bytes();
  ro = startr; // r->offset();
  op = fixnum_operations[opval];
  
#define set_high8(place, nbits, value) \
  (place) = ((place) & ~(-0400 >> (nbits))) | ((value) & (-0400 >> (nbits)));

#define set_low8(place, nbits, value) \
  (place) = ((place) & (-0400 >> (8 - (nbits)))) | ((value) & ~(-0400 >> (8 - (nbits))));
  
#define extract_byte8(integer, pointer, index, offset) \
  (integer) = (pointer)[(index)+1] & 0377;            \
  (integer) = ((pointer)[index] << (offset)) | ((integer) >> (8 - (offset)));

#define store_byte8(pointer, index, offset, value)               \
  set_low8((pointer)[index], 8 - (offset), (value) >> (offset)); \
  set_high8((pointer)[(index)+1], offset, (value) << (8 - (offset)));

  //
  if (xo == 0 && yo == 0 && ro == 0) {
    for (n = d / 8, i = 0; i < n; i++) {
      rp[i] = (*op)(xp[i], yp[i]);
    }
    if ((j = d % 8) > 0) {
      byte rpt = (*op)(xp[n], yp[n]);
      set_high8(rp[n], j, rpt);
    }
    if (!replace)
      return r;
  } else {
    for (n = d / 8, i = 0; i <= n; i++) {
      extract_byte8(xi, xp, i, xo);
      extract_byte8(yi, yp, i, yo);
      if (i == n) {
        if ((j = d % 8) == 0)
          break;
        extract_byte8(ri, rp, n, ro);
        set_high8(ri, j, (*op)(xi, yi));
      } else {
        ri = (*op)(xi, yi);
      }
      store_byte8(rp, i, ro, ri);
    }
    if (!replace)
      return r;
  }
  rp = r0->bytes();
  ro = startr0; // r0->offset();
  for (n = d / 8, i = 0; i <= n; i++) {
    if (i == n) {
      if ((j = d % 8) == 0)
        break;
      extract_byte8(ri, rp, n, ro);
      set_high8(ri, j, r->bytes()[n]);
    } else
      ri = r->bytes()[i];
    store_byte8(rp, i, ro, ri);
  }
  return r0;
ERROR:
  SIMPLE_ERROR(BF("Illegal arguments for bit-array operation."));
}
#endif


#if BIT_ARRAY_BYTE_SIZE==32
#define mask32 0xFFFFFFFF00000000
template <typename Place, typename Nbits, typename Value>
inline void set_high32(Place& place, Nbits nbits, Value value)
{
  (place) = ((place) & ~(mask32 >> (nbits))) | ((value) & (mask32>> (nbits)));
}

template <typename Place, typename Nbits, typename Value>
inline void set_low32(Place& place, Nbits nbits, Value value) {
  (place) = ((place) & (mask32 >> (32 - (nbits)))) | ((value) & ~(mask32 >> (32 - (nbits))));
}

template <typename Integer, typename Pointer, typename Index, typename Offset>
inline void extract_byte32(Integer& integer, Pointer pointer, Index index, Offset offset) {
  (integer) = (pointer)[(index)+1] & (~mask32);
  (integer) = ((pointer)[index] << (offset)) | ((integer) >> (32 - (offset)));
}

template <typename Pointer, typename Index, typename Offset, typename Value>
inline void store_byte32(Pointer pointer, Index index, Offset offset, Value value) {
  set_low32((pointer)[index], 32 - (offset), (value) >> (offset));
  set_high32((pointer)[(index)+1], offset, (value) << (32 - (offset)));
}

//#define TEMPLATE_BIT_ARRAY_OP 1
#ifndef TEMPLATE_BIT_ARRAY_OP

CL_LAMBDA(op x y &optional r);
CL_DECLARE();
CL_DOCSTRING("bitArrayOp");
CL_DEFUN T_sp core__bit_array_op(int opval, T_sp tx, T_sp ty, T_sp tr) {
  gctools::Fixnum i, j, n, d;
  SimpleBitVector_sp r0;
  size_t startr0 = 0;
  bit_operator op;
  bool replace = false;
  byte64_t xi, yi, ri;
  byte32_t *xp, *yp, *rp;
  byte64_t xo, yo, ro;
  AbstractSimpleVector_sp ax;
  size_t startx, endx;
  AbstractSimpleVector_sp ay;
  size_t starty, endy;
  Array_sp array_x = gc::As<Array_sp>(tx);
  array_x->asAbstractSimpleVectorRange(ax, startx, endx);
  SimpleBitVector_sp x = gc::As_unsafe<SimpleBitVector_sp>(ax);
  Array_sp array_y = gc::As<Array_sp>(ty);
  array_y->asAbstractSimpleVectorRange(ay, starty, endy);
  SimpleBitVector_sp y = gc::As_unsafe<SimpleBitVector_sp>(ay);
  SimpleBitVector_sp r;
  size_t startr, endr;
  d = (endx - startx); // x->arrayTotalSize();
  xp = x->bytes();
  xo = startx; // x->offset();
  if (d != array_y->arrayTotalSize())
    goto ERROR;
  yp = y->bytes();
  yo = starty; // y->offset();
  if (tr == _lisp->_true())
    tr = x;
  if (tr.notnilp()) {
    AbstractSimpleVector_sp ar;
    Array_sp array_r = gc::As<Array_sp>(tr);
    array_r->asAbstractSimpleVectorRange(ar, startr, endr);
    r = gc::As_unsafe<SimpleBitVector_sp>(ar);
    if (!r) {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 4, tr, cl::_sym_SimpleBitVector_O);
    }
    if (endr-startr != d) //(r->arrayTotalSize() != d)
      goto ERROR;
    i = (r->bytes()-xp)*32+startr-xo;
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      startr0 = startr;
      tr = _Nil<T_O>();
      replace = true;
      goto L1;
    }
    i = (r->bytes() - yp) * 32 + (startr - yo);
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      startr0 = startr;
      tr = _Nil<T_O>();
      replace = true;
    }
  }
L1:
  if (tr.nilp()) {
    startr = 0;
    endr = d;
    r = SimpleBitVector_O::make(d);
  }
  rp = r->bytes();
  ro = startr; // r->offset();
  op = fixnum_operations[opval];

  //
  if (xo == 0 && yo == 0 && ro == 0) {
    for (n = d / 32, i = 0; i < n; i++) {
      rp[i] = (*op)(xp[i], yp[i]);
    }
    if ((j = d % 32) > 0) {
      byte64_t rpt = (*op)(xp[n], yp[n]);
      set_high32(rp[n], j, rpt);
    }
    if (!replace)
      return r;
  } else {
    for (n = d / 32, i = 0; i <= n; i++) {
      extract_byte32(xi, xp, i, xo);
      extract_byte32(yi, yp, i, yo);
      if (i == n) {
        if ((j = d % 32) == 0)
          break;
        extract_byte32(ri, rp, n, ro);
        set_high32(ri, j, (*op)(xi, yi));
      } else {
        ri = (*op)(xi, yi);
      }
      store_byte32(rp, i, ro, ri);
    }
    if (!replace)
      return r;
  }
  rp = r0->bytes();
  ro = startr0; // r0->offset();
  for (n = d / 32, i = 0; i <= n; i++) {
    if (i == n) {
      if ((j = d % 32) == 0)
        break;
      extract_byte32(ri, rp, n, ro);
      set_high32(ri, j, r->bytes()[n]);
    } else
      ri = r->bytes()[i];
    store_byte32(rp, i, ro, ri);
  }
  return r0;
ERROR:
  SIMPLE_ERROR(BF("Illegal arguments for bit-array operation."));
}

CL_DEFUN T_sp core__bit_array_op_b_clr_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(b_clr_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_and_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(and_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_andc2_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(andc2_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_1_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(b_1_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_andc1_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(andc1_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_2_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(b_2_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_xor_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(xor_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_ior_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(ior_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_nor_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(nor_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_eqv_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(eqv_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_c2_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(b_c2_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_orc2_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(orc2_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_c1_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(b_c1_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_orc1_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(orc1_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_nand_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(nand_op_id,tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_set_op(T_sp tx, T_sp ty, T_sp tr) { return core__bit_array_op(b_set_op_id,tx,ty,tr); };

#else
template <int OP> struct do_bit_op {};
template <> struct do_bit_op<b_clr_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return b_clr_op(i,j); };};
template <> struct do_bit_op<and_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return and_op(i,j);};};
template <> struct do_bit_op<andc2_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return andc2_op(i,j);};};
template <> struct do_bit_op<b_1_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return b_1_op(i,j);};};
template <> struct do_bit_op<andc1_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return andc1_op(i,j);};};
template <> struct do_bit_op<b_2_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return b_2_op(i,j);};};
template <> struct do_bit_op<xor_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return xor_op(i,j);};};
template <> struct do_bit_op<ior_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return ior_op(i,j);};};
template <> struct do_bit_op<nor_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return nor_op(i,j);};};
template <> struct do_bit_op<eqv_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return eqv_op(i,j);};};
template <> struct do_bit_op<b_c2_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return b_c2_op(i,j);};};
template <> struct do_bit_op<orc2_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return orc2_op(i,j);};};
template <> struct do_bit_op<b_c1_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return b_c1_op(i,j);};};
template <> struct do_bit_op<orc1_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return orc1_op(i,j);};};
template <> struct do_bit_op<nand_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return nand_op(i,j);};};
template <> struct do_bit_op<b_set_op_id> {static gc::Fixnum do_it(gc::Fixnum i, gc::Fixnum j) { return b_set_op(i,j);};};

template <int OP>
T_sp template_bit_array_op(T_sp tx, T_sp ty, T_sp tr) {
  gctools::Fixnum i, j, n, d;
  SimpleBitVector_sp r0;
  size_t startr0 = 0;
  bit_operator op;
  bool replace = false;
  byte64_t xi, yi, ri;
  byte32_t *xp, *yp, *rp;
  byte64_t xo, yo, ro;
  AbstractSimpleVector_sp ax;
  size_t startx, endx;
  AbstractSimpleVector_sp ay;
  size_t starty, endy;
  Array_sp array_x = gc::As<Array_sp>(tx);
  array_x->asAbstractSimpleVectorRange(ax, startx, endx);
  SimpleBitVector_sp x = gc::As_unsafe<SimpleBitVector_sp>(ax);
  Array_sp array_y = gc::As<Array_sp>(ty);
  array_y->asAbstractSimpleVectorRange(ay, starty, endy);
  SimpleBitVector_sp y = gc::As_unsafe<SimpleBitVector_sp>(ay);
  SimpleBitVector_sp r;
  size_t startr, endr;
  d = (endx - startx); // x->arrayTotalSize();
  xp = x->bytes();
  xo = startx; // x->offset();
  if (d != array_y->arrayTotalSize())
    goto ERROR;
  yp = y->bytes();
  yo = starty; // y->offset();
  if (tr == _lisp->_true())
    tr = x;
  if (tr.notnilp()) {
    AbstractSimpleVector_sp ar;
    Array_sp array_r = gc::As<Array_sp>(tr);
    array_r->asAbstractSimpleVectorRange(ar, startr, endr);
    r = gc::As_unsafe<SimpleBitVector_sp>(ar);
    if (!r) {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_bitArrayOp, 4, tr, cl::_sym_SimpleBitVector_O);
    }
    if (endr-startr != d) //(r->arrayTotalSize() != d)
      goto ERROR;
    i = (r->bytes()-xp)*32+startr-xo;
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      startr0 = startr;
      tr = _Nil<T_O>();
      replace = true;
      goto L1;
    }
    i = (r->bytes() - yp) * 32 + (startr - yo);
    if ((i > 0 && i < d) || (i < 0 && -i < d)) {
      r0 = r;
      startr0 = startr;
      tr = _Nil<T_O>();
      replace = true;
    }
  }
L1:
  if (tr.nilp()) {
    startr = 0;
    endr = d;
    r = SimpleBitVector_O::make(d);
  }
  rp = r->bytes();
  ro = startr; // r->offset();
  if (xo == 0 && yo == 0 && ro == 0) {
    for (n = d / 32, i = 0; i < n; i++) {
      rp[i] = do_bit_op<OP>::do_it(xp[i], yp[i]);
    }
    if ((j = d % 32) > 0) {
      byte64_t rpt = do_bit_op<OP>::do_it(xp[n], yp[n]);
      set_high32(rp[n], j, rpt);
    }
    if (!replace)
      return r;
  } else {
    for (n = d / 32, i = 0; i <= n; i++) {
      extract_byte32(xi, xp, i, xo);
      extract_byte32(yi, yp, i, yo);
      if (i == n) {
        if ((j = d % 32) == 0)
          break;
        extract_byte32(ri, rp, n, ro);
        set_high32(ri, j, do_bit_op<OP>::do_it(xi, yi));
      } else {
        ri = do_bit_op<OP>::do_it(xi, yi);
      }
      store_byte32(rp, i, ro, ri);
    }
    if (!replace)
      return r;
  }
  rp = r0->bytes();
  ro = startr0; // r0->offset();
  for (n = d / 32, i = 0; i <= n; i++) {
    if (i == n) {
      if ((j = d % 32) == 0)
        break;
      extract_byte32(ri, rp, n, ro);
      set_high32(ri, j, r->bytes()[n]);
    } else
      ri = r->bytes()[i];
    store_byte32(rp, i, ro, ri);
  }
  return r0;
ERROR:
  SIMPLE_ERROR(BF("Illegal arguments for bit-array operation."));
}

CL_DEFUN T_sp core__bit_array_op_b_clr_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<b_clr_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_and_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<and_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_andc2_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<andc2_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_1_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<b_1_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_andc1_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<andc1_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_2_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<b_2_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_xor_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<xor_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_ior_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<ior_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_nor_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<nor_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_eqv_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<eqv_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_c2_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<b_c2_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_orc2_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<orc2_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_c1_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<b_c1_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_orc1_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<orc1_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_nand_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<nand_op_id>(tx,ty,tr); };
CL_DEFUN T_sp core__bit_array_op_b_set_op(T_sp tx, T_sp ty, T_sp tr) { return template_bit_array_op<b_set_op_id>(tx,ty,tr); };

#endif
#endif
/*! Copied from ECL */
CL_DEFUN T_sp cl__logbitp(Integer_sp p, Integer_sp x) {
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
    IMPLEMENT_MEF("Convert the code below to something Clasp can use");
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
    cl__logandc1(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLANDC1, x, y))
            }

    T_sp
    cl__logandc2(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLANDC2, x, y))
            }

    T_sp
    cl__logorc1(T_sp x, T_sp y)
    {
	@(return clasp_boole(CLASP_BOOLORC1, x, y))
            }

    T_sp
    cl__logorc2(T_sp x, T_sp y)
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
    cl__boole(T_sp o, T_sp x, T_sp y)
    {
	/* INV: log_op2() checks types */
	@(return clasp_boole(coerce_to_logical_operator(o), x, y))
            }


    T_sp
    cl__ash(T_sp x, T_sp y)
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

CL_LAMBDA(op arg1 arg2);
CL_DECLARE();
CL_DOCSTRING("boole");
CL_DEFUN T_sp cl__boole(T_sp op, T_sp arg1, T_sp arg2) {
  if (op.nilp()) {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 1, op, cl::_sym_integer);
  }
  Fixnum_sp fnop = gc::As<Fixnum_sp>(op);
  return clasp_boole(unbox_fixnum(fnop), arg1, arg2);
};

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


void initialize_bits() {
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

//  af_def(ClPkg, "logbitp", &cl_logbitp);
};
};
