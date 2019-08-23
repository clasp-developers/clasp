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

#define boolOpsMax 16

static bit_operator fixnum_operations[boolOpsMax] = {
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

static _clasp_big_binary_op bignum_operations[boolOpsMax] = {
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

T_sp clasp_boole(int op, T_sp x, T_sp y) {
  if (x.nilp())
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 2, x, cl::_sym_integer);
  else if (y.nilp())
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 3, y, cl::_sym_integer);
  if ((op < 0) || (op >= boolOpsMax))
    // issue #438
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 1, make_fixnum(op), Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(boolOpsMax-1)));
  if (x.fixnump()) {
    Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
    if (y.fixnump()) {
      Fixnum_sp fny = gc::As<Fixnum_sp>(y);
      gctools::Fixnum z = fixnum_operations[op](unbox_fixnum(fnx), unbox_fixnum(fny));
      return make_fixnum(z);
    } else if (Bignum_sp bny = y.asOrNull<Bignum_O>()) {
      Bignum_sp x_copy = my_thread->bigRegister0();
      x_copy->setFixnum(unbox_fixnum(fnx));
      (bignum_operations[op])(x_copy, x_copy, bny);
      return _clasp_big_register_normalize(x_copy);
    } else {
      ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 3, y, cl::_sym_integer);
    }
  } else if (Bignum_sp bnx = x.asOrNull<Bignum_O>()) {
    Bignum_sp x_copy = my_thread->bigRegister0();
    if (y.fixnump()) {
      Fixnum_sp fny(gc::As<Fixnum_sp>(y));
      Bignum_sp bny = my_thread->bigRegister1();
      bny->setFixnum(unbox_fixnum(fny));
      (bignum_operations[op])(x_copy, bnx, bny);
      clasp_big_register_free(bny);
    } else if (Bignum_sp bny = y.asOrNull<Bignum_O>()) {
      (bignum_operations[op])(x_copy, bnx, bny);
    } else {
      ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 3, y, cl::_sym_integer);
    }
    return _clasp_big_register_normalize(x_copy);
  } else {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 2, x, cl::_sym_integer);
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

class Dispatcher
{
public:
    virtual Array_sp dispatcher (Array_O *target, SimpleBitVector_sp result) {
      //std::cout << "listclass is  " << lisp_classNameAsString(core::instance_class(target->asSmartPtr())) << '\n';
      // Und bist du nicht willig, so brauche ich Gewalt
      // Don't understand why target->create_result_bitarray(result) does not work
      SimpleBitVector_O * sbvptr = dynamic_cast<SimpleBitVector_O *>(target);
      MDArrayBit_O * mdabptr = dynamic_cast<MDArrayBit_O *>(target);
      SimpleMDArrayBit_O * smbabptr = dynamic_cast<SimpleMDArrayBit_O *>(target);
      BitVectorNs_O * bvnptr =  dynamic_cast<BitVectorNs_O *>(target);
      if (sbvptr)
        return result;
      else if (mdabptr)
        return mdabptr->create_result_bitarray(result);
      else if (smbabptr)
        return smbabptr->create_result_bitarray(result);
      else if (bvnptr)
        return bvnptr->create_result_bitarray(result);
      else
        // most likely return a SimpleBitVector_sp but perhaps better than crashing
        return result;
    }
};

// without these c++ seems always to call create_result_bitarray of Array_0
//Array_sp return_correct_bitvector (SimpleBitVector_sp target, SimpleBitVector_sp result) {
//  return (* target).create_result_bitarray(result);
//}


CL_LAMBDA(op x y &optional r);
CL_DECLARE();
CL_DOCSTRING("bitArrayOp");
CL_DEFUN T_sp core__bit_array_op(int opval, Array_sp tx, Array_sp ty, T_sp tr) {
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
  Dispatcher dispatcher;
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
  if (d != (endy - starty)) //(d != array_y->arrayTotalSize()) gives incorrect result for multi-dimensional arrays
    goto ERROR;
  yp = y->bytes();
  yo = starty; // y->offset();
  if (tr == _lisp->_true())
    tr = tx;
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
      return dispatcher.dispatcher(&(* tx),r);
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
      return dispatcher.dispatcher(&(* tx),r);
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
  return dispatcher.dispatcher(&(* tx),r0);
ERROR:
  SIMPLE_ERROR(BF("Illegal arguments for bit-array operation."));
}
#endif
/*! Copied from ECL */
CL_DEFUN T_sp cl__logbitp(Integer_sp p, Integer_sp x) {
  // Arguments and Values:p - a non-negative integer,  x - an integer.
  if (clasp_minusp(p))
      // Expected type for p is (Integer 0 *) or cl::_sym_UnsignedByte
    TYPE_ERROR(p, cl::_sym_UnsignedByte);
  bool i;
  if (p.fixnump()) {
    cl_index n = clasp_to_size(p);
    if (x.fixnump()) {
      gctools::Fixnum y = x.unsafe_fixnum();
      if (n >= FIXNUM_BITS) {
        i = (y < 0);
      } else {
        i = ((y >> n) & 1);
      }
    } else {
      i = mpz_tstbit(gc::As<Bignum_sp>(x)->as_mpz_().get_mpz_t(), n);
    }
  } else {
    if (x.fixnump())
      i = (x.unsafe_fixnum() < 0);
    else
      if (clasp_minusp(x))
        i = true;
      else i = false;
  }
  return i ? _lisp->_true() : _Nil<T_O>();
}

CL_LAMBDA(op arg1 arg2);
CL_DECLARE();
CL_DOCSTRING("boole");
CL_DEFUN T_sp cl__boole(T_sp op, T_sp arg1, T_sp arg2) {
  if (op.nilp()) {
    // the type of this error should be one of values of cl::_sym_boole_1 .. cl::_sym_boole_xor
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_boole, 1, op, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), make_fixnum(boolOpsMax-1)));
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

// logand, logxor, logior, logeqv are all nearly the same, apart from the operation
// logandc1_id until lognor_id as well

typedef enum {
    logand_id = 0,
    logxor_id = 1,
    logior_id = 2,
    logeqv_id = 3,
    logandc1_id = 4,
    logandc2_id =5,
    logorc1_id = 6,
    logorc2_id = 7,
    lognand_id = 8,
    lognor_id = 9
} log_operations;

Integer_sp log_operation_2op(log_operations operation, Integer_sp first, Integer_sp second) {
  // if the arguments are all fixnum, don't convert everything to mpz, but stay in fixnums
  if (first.fixnump() && second.fixnump()){
    gc::Fixnum first_internal = first.unsafe_fixnum();
    gc::Fixnum second_internal = second.unsafe_fixnum();
    gc::Fixnum result;
    switch (operation) {
    case logand_id:
        result = first_internal & second_internal;
        break;
    case logxor_id:
        result = first_internal ^ second_internal;
        break;
    case logior_id:
        result = first_internal | second_internal;
        break;
    case logeqv_id:
        result = (~(first_internal ^ second_internal));
        break;
    case logandc1_id:
        result = (~first_internal) & second_internal;
        break;
    case logandc2_id:
        result = first_internal & (~second_internal);
        break;
    case logorc1_id:
        result = (~first_internal) | second_internal;
        break;
    case logorc2_id:
        result = first_internal | (~second_internal);
        break;
    case lognand_id:
        result = ~(first_internal & second_internal);
        break;
    case lognor_id:
        result = ~(first_internal | second_internal);
        break;
    default:
        SIMPLE_ERROR(BF("Unknown operation in log_operation_2op"));
    }
    return clasp_make_fixnum(result);
  }
  else {
    mpz_class result_bignum;
    mpz_class temp_bignum;
    switch (operation) {
    case logand_id:
        mpz_and(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case logxor_id:
        mpz_xor(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case logior_id:
        mpz_ior(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case logeqv_id:
        mpz_xor(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_com(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case logandc1_id:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t());
        mpz_and(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case logandc2_id:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_and(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case logorc1_id:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t());
        mpz_ior(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case logorc2_id:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_ior(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case lognand_id:
        mpz_and(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_com(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case lognor_id:
        mpz_ior(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_com(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    default:
        SIMPLE_ERROR(BF("Unknown operation in cl__log_operation_rest"));
    }
    return Integer_O::create(result_bignum);
  }
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logand_2op");
CL_DEFUN Integer_sp core__logand_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(logand_id, first, second);
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logxor_2op");
CL_DEFUN Integer_sp core__logxor_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(logxor_id, first, second);
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logior_2op");
CL_DEFUN Integer_sp core__logior_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(logior_id, first, second);
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logeqv_2op");
CL_DEFUN Integer_sp core__logeqv_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(logeqv_id, first, second);
}

Integer_sp log_operation_rest(List_sp integers, log_operations operation) {
  // if the arguments are all fixnum, don't convert everything to mpz, but stay in fixnums
  bool acc_fixnum_p = true;
  Integer_sp first = gc::As<Integer_sp>(oCar(integers));
  gc::Fixnum acc_fixnum;
  mpz_class acc_bignum;
  if (first.fixnump()) {
    acc_fixnum = first.unsafe_fixnum();
  }
  else {
    acc_fixnum_p = false;
    acc_bignum = clasp_to_mpz(first);
  }
  for (auto cur : (List_sp)oCdr(integers)) {
    Integer_sp icur = gc::As<Integer_sp>(oCar(cur));
    if (acc_fixnum_p) {
      if (icur.fixnump()) {
        // we stay in fixnum
        switch (operation) {
        case logand_id:
            acc_fixnum = acc_fixnum & icur.unsafe_fixnum();
            break;
        case logxor_id:
            acc_fixnum = acc_fixnum ^ icur.unsafe_fixnum();
            break;
        case logior_id:
            acc_fixnum = acc_fixnum | icur.unsafe_fixnum();
            break;
        case logeqv_id:
            acc_fixnum = (~(acc_fixnum ^ icur.unsafe_fixnum()));
            break;
        default:
            SIMPLE_ERROR(BF("Unknown operation in cl__log_operation_rest"));
        }
      }
      else {
        // need to go bignum
        acc_fixnum_p = false;
        acc_bignum = clasp_to_mpz(Integer_O::create(acc_fixnum));
        mpz_class temp;
        mpz_class temp1;
        switch (operation) {
        case logand_id:
            mpz_and(temp.get_mpz_t(), acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
            break;
        case logxor_id:
            mpz_xor(temp.get_mpz_t(),  acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
            break;
        case logior_id:
            mpz_ior(temp.get_mpz_t(),  acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
            break;
        case logeqv_id:
            mpz_xor(temp1.get_mpz_t(), acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
            mpz_com(temp.get_mpz_t(), temp1.get_mpz_t());
        default:
            SIMPLE_ERROR(BF("Unknown operation in cl__log_operation_rest"));
        }
        acc_bignum = temp;
      }
    } else {
      mpz_class temp;
      mpz_class temp1;
      switch (operation) {
      case logand_id:
          mpz_and(temp.get_mpz_t(), acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
          break;
      case logxor_id:
          mpz_xor(temp.get_mpz_t(),  acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
          break;
      case logior_id:
          mpz_ior(temp.get_mpz_t(),  acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
          break;
      case logeqv_id:
          mpz_xor(temp1.get_mpz_t(), acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
          mpz_com(temp.get_mpz_t(), temp1.get_mpz_t());
          break;
      default:
          SIMPLE_ERROR(BF("Unknown operation in cl__log_operation_rest"));
      }
      acc_bignum = temp;
    }
  }
  if (acc_fixnum_p)
    return Integer_O::create(acc_fixnum);
  else
    return Integer_O::create(acc_bignum);
}

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logand");
CL_DEFUN Integer_sp cl__logand(List_sp integers) {
  // if the arguments are all fixnum, don't convert everything to mpz, but stay in fixnums
  if (integers.nilp())
    return clasp_make_fixnum(-1);
  else
    return log_operation_rest(integers, logand_id); 
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logior");
CL_DEFUN Integer_sp cl__logior(List_sp integers) {
  if (integers.nilp())
    return clasp_make_fixnum(0);
  else
    return log_operation_rest(integers, logior_id); 
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logxor");
CL_DEFUN Integer_sp cl__logxor(List_sp integers) {
  if (integers.nilp())
    return clasp_make_fixnum(0);
  else
    return log_operation_rest(integers, logxor_id);
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logeqv");
CL_DEFUN Integer_sp cl__logeqv(List_sp integers) {
  if (integers.nilp())
    return Integer_O::create((gc::Fixnum) - 1);
  else
    return log_operation_rest(integers, logeqv_id);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logandc1");
CL_DEFUN Integer_sp cl__logandc1(Integer_sp a, Integer_sp b) {
  return log_operation_2op(logandc1_id, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logandc2");
CL_DEFUN Integer_sp cl__logandc2(Integer_sp a, Integer_sp b) {
  return log_operation_2op(logandc2_id, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logorc1");
CL_DEFUN Integer_sp cl__logorc1(Integer_sp a, Integer_sp b) {
  return log_operation_2op(logorc1_id, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logorc2");
CL_DEFUN Integer_sp cl__logorc2(Integer_sp a, Integer_sp b) {
  return log_operation_2op(logorc2_id, a, b);
};

CL_LAMBDA(a);
CL_DECLARE();
CL_DOCSTRING("lognot");
CL_DEFUN Integer_sp cl__lognot(Integer_sp a) {
  if (a.fixnump()) {
    // in ecl return @logxor(2,x,ecl_make_fixnum(-1))
    return clasp_make_fixnum(a.unsafe_fixnum() ^ -1);   
  }
  else {
    mpz_class za = clasp_to_mpz(a);
    mpz_class cza;
    mpz_com(cza.get_mpz_t(), za.get_mpz_t());
    return Integer_O::create(cza);
  }
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("lognand");
CL_DEFUN Integer_sp cl__lognand(Integer_sp a, Integer_sp b) {
  return log_operation_2op(lognand_id, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("lognor");
CL_DEFUN Integer_sp cl__lognor(Integer_sp a, Integer_sp b) {
  return log_operation_2op(lognor_id, a, b);
};

}; // namespace core
