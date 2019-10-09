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

// The division is length/BIT_ARRAY_WORD_BITS, but rounding up.
#define DEF_SBV_BIT_OP(name, form)\
  CL_DEFUN SimpleBitVector_sp core__sbv_bit_##name(SimpleBitVector_sp a, SimpleBitVector_sp b,\
                                                   SimpleBitVector_sp r, size_t length) { \
    bit_array_word *ab, *bb, *rb;\
    ab = a->bytes(); bb = b->bytes(); rb = r->bytes();\
    size_t nwords = 1 + ((length - 1) / BIT_ARRAY_WORD_BITS);\
    for (size_t i = 0; i < nwords; ++i) rb[i] = form;\
    return r;\
  }
DEF_SBV_BIT_OP(and, ab[i] & bb[i])
DEF_SBV_BIT_OP(ior, ab[i] | bb[i])
DEF_SBV_BIT_OP(xor, ab[i] ^ bb[i])
DEF_SBV_BIT_OP(nand, ~(ab[i] & bb[i]))
DEF_SBV_BIT_OP(nor, ~(ab[i] | bb[i]))
DEF_SBV_BIT_OP(eqv, ~(ab[i] ^ bb[i]))
DEF_SBV_BIT_OP(andc1, ~(ab[i]) & bb[i])
DEF_SBV_BIT_OP(andc2, ab[i] & ~(bb[i]))
DEF_SBV_BIT_OP(orc1, ~(ab[i]) | bb[i])
DEF_SBV_BIT_OP(orc2, ab[i] | ~(bb[i]))

CL_DEFUN SimpleBitVector_sp core__sbv_bit_not(SimpleBitVector_sp vec, SimpleBitVector_sp res,
                                              size_t length) {
  bit_array_word *vecb, *resb;
  vecb = vec->bytes(); resb = res->bytes();
  size_t nwords = 1 + ((length - 1) / BIT_ARRAY_WORD_BITS);
  for (size_t i = 0; i < nwords; ++i) resb[i] = ~(vecb[i]);
  return res;
}

// Population count for simple bit vector.
CL_DEFUN Integer_sp core__sbv_popcnt(SimpleBitVector_sp vec) {
  ASSERT(sizeof(bit_array_word) == sizeof(unsigned int)); // for popcount. FIXME
  bit_array_word* bytes = vec->bytes();
  size_t len = vec->length();
  size_t nwords = len / BIT_ARRAY_WORD_BITS;
  size_t leftover = len % BIT_ARRAY_WORD_BITS;
  size_t i;
  gctools::Fixnum result = 0;
  for (i = 0; i < nwords; ++i) result += bit_array_word_popcount(bytes[i]);
  if (leftover != 0) {
    // leftover is greater than zero and less than 32,
    // so none of these shifts can overflow.
    byte32_t unshifted_mask = (1 << leftover) - 1;
    byte32_t mask = unshifted_mask << (BIT_ARRAY_WORD_BITS - leftover);
    result += bit_array_word_popcount(bytes[nwords] & mask);
  }
  return make_fixnum(result);
}

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

Integer_sp log_operation_2op(boole_ops operation, Integer_sp first, Integer_sp second) {
  // if the arguments are all fixnum, don't convert everything to mpz, but stay in fixnums
  if (first.fixnump() && second.fixnump()){
    gc::Fixnum first_internal = first.unsafe_fixnum();
    gc::Fixnum second_internal = second.unsafe_fixnum();
    gc::Fixnum result;
    switch (operation) {
    case boole_and:
        result = first_internal & second_internal;
        break;
    case boole_xor:
        result = first_internal ^ second_internal;
        break;
    case boole_ior:
        result = first_internal | second_internal;
        break;
    case boole_eqv:
        result = (~(first_internal ^ second_internal));
        break;
    case boole_andc1:
        result = (~first_internal) & second_internal;
        break;
    case boole_andc2:
        result = first_internal & (~second_internal);
        break;
    case boole_orc1:
        result = (~first_internal) | second_internal;
        break;
    case boole_orc2:
        result = first_internal | (~second_internal);
        break;
    case boole_nand:
        result = ~(first_internal & second_internal);
        break;
    case boole_nor:
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
    case boole_and:
        mpz_and(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case boole_xor:
        mpz_xor(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case boole_ior:
        mpz_ior(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case boole_eqv:
        mpz_xor(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_com(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case boole_andc1:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t());
        mpz_and(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case boole_andc2:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_and(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case boole_orc1:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t());
        mpz_ior(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        break;
    case boole_orc2:
        mpz_com(temp_bignum.get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_ior(result_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case boole_nand:
        mpz_and(temp_bignum.get_mpz_t(), clasp_to_mpz(first).get_mpz_t(), clasp_to_mpz(second).get_mpz_t());
        mpz_com(result_bignum.get_mpz_t(), temp_bignum.get_mpz_t());
        break;
    case boole_nor:
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
  return log_operation_2op(boole_and, first, second);
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logxor_2op");
CL_DEFUN Integer_sp core__logxor_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(boole_xor, first, second);
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logior_2op");
CL_DEFUN Integer_sp core__logior_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(boole_ior, first, second);
}

CL_LAMBDA(first second);
CL_DECLARE();
CL_DOCSTRING("logeqv_2op");
CL_DEFUN Integer_sp core__logeqv_2op(Integer_sp first, Integer_sp second) {
  return log_operation_2op(boole_eqv, first, second);
}

Integer_sp log_operation_rest(List_sp integers, boole_ops operation) {
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
        case boole_and:
            acc_fixnum = acc_fixnum & icur.unsafe_fixnum(); continue;
        case boole_xor:
            acc_fixnum = acc_fixnum ^ icur.unsafe_fixnum(); continue;
        case boole_ior:
            acc_fixnum = acc_fixnum | icur.unsafe_fixnum(); continue;
        case boole_eqv:
            acc_fixnum = (~(acc_fixnum ^ icur.unsafe_fixnum())); continue;
        default:
            SIMPLE_ERROR(BF("Unknown operation in cl__log_operation_rest"));
        }
      } else {
        // need to go bignum
        acc_fixnum_p = false;
        acc_bignum = clasp_to_mpz(Integer_O::create(acc_fixnum));
      }
    }
    // Now either acc_fixnum_p was false and icur is a fixnum, or acc_fixnum_p was true.
    mpz_class temp;
    mpz_class temp1;
    switch (operation) {
    case boole_and:
        mpz_and(temp.get_mpz_t(), acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
        break;
    case boole_xor:
        mpz_xor(temp.get_mpz_t(),  acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
        break;
    case boole_ior:
        mpz_ior(temp.get_mpz_t(),  acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
        break;
    case boole_eqv:
        mpz_xor(temp1.get_mpz_t(), acc_bignum.get_mpz_t(), clasp_to_mpz(icur).get_mpz_t());
        mpz_com(temp.get_mpz_t(), temp1.get_mpz_t());
        break;
    default:
        SIMPLE_ERROR(BF("Unknown operation in cl__log_operation_rest"));
    }
    acc_bignum = temp;
  } // loop over integers
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
    return log_operation_rest(integers, boole_and); 
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logior");
CL_DEFUN Integer_sp cl__logior(List_sp integers) {
  if (integers.nilp())
    return clasp_make_fixnum(0);
  else
    return log_operation_rest(integers, boole_ior); 
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logxor");
CL_DEFUN Integer_sp cl__logxor(List_sp integers) {
  if (integers.nilp())
    return clasp_make_fixnum(0);
  else
    return log_operation_rest(integers, boole_xor);
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_DOCSTRING("logeqv");
CL_DEFUN Integer_sp cl__logeqv(List_sp integers) {
  if (integers.nilp())
    return Integer_O::create((gc::Fixnum) - 1);
  else
    return log_operation_rest(integers, boole_eqv);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logandc1");
CL_DEFUN Integer_sp cl__logandc1(Integer_sp a, Integer_sp b) {
  return log_operation_2op(boole_andc1, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logandc2");
CL_DEFUN Integer_sp cl__logandc2(Integer_sp a, Integer_sp b) {
  return log_operation_2op(boole_andc2, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logorc1");
CL_DEFUN Integer_sp cl__logorc1(Integer_sp a, Integer_sp b) {
  return log_operation_2op(boole_orc1, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("logorc2");
CL_DEFUN Integer_sp cl__logorc2(Integer_sp a, Integer_sp b) {
  return log_operation_2op(boole_orc2, a, b);
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
  return log_operation_2op(boole_nand, a, b);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING("lognor");
CL_DEFUN Integer_sp cl__lognor(Integer_sp a, Integer_sp b) {
  return log_operation_2op(boole_nor, a, b);
};

}; // namespace core
