/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    big.c -- Bignum routines.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define ECL_INCLUDE_MATH_H
#include <string.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

/* 
 * Using GMP multiple precision integers.
 */

void
_ecl_big_register_free(cl_object x)
{
        return;
        /* We only need to free the integer when it gets too large */
        if (ECL_BIGNUM_DIM(x) > 3 * ECL_BIG_REGISTER_SIZE) {
                mpz_realloc2(x->big.big_num, ECL_BIG_REGISTER_SIZE * GMP_LIMB_BITS);
        }
}

static cl_object
_ecl_alloc_compact_bignum(cl_index limbs)
{
#if 1
        cl_index bytes = limbs * sizeof(mp_limb_t);
        cl_object new_big = ecl_alloc_compact_object(t_bignum, bytes);
        ECL_BIGNUM_LIMBS(new_big) = ECL_COMPACT_OBJECT_EXTRA(new_big);
        ECL_BIGNUM_SIZE(new_big) = 0;
        ECL_BIGNUM_DIM(new_big) = limbs;
#else
        cl_object new_big = ecl_alloc_object(t_bignum);
        mpz_init2(new_big->big.big_num, limbs * GMP_LIMB_BITS);
#endif
        return new_big;
}

static cl_object
_ecl_big_copy(cl_object old)
{
        cl_fixnum size = ECL_BIGNUM_SIZE(old);
        cl_index dim = (size < 0)? (-size) : size;
        cl_index bytes = dim * sizeof(mp_limb_t);
        cl_object new_big = _ecl_alloc_compact_bignum(dim);
        ECL_BIGNUM_SIZE(new_big) = size;
        memcpy(ECL_BIGNUM_LIMBS(new_big), ECL_BIGNUM_LIMBS(old), bytes);
        return new_big;
}

cl_object
_ecl_big_register_copy(cl_object old)
{
        cl_object new_big = _ecl_big_copy(old);
        _ecl_big_register_free(old);
	return new_big;
}

static cl_object
big_normalize(cl_object x)
{
	int s = ECL_BIGNUM_SIZE(x);
	if (s == 0)
                return(ecl_make_fixnum(0));
	if (s == 1) {
                mp_limb_t y = ECL_BIGNUM_LIMBS(x)[0];
                if (y <= MOST_POSITIVE_FIXNUM)
                        return ecl_make_fixnum(y);
	} else if (s == -1) {
                mp_limb_t y = ECL_BIGNUM_LIMBS(x)[0];
                if (y <= -MOST_NEGATIVE_FIXNUM)
                        return ecl_make_fixnum(-y);
	}
	return x;
}

cl_object
_ecl_big_register_normalize(cl_object x)
{
	int s = ECL_BIGNUM_SIZE(x);
	if (s == 0)
                return(ecl_make_fixnum(0));
	if (s == 1) {
                mp_limb_t y = ECL_BIGNUM_LIMBS(x)[0];
                if (y <= MOST_POSITIVE_FIXNUM)
                        return ecl_make_fixnum(y);
	} else if (s == -1) {
                mp_limb_t y = ECL_BIGNUM_LIMBS(x)[0];
                if (y <= -MOST_NEGATIVE_FIXNUM)
                        return ecl_make_fixnum(-y);
	}
	return _ecl_big_register_copy(x);
}

#if GMP_LIMB_BITS >= FIXNUM_BITS
static const int limbs_per_fixnum = 1;
#else
static const int limbs_per_fixnum = (FIXNUM_BITS + GMP_LIMB_BITS - 1) / GMP_LIMB_BITS;
#endif

#define ECL_BIGNUM_ABS_SIZE(x) \
	(ECL_BIGNUM_SIZE(x)<0? -ECL_BIGNUM_SIZE(x) : ECL_BIGNUM_SIZE(x))

cl_object
_ecl_fix_times_fix(cl_fixnum x, cl_fixnum y)
{
#if ECL_LONG_BITS >= FIXNUM_BITS
        ECL_WITH_TEMP_BIGNUM(z,4);
        _ecl_big_set_si(z, x);
        _ecl_big_mul_si(z, z, y);
#else
        ECL_WITH_TEMP_BIGNUM(z,4);
        ECL_WITH_TEMP_BIGNUM(w,4);
        _ecl_big_set_fixnum(z, x);
        _ecl_big_set_fixnum(w, y);
        _ecl_big_mul(z, z, w);
#endif
        {
                cl_object y = big_normalize(z);
                if (y == z) y = _ecl_big_copy(z);
                return y;
        }
}

cl_object
_ecl_big_times_big(cl_object a, cl_object b)
{
        cl_index size_a = ECL_BIGNUM_ABS_SIZE(a);
        cl_index size_b = ECL_BIGNUM_ABS_SIZE(b);
	cl_index size = size_a + size_b;
        cl_object z = _ecl_alloc_compact_bignum(size);
        _ecl_big_mul(z, a, b);
	return z;
}


cl_object
_ecl_big_times_fix(cl_object b, cl_fixnum i)
{
        cl_index size;
	cl_object z;

        if (i == 0)
                return ecl_make_fixnum(0);
	if (i == 1)
		return b;
        size = ECL_BIGNUM_ABS_SIZE(b);
        size += limbs_per_fixnum;
        z = _ecl_alloc_compact_bignum(size);
#if ECL_LONG_BITS >= FIXNUM_BITS
        _ecl_big_mul_si(z, b, i);
#else
	{
		ECL_WITH_TEMP_BIGNUM(w,4);
		_ecl_big_set_fixnum(w, i);
		_ecl_big_mul(z, b, w);
	}
#endif
        return z;
}

cl_object
_ecl_big_plus_fix(cl_object a, cl_fixnum b)
{
	ECL_WITH_TEMP_BIGNUM(big_b, 2);
	_ecl_big_set_fixnum(big_b, b);
	return _ecl_big_plus_big(a, big_b);
}

cl_object
_ecl_big_plus_big(cl_object a, cl_object b)
{
        cl_index size_a = ECL_BIGNUM_ABS_SIZE(a);
        cl_index size_b = ECL_BIGNUM_ABS_SIZE(b);
        cl_index size_z = (size_a < size_b)? (size_b + 1) : (size_a + 1);
        cl_object z = _ecl_alloc_compact_bignum(size_z);
        _ecl_big_add(z, a, b);
        return big_normalize(z);
}

cl_object
_ecl_big_minus_big(cl_object a, cl_object b)
{
        cl_index size_a = ECL_BIGNUM_ABS_SIZE(a);
        cl_index size_b = ECL_BIGNUM_ABS_SIZE(b);
        cl_index size_z = (size_a < size_b)? (size_b + 1) : (size_a + 1);
        cl_object z = _ecl_alloc_compact_bignum(size_z);
        mpz_sub(z->big.big_num, a->big.big_num, b->big.big_num);
        return big_normalize(z);
}

cl_object
_ecl_fix_minus_big(cl_fixnum a, cl_object b)
{
	cl_index size_b = ECL_BIGNUM_ABS_SIZE(b);
        cl_index size_z = size_b + limbs_per_fixnum;
        cl_object z = _ecl_alloc_compact_bignum(size_z);
        _ecl_big_set_fixnum(z, a);
        mpz_sub(z->big.big_num, z->big.big_num, b->big.big_num);
        return big_normalize(z);
}

cl_object
_ecl_big_negate(cl_object a)
{
        cl_index size_a = ECL_BIGNUM_ABS_SIZE(a);
        cl_object z = _ecl_alloc_compact_bignum(size_a);
        mpz_neg(z->big.big_num, a->big.big_num);
        return big_normalize(z);
}

cl_object
_ecl_big_divided_by_big(cl_object a, cl_object b)
{
        cl_object z;
        cl_index size_a = ECL_BIGNUM_ABS_SIZE(a);
        cl_index size_b = ECL_BIGNUM_ABS_SIZE(b);
        cl_fixnum size_z = size_a - size_b + 1;
        if (size_z <= 0) size_z = 1;
        z = _ecl_alloc_compact_bignum(size_z);
        mpz_tdiv_q(z->big.big_num,a->big.big_num,b->big.big_num);
        return big_normalize(z);
}

cl_object
_ecl_big_gcd(cl_object a, cl_object b)
{
        cl_object z = _ecl_big_register0();
        mpz_gcd(z->big.big_num, a->big.big_num, b->big.big_num);
        return _ecl_big_register_normalize(z);
}

cl_object
_ecl_big_divided_by_fix(cl_object x, cl_fixnum y)
{
        ECL_WITH_TEMP_BIGNUM(by, 2);
        _ecl_big_set_fixnum(by, y);
        return _ecl_big_divided_by_big(x, by);
}

cl_object
_ecl_big_ceiling(cl_object a, cl_object b, cl_object *pr)
{
        cl_object q = _ecl_big_register0();
        cl_object r = _ecl_big_register1();
        mpz_cdiv_qr(q->big.big_num, r->big.big_num, a->big.big_num, b->big.big_num);
        *pr = _ecl_big_register_normalize(r);
        return _ecl_big_register_normalize(q);
}

cl_object
_ecl_big_floor(cl_object a, cl_object b, cl_object *pr)
{
        cl_object q = _ecl_big_register0();
        cl_object r = _ecl_big_register1();
        mpz_fdiv_qr(q->big.big_num, r->big.big_num, a->big.big_num, b->big.big_num);
        *pr = _ecl_big_register_normalize(r);
        return _ecl_big_register_normalize(q);
}

cl_object
_ecl_fix_divided_by_big(cl_fixnum x, cl_object y)
{
        ECL_WITH_TEMP_BIGNUM(bx, 2);
        _ecl_big_set_fixnum(bx, x);
        return _ecl_big_divided_by_big(bx, y);
}

static void *
mp_alloc(size_t size)
{
        return ecl_alloc_atomic_align(size, sizeof(mp_limb_t));
}

static void *
mp_realloc(void *ptr, size_t osize, size_t nsize)
{
	mp_limb_t *p = ecl_alloc_atomic_align(nsize, sizeof(mp_limb_t));
	memcpy(p, ptr, (osize < nsize)? osize : nsize);
        ecl_dealloc(ptr);
	return p;
}

static void
mp_free(void *ptr, size_t size)
{
        ecl_dealloc(ptr);
}

cl_fixnum
fixint(cl_object x)
{
        if (ECL_FIXNUMP(x))
                return ecl_fixnum(x);
        if (ECL_BIGNUMP(x)) {
                if (mpz_fits_slong_p(x->big.big_num)) {
                        return mpz_get_si(x->big.big_num);
                }
        }
	FEwrong_type_argument(@[fixnum], x);
}

cl_index
fixnnint(cl_object x)
{
        if (ECL_FIXNUMP(x)) {
                cl_fixnum i = ecl_fixnum(x);
                if (i >= 0)
                        return i;
        } else if (ECL_BIGNUMP(x)) {
                if (mpz_fits_ulong_p(x->big.big_num)) {
                        return mpz_get_ui(x->big.big_num);
                }
        }
	FEwrong_type_argument(cl_list(3, @'integer', ecl_make_fixnum(0),
				      ecl_make_fixnum(MOST_POSITIVE_FIXNUM)),
			      x);
}

#undef _ecl_big_set_fixnum
#undef _ecl_big_set_index
#if ECL_LONG_BITS >= FIXNUM_BITS
cl_object
_ecl_big_set_fixnum(cl_object x, cl_fixnum f)
{
        mpz_set_si((x)->big.big_num,(f));
        return x;
}

cl_object
_ecl_big_set_index(cl_object x, cl_index f)
{
        mpz_set_ui((x)->big.big_num,(f));
        return x;
}

cl_fixnum
_ecl_big_get_fixnum(cl_object x)
{
	return mpz_get_si((x)->big.big_num);
}

cl_index
_ecl_big_get_index(cl_object x)
{
	return mpz_get_ui((x)->big.big_num);
}
#elif GMP_LIMB_BITS >= FIXNUM_BITS
cl_object
_ecl_big_set_fixnum(cl_object x, cl_fixnum f)
{
        if (f == 0) {
                mpz_set_si(x->big.big_num, 0);
        } else if (f > 0) {
                ECL_BIGNUM_SIZE(x) = 1;
                ECL_BIGNUM_LIMBS(x)[0] = f;
        } else if (f < 0) {
                ECL_BIGNUM_SIZE(x) = -1;
                ECL_BIGNUM_LIMBS(x)[0] = -f;
        }
}

cl_object
_ecl_big_set_index(cl_object x, cl_index f)
{
        if (f == 0) {
                mpz_set_si(x->big.big_num, 0);
        } else if (f > 0) {
                ECL_BIGNUM_SIZE(x) = 1;
                ECL_BIGNUM_LIMBS(x)[0] = f;
        }
}

cl_fixnum
_ecl_big_get_fixnum(cl_object x)
{
	/* INV: x is a bignum and thus size != 0 */
	cl_fixnum output = ECL_BIGNUM_LIMBS(x)[0];
	return (ECL_BIGNUM_SIZE(x) > 0) ? output : -output;
}

cl_index
_ecl_big_get_index(cl_object x)
{
	/* INV: x is a bignum and thus size != 0 */
	cl_index output = ECL_BIGNUM_LIMBS(x)[0];
	return (ECL_BIGNUM_SIZE(x) > 0)? output : ~(output - 1);
}

bool
_ecl_big_fits_in_index(cl_object x)
{
	/* INV: x is a bignum and thus size != 0 */
	return (ECL_BIGNUM_SIZE(x) ^ 1) == 0;
}
#else
# error "ECL cannot build with GMP when both long and mp_limb_t are smaller than cl_fixnum"
#endif /* FIXNUM_BITS > GMP_LIMB_BITS, ECL_LONG_BITS */

#ifdef ECL_LONG_FLOAT
long double
_ecl_big_to_long_double(cl_object o)
{
        long double output = 0;
        int i, l = mpz_size(o->big.big_num), exp = 0;
        for (i = 0; i < l; i++) {
                output += ldexpl(mpz_getlimbn(o->big.big_num, i), exp);
                exp += GMP_LIMB_BITS;
        }
        return (mpz_sgn(o->big.big_num) < 0)? -output : output;
}
#endif

static void
mpz_ior_op(cl_object out, cl_object i, cl_object j)
{
	mpz_ior(out->big.big_num, i->big.big_num, j->big.big_num);
}

static void
mpz_xor_op(cl_object out, cl_object i, cl_object j)
{
	mpz_xor(out->big.big_num, i->big.big_num, j->big.big_num);
}

static void
mpz_and_op(cl_object out, cl_object i, cl_object j)
{
	mpz_and(out->big.big_num, i->big.big_num, j->big.big_num);
}

static void
mpz_eqv_op(cl_object out, cl_object i, cl_object j)
{
	mpz_xor(out->big.big_num, i->big.big_num, j->big.big_num);
	mpz_com(out->big.big_num, out->big.big_num);
}

static void
mpz_nand_op(cl_object out, cl_object i, cl_object j)
{
	mpz_and(out->big.big_num, i->big.big_num, j->big.big_num);
	mpz_com(out->big.big_num, out->big.big_num);
}

static void
mpz_nor_op(cl_object out, cl_object i, cl_object j)
{
	mpz_ior(out->big.big_num, i->big.big_num, j->big.big_num);
	mpz_com(out->big.big_num, out->big.big_num);
}

static void
mpz_andc1_op(cl_object out, cl_object i, cl_object j)
{
	mpz_com(out->big.big_num, i->big.big_num);
	mpz_and(out->big.big_num, out->big.big_num, j->big.big_num);
}

static void
mpz_orc1_op(cl_object out, cl_object i, cl_object j)
{
	mpz_com(out->big.big_num, i->big.big_num);
	mpz_ior(out->big.big_num, out->big.big_num, j->big.big_num);
}

static void
mpz_andc2_op(cl_object out, cl_object i, cl_object j)
{
	/* (i & ~j) = ~((~i) | j) */
	mpz_orc1_op(out, i, j);
	mpz_com(out->big.big_num, out->big.big_num);
}

static void
mpz_orc2_op(cl_object out, cl_object i, cl_object j)
{
	/* (i | ~j) = ~((~i) & j) */
	mpz_andc1_op(out, i, j);
	mpz_com(out->big.big_num, out->big.big_num);
}

static void
mpz_b_clr_op(cl_object out, cl_object i, cl_object j)
{
	mpz_set_si(out->big.big_num, 0);
}

static void
mpz_b_set_op(cl_object o, cl_object i, cl_object j)
{
	mpz_set_si(o->big.big_num, -1);
}

static void
mpz_b_1_op(cl_object out, cl_object i, cl_object j)
{
        if (i != out)
                mpz_set(out->big.big_num, i->big.big_num);
}

static void
mpz_b_2_op(cl_object out, cl_object i, cl_object j)
{
	mpz_set(out->big.big_num, j->big.big_num);
}

static void
mpz_b_c1_op(cl_object out, cl_object i, cl_object j)
{
	mpz_com(out->big.big_num, i->big.big_num);
}

static void
mpz_b_c2_op(cl_object out, cl_object i, cl_object j)
{
	mpz_com(out->big.big_num, j->big.big_num);
}

static _ecl_big_binary_op bignum_operations[16] = {
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

_ecl_big_binary_op
_ecl_big_boole_operator(int op)
{
        unlikely_if((op < 0) || (op >= 16)) {
                ecl_internal_error("_ecl_big_boole_operator passed "
                                   "an invalid operator");
        }
        return bignum_operations[op];
}

void
init_big()
{
        if (ecl_option_values[ECL_OPT_SET_GMP_MEMORY_FUNCTIONS])
                mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
}
