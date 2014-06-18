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

#include <ecl/ecl.h>
#include <stdlib.h>
#include <ecl/internal.h>

/*
 * BIT OPERATIONS FOR FIXNUMS
 */

static cl_fixnum
ior_op(cl_fixnum i, cl_fixnum j)
{
	return(i | j);
}

static cl_fixnum
xor_op(cl_fixnum i, cl_fixnum j)
{
	return(i ^ j);
}

static cl_fixnum
and_op(cl_fixnum i, cl_fixnum j)
{
	return(i & j);
}

static cl_fixnum
eqv_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i ^ j));
}

static cl_fixnum
nand_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i & j));
}

static cl_fixnum
nor_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i | j));
}

static cl_fixnum
andc1_op(cl_fixnum i, cl_fixnum j)
{
	return((~i) & j);
}

static cl_fixnum
andc2_op(cl_fixnum i, cl_fixnum j)
{
	return(i & (~j));
}

static cl_fixnum
orc1_op(cl_fixnum i, cl_fixnum j)
{
	return((~i) | j);
}

static cl_fixnum
orc2_op(cl_fixnum i, cl_fixnum j)
{
	return(i | (~j));
}

static cl_fixnum
b_clr_op(cl_fixnum i, cl_fixnum j)
{
	return(0);
}

static cl_fixnum
b_set_op(cl_fixnum i, cl_fixnum j)
{
	return(-1);
}

static cl_fixnum
b_1_op(cl_fixnum i, cl_fixnum j)
{
	return(i);
}

static cl_fixnum
b_2_op(cl_fixnum i, cl_fixnum j)
{
	return(j);
}

static cl_fixnum
b_c1_op(cl_fixnum i, cl_fixnum j)
{
	return(~i);
}

static cl_fixnum
b_c2_op(cl_fixnum i, cl_fixnum j)
{
	return(~j);
}

typedef cl_fixnum (*bit_operator)(cl_fixnum, cl_fixnum);

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

cl_object
ecl_boole(int op, cl_object x, cl_object y)
{
	switch (ecl_t_of(x)) {
	case t_fixnum:
		switch (ecl_t_of(y)) {
		case t_fixnum: {
			cl_fixnum z = fixnum_operations[op](ecl_fixnum(x), ecl_fixnum(y));
			return ecl_make_fixnum(z);
		}
		case t_bignum: {
                        cl_object x_copy = _ecl_big_register0();
                        _ecl_big_set_fixnum(x_copy, ecl_fixnum(x));
                        (_ecl_big_boole_operator(op))(x_copy, x_copy, y);
                        return _ecl_big_register_normalize(x_copy);
		}
		default:
                        FEwrong_type_nth_arg(@[boole], 2, y, @[integer]);
		}
		break;
	case t_bignum: {
                cl_object x_copy = _ecl_big_register0();
		switch (ecl_t_of(y)) {
		case t_fixnum: {
			cl_object z = _ecl_big_register1();
                        _ecl_big_set_fixnum(z,ecl_fixnum(y));
                        (_ecl_big_boole_operator(op))(x_copy, x, z);
			_ecl_big_register_free(z);
			break;
		}
		case t_bignum:
			(_ecl_big_boole_operator(op))(x_copy, x, y);
			break;
		default:
                        FEwrong_type_nth_arg(@[boole], 2, y, @[integer]);
		}
                return _ecl_big_register_normalize(x_copy);
	}
	default:
                FEwrong_type_nth_arg(@[boole], 1, x, @[integer]);
	}
	return x;
}

cl_object
cl_lognot(cl_object x)
{
	return @logxor(2,x,ecl_make_fixnum(-1));
}

static cl_fixnum
count_bits(cl_object x)
{
	cl_fixnum count;

	switch (ecl_t_of(x)) {
	case t_fixnum: {
		cl_fixnum i = ecl_fixnum(x);
		cl_fixnum j = (i < 0) ? ~i : i;
		for (count=0 ; j ; j >>= 1)
			if (j & 1) count++;
		break;
	}
	case t_bignum:
		if (_ecl_big_sign(x) >= 0)
			count = mpz_popcount(x->big.big_num);
		else {
			cl_object z = _ecl_big_register0();
			mpz_com(z->big.big_num, x->big.big_num);
			count = mpz_popcount(z->big.big_num);
			_ecl_big_register_free(z);
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
cl_object
ecl_ash(cl_object x, cl_fixnum w)
{
	cl_object y;

	if (w == 0)
		return(x);
	y = _ecl_big_register0();
	if (w < 0) {
		cl_index bits = -w;
		if (ECL_FIXNUMP(x)) {
			/* The result of shifting a number further than the number
			 * of digits it has is unpredictable in C. For instance, GCC
			 * on intel masks out all bits of "bits" beyond the 5 and
			 * it may happen that a shift of 37 becomes a shift of 5.
			 * Furthermore, in general, shifting negative numbers leads
			 * to implementation-specific results :-/
			 */
			cl_fixnum y = ecl_fixnum(x);
			if (bits >= FIXNUM_BITS) {
				y = (y < 0)? -1 : 0;
			} else {
				y >>= bits;
			}
			return ecl_make_fixnum(y);
		}
		mpz_div_2exp(y->big.big_num, x->big.big_num, bits);
	} else {
		if (ECL_FIXNUMP(x)) {
			_ecl_big_set_fixnum(y, ecl_fixnum(x));
			x = y;
		}
		mpz_mul_2exp(y->big.big_num, x->big.big_num, (unsigned long)w);
	}
	return _ecl_big_register_normalize(y);
}

int
ecl_fixnum_bit_length(cl_fixnum i)
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
		@(return ecl_make_fixnum(0))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLIOR, nums))
@)

@(defun logxor (&rest nums)
@
	if (narg == 0)
		@(return ecl_make_fixnum(0))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLXOR, nums))
@)

@(defun logand (&rest nums)
@
	if (narg == 0)
		@(return ecl_make_fixnum(-1))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLAND, nums))
@)

@(defun logeqv (&rest nums)
@
	if (narg == 0)
		@(return ecl_make_fixnum(-1))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLEQV, nums))
@)

cl_object
cl_lognand(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLNAND, x, y))
}

cl_object
cl_lognor(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLNOR, x, y))
}

cl_object
cl_logandc1(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLANDC1, x, y))
}

cl_object
cl_logandc2(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLANDC2, x, y))
}

cl_object
cl_logorc1(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLORC1, x, y))
}

cl_object
cl_logorc2(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLORC2, x, y))
}

static int
coerce_to_logical_operator(cl_object o)
{
	cl_fixnum op;
	op = ecl_to_fix(o);
	if (op < 0 || op > ECL_BOOLSET)
		FEerror("~S is an invalid logical operator.", 1, o);
	return op;
}

cl_object
cl_boole(cl_object o, cl_object x, cl_object y)
{
	/* INV: log_op2() checks types */
	@(return ecl_boole(coerce_to_logical_operator(o), x, y))
}

cl_object
cl_logbitp(cl_object p, cl_object x)
{
	bool i;

	assert_type_integer(x);
	if (ECL_FIXNUMP(p)) {
		cl_index n = ecl_to_size(p);
		if (ECL_FIXNUMP(x)) {
			cl_fixnum y = ecl_fixnum(x);
			if (n >= FIXNUM_BITS) {
				i = (y < 0);
			} else {
				i = ((y >> n) & 1);
			}
		} else {
			i = mpz_tstbit(x->big.big_num, n);
		}
	} else {
		assert_type_non_negative_integer(p);
		if (ECL_FIXNUMP(x))
			i = (ecl_fixnum(x) < 0);
		else
			i = (_ecl_big_sign(x) < 0);
	}
	@(return (i ? ECL_T : ECL_NIL))
}

cl_object
cl_ash(cl_object x, cl_object y)
{
	cl_object r;
	int sign_x;

        assert_type_integer(x);
	assert_type_integer(y);
	if (ECL_FIXNUMP(y))
	  r = ecl_ash(x, ecl_fixnum(y));
	else {
	  /*
	    bit position represented by bignum is probably
	    out of our address space. So, result is returned
	    according to sign of integer.
	    */
	  if (ECL_FIXNUMP(x))
	    if (ecl_fixnum_minusp(x))
	      sign_x = -1;
	    else if (x == ecl_make_fixnum(0))
	      sign_x = 0;
	    else
	      sign_x = 1;
	  else
	    sign_x = _ecl_big_sign(x);
	  if (_ecl_big_sign(y) < 0)
	    if (sign_x < 0)
	      r = ecl_make_fixnum(-1);
	    else
	      r = ecl_make_fixnum(0);
	  else if (sign_x == 0)
	    r = x;
	  else
	    FEerror("Insufficient memory.", 0);
	}
	@(return r)
}

cl_object
cl_logcount(cl_object x)
{
	@(return ecl_make_fixnum(count_bits(x)))
}

cl_index
ecl_integer_length(cl_object x)
{
	int count;
	cl_fixnum i;

	switch (ecl_t_of(x)) {
	case t_fixnum:
		i = ecl_fixnum(x);
		count = ecl_fixnum_bit_length(i);
		break;
	case t_bignum:
		if (_ecl_big_sign(x) < 0)
			x = cl_lognot(x);
		count = mpz_sizeinbase(x->big.big_num, 2);
		break;
	default:
                FEwrong_type_only_arg(@[integer-length], x, @[integer]);
	}
	return count;
}

cl_object
cl_integer_length(cl_object x)
{
	@(return ecl_make_fixnum(ecl_integer_length(x)))
}

cl_object
si_bit_array_op(cl_object o, cl_object x, cl_object y, cl_object r)
{
	cl_fixnum i, j, n, d;
	cl_object r0;
	bit_operator op;
	bool replace = FALSE;
	int xi, yi, ri;
	byte *xp, *yp, *rp;
	int xo, yo, ro;

	if (ecl_t_of(x) == t_bitvector) {
		d = x->vector.dim;
		xp = x->vector.self.bit;
		xo = x->vector.offset;
		if (ecl_t_of(y) != t_bitvector)
			goto ERROR;
		if (d != y->vector.dim)
			goto ERROR;
		yp = y->vector.self.bit;
		yo = y->vector.offset;
		if (r == ECL_T)
			r = x;
		if (r != ECL_NIL) {
			if (ecl_t_of(r) != t_bitvector)
				goto ERROR;
			if (r->vector.dim != d)
				goto ERROR;
			i = (r->vector.self.bit - xp)*8 + (r->vector.offset - xo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = ECL_NIL;
				replace = TRUE;
				goto L1;
			}
			i = (r->vector.self.bit - yp)*8 + (r->vector.offset - yo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = ECL_NIL;
				replace = TRUE;
			}
		}
	L1:
		if (Null(r)) {
			r = ecl_alloc_simple_vector(d, ecl_aet_bit);
		}
	} else {
		if (ecl_t_of(x) != t_array)
			goto ERROR;
		if ((cl_elttype)x->array.elttype != ecl_aet_bit)
			goto ERROR;
		d = x->array.dim;
		xp = x->vector.self.bit;
		xo = x->vector.offset;
		if (ecl_t_of(y) != t_array)
			goto ERROR;
		if ((cl_elttype)y->array.elttype != ecl_aet_bit)
			goto ERROR;
		if (x->array.rank != y->array.rank)
			goto ERROR;
		yp = y->vector.self.bit;
		yo = y->vector.offset;
		for (i = 0;  i < x->array.rank;  i++)
			if (x->array.dims[i] != y->array.dims[i])
				goto ERROR;
		if (r == ECL_T)
			r = x;
		if (r != ECL_NIL) {
			if (ecl_t_of(r) != t_array)
				goto ERROR;
			if ((cl_elttype)r->array.elttype != ecl_aet_bit)
				goto ERROR;
			if (r->array.rank != x->array.rank)
				goto ERROR;
			for (i = 0;  i < x->array.rank;  i++)
				if (r->array.dims[i] != x->array.dims[i])
					goto ERROR;
			i = (r->vector.self.bit - xp)*8 + (r->vector.offset - xo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = ECL_NIL;
				replace = TRUE;
				goto L2;
			} 
			i = (r->vector.self.bit - yp)*8 + (r->vector.offset - yo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = ECL_NIL;
				replace = TRUE;
			}
		}
	L2:
		if (Null(r)) {
		  r = ecl_alloc_object(t_array);
		  r->array.self.t = NULL;
		  r->array.displaced = ECL_NIL;
		  r->array.rank = x->array.rank;
		  r->array.dims = x->array.dims;
		  r->array.elttype = ecl_aet_bit;
		  r->array.dim = x->array.dim;
		  r->array.flags = 0; /* no fill pointer, not adjustable */
		  ecl_array_allocself(r);
		}
	}
	rp = r->vector.self.bit;
	ro = r->vector.offset;
	op = fixnum_operations[coerce_to_logical_operator(o)];

#define	set_high(place, nbits, value) \
	(place)=((place)&~(-0400>>(nbits)))|((value)&(-0400>>(nbits)))

#define	set_low(place, nbits, value) \
	(place)=((place)&(-0400>>(8-(nbits))))|((value)&~(-0400>>(8-(nbits))))

#define	extract_byte(integer, pointer, index, offset) \
	(integer) = (pointer)[(index)+1] & 0377; \
	(integer) = ((pointer)[index]<<(offset))|((integer)>>(8-(offset)))

#define	store_byte(pointer, index, offset, value) \
	set_low((pointer)[index], 8-(offset), (value)>>(offset)); \
	set_high((pointer)[(index)+1], offset, (value)<<(8-(offset)))

	if (xo == 0 && yo == 0 && ro == 0) {
		for (n = d/8, i = 0;  i < n;  i++)
			rp[i] = (*op)(xp[i], yp[i]);
		if ((j = d%8) > 0)
			set_high(rp[n], j, (*op)(xp[n], yp[n]));
		if (!replace)
			@(return r)
	} else {
		for (n = d/8, i = 0;  i <= n;  i++) {
			extract_byte(xi, xp, i, xo);
			extract_byte(yi, yp, i, yo);
			if (i == n) {
				if ((j = d%8) == 0)
					break;
				extract_byte(ri, rp, n, ro);
				set_high(ri, j, (*op)(xi, yi));
			} else
				ri = (*op)(xi, yi);
			store_byte(rp, i, ro, ri);
		}
		if (!replace)
			@(return r)
	}
	rp = r0->vector.self.bit;
	ro = r0->vector.offset;
	for (n = d/8, i = 0;  i <= n;  i++) {
		if (i == n) {
			if ((j = d%8) == 0)
				break;
			extract_byte(ri, rp, n, ro);
			set_high(ri, j, r->vector.self.bit[n]);
		} else
			ri = r->vector.self.bit[i];
		store_byte(rp, i, ro, ri);
	}
	@(return r0)
ERROR:
	FEerror("Illegal arguments for bit-array operation.", 0);
}
