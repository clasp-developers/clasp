/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.
    Copyright (c) 2012, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include "foundation.h"
#include "object.h"
#include "numbers.h"
#include "num_co.h"
#include "character.h"
#include "symbolTable.h"
#include "strWithFillPtr.h"
#include "bignum.h"
#include "wrappers.h"

namespace core {

#define PRINT_BASE brcl_make_fixnum(10)
#define EXPT_RADIX(x) brcl_ash(brcl_make_fixnum(1),x)

    typedef struct {
        Real_sp r;
        Real_sp s;
        Real_sp mm;
        Real_sp mp;
        bool high_ok;
        bool low_ok;
    } float_approx;

    Real_sp times2(Real_sp x)
    {
	return brcl_plus(x, x).as<Real_O>();
    }


    static float_approx* setup(Float_sp number, float_approx *approx)
    {
	Real_mv mv_f = cl_integerDecodeFloat(number);
	Integer_sp f = mv_f.as<Integer_O>();
	Fixnum_sp fne = mv_f.valueGet(1).as<Fixnum_O>();
	Fixnum e = brcl_fixnum(fne), min_e;
	bool limit_f = 0;
	switch (brcl_t_of(number)) {
	case number_SingleFloat:
	    min_e = FLT_MIN_EXP;
	    limit_f = (number.as<SingleFloat_O>()->get() ==
		       ldexpf(FLT_RADIX, FLT_MANT_DIG-1));
	    break;
	case number_DoubleFloat:
	    min_e = DBL_MIN_EXP;
	    limit_f = (number.as<DoubleFloat_O>()->get() ==
		       ldexp(FLT_RADIX, DBL_MANT_DIG-1));
	    break;
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
	    min_e = LDBL_MIN_EXP;
	    limit_f = (number.as<LongFloat_O>()->get() ==
		       ldexpl(FLT_RADIX, LDBL_MANT_DIG-1));
#endif
	default:
	    SIMPLE_ERROR(BF("Illegal type"));
	}
	approx->low_ok = approx->high_ok = brcl_evenp(f);
	if (e > 0) {
	    Fixnum_sp zz(Fixnum_O::create(1));
	    Real_sp be = EXPT_RADIX(e);
	    if (limit_f) {
		Real_sp be1 = brcl_times(be, brcl_make_fixnum(FLT_RADIX)).as<Real_O>();
		approx->r = times2(brcl_times(f, be1).as<Real_O>());
		approx->s = brcl_make_fixnum(FLT_RADIX*2);
		approx->mm = be;
		approx->mp = be1;
	    } else {
		approx->r = times2(brcl_times(f, be).as<Real_O>());
		approx->s = brcl_make_fixnum(2);
		approx->mm = be;
		approx->mp = be;
	    }
	} else if (!limit_f || (e == min_e)) {
	    approx->r = times2(f);
	    approx->s = times2(EXPT_RADIX(-e));
	    approx->mp = brcl_make_fixnum(1);
	    approx->mm = brcl_make_fixnum(1);
	} else {
	    approx->r = times2(brcl_make_fixnum(FLT_RADIX));
	    approx->s = times2(EXPT_RADIX(1-e));
	    approx->mp = brcl_make_fixnum(FLT_RADIX);
	    approx->mm = brcl_make_fixnum(1);
	}
	return approx;
    }

    static Fixnum    scale(float_approx *approx)
    {
        Fixnum k = 0;
        Real_sp x = brcl_plus(approx->r, approx->mp).as<Real_O>();
        int sign;
        do {
	    sign = brcl_number_compare(x, approx->s);
	    if (approx->high_ok) {
		if (sign < 0)
		    break;
	    } else {
		if (sign <= 0)
		    break;
	    }
	    approx->s = brcl_times(approx->s, PRINT_BASE).as<Real_O>();
	    k++;
        } while(1);
        do {
	    x = brcl_times(x, PRINT_BASE).as<Real_O>();
	    sign = brcl_number_compare(x, approx->s);
	    if (approx->high_ok) {
		if (sign >= 0)
		    break;
	    } else {
		if (sign > 0)
		    break;
	    }
	    k--;
	    approx->r = brcl_times(approx->r, PRINT_BASE).as<Real_O>();
	    approx->mm = brcl_times(approx->mm, PRINT_BASE).as<Real_O>();
	    approx->mp = brcl_times(approx->mp, PRINT_BASE).as<Real_O>();
        } while(1);
        return k;
    }

    static StrWithFillPtr_sp
    generate(StrWithFillPtr_sp digits, float_approx *approx)
    {
        Real_sp d, x;
        cl_fixnum digit;
        bool tc1, tc2;
        do {
	    Real_mv mv_d = brcl_truncate2(brcl_times(approx->r, PRINT_BASE).as<Real_O>(), approx->s);
	    d = mv_d;
	    approx->r = mv_d.valueGet(1).as<Real_O>();
	    approx->mp = brcl_times(approx->mp, PRINT_BASE).as<Real_O>();
	    approx->mm = brcl_times(approx->mm, PRINT_BASE).as<Real_O>();
	    tc1 = approx->low_ok?
		brcl_lowereq(approx->r, approx->mm) :
		brcl_lower(approx->r, approx->mm);
	    x = brcl_plus(approx->r, approx->mp).as<Real_O>();
	    tc2 = approx->high_ok?
		brcl_greatereq(x, approx->s) :
		brcl_greater(x, approx->s);
	    if (tc1 || tc2) {
		break;
	    }
	    brcl_string_push_extend(digits, brcl_digit_char(brcl_fixnum(d), 10));
        } while (1);
        if (tc2 && !tc1) {
	    digit = brcl_fixnum(d) + 1;
        } else if (tc1 && !tc2) {
	    digit = brcl_fixnum(d);
        } else if (brcl_lower(times2(approx->r), approx->s)) {
	    digit = brcl_fixnum(d);
        } else {
	    digit = brcl_fixnum(d) + 1;
        }
        brcl_string_push_extend(digits, brcl_digit_char(digit, 10));
        return digits;
    }

    static void
    change_precision(float_approx *approx, Real_sp position, Real_sp relativep)
    {
        cl_fixnum pos;
        if (Null(position))
	    return;
        pos = brcl_fixnum(position);
        if (!Null(relativep)) {
	    Real_sp k = brcl_make_fixnum(0);
	    Real_sp l = brcl_make_fixnum(1);
	    while (brcl_lower(brcl_times(approx->s, l),
			      brcl_plus(approx->r, approx->mp))) {
		k = brcl_one_plus(k).as<Real_O>();
		l = brcl_times(l, PRINT_BASE).as<Real_O>();
	    }
	    position = brcl_minus(k, position).as<Real_O>();
	    {
		Real_sp e1 = cl_expt(PRINT_BASE, position).as<Real_O>();
		Real_sp e2 = brcl_divide(e1, brcl_make_fixnum(2)).as<Real_O>();
		Real_sp e3 = cl_expt(PRINT_BASE, k).as<Real_O>(); 
		if (brcl_greatereq(brcl_plus(approx->r, brcl_times(approx->s, e1)),
				   brcl_times(approx->s, e2)))
		    position = brcl_one_minus(position).as<Real_O>();
	    }
        }
        {
	    Real_sp x = brcl_times(approx->s, cl_expt(PRINT_BASE, position)).as<Real_O>();
	    Real_sp e = brcl_divide(x, brcl_make_fixnum(2)).as<Real_O>();
	    Real_sp low = brcl_max2( approx->mm, e);
	    Real_sp high = brcl_max2( approx->mp, e);
	    if (brcl_lowereq(approx->mm, low)) {
		approx->mm = low;
		approx->low_ok = 1;
	    }
	    if (brcl_lowereq(approx->mp, high)) {
		approx->mp = high;
		approx->high_ok = 1;
	    }
        }
    }




    
    
#define ARGS_core_float_to_digits "(digits number position relativep)"
#define DECL_core_float_to_digits ""
#define DOCS_core_float_to_digits "float_to_digits"
    T_mv core_float_to_digits(StrWithFillPtr_sp digits, Float_sp number, Real_sp position,
			      Real_sp relativep)
    {
        cl_fixnum k;
        float_approx approx[1];
        setup(number, approx);
        change_precision(approx, position, relativep);
        k = scale(approx);
        if (Null(digits))
	    digits = af_make_vector(cl::_sym_BaseChar_O,
				    10,
				    true /* adjustable */,
				    brcl_make_fixnum(0) /* fill pointer */,
				    BRCL_NIL /* displacement */,
				    BRCL_NIL /* displ. offset */,
				    BRCL_NIL /* initial_element */,
				    _Nil<Cons_O>() /* initial_contents */).as<StrWithFillPtr_O>();
        generate(digits, approx);
        return Values(brcl_make_fixnum(k), digits);
    }



    void initialize_float_to_digits()
    {
	SYMBOL_EXPORT_SC_(CorePkg,float_to_digits);
	CoreDefun(float_to_digits);
    }

};
