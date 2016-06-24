/*
    File: lispMath.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispMath.h>
#include <clasp/core/bignum.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/wrappers.h>
namespace core {

#if 0

    T_mv ceiling1(Real_sp x)
    {
	T_sp v0, v1;
	T_mv v0v1;
	Integer_sp tv1;
	switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = make_fixnum(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rx = x.as<Ratio_O>();
	    v0v1 = ceiling2(rx->numerator(),rx->denominator());
	    v0 = v0v1;
	    tv1 = v0v1.valueGet(1).as<Integer_O>();
	    v1 = Ratio_O::create(tv1,rx->denominator());
	    break;
	}
	case number_SingleFloat: {
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float d = sfx->get();
	    float y = ceilf(d);
	    v0 = Integer_O::create(y);
	    v1 = clasp_make_single_float(d-y);
	    break;
	}
	case number_DoubleFloat: {
	    DoubleFloat_sp sfx = x.as<DoubleFloat_O>();
	    double d = sfx->get();
	    double y = ceil(d);
	    v0 = Integer_O::create(y);
	    v1 = DoubleFloat_O::create(d-y);
	    break;
	}
	case number_LongFloat: {
	    LongFloat_sp sfx = x.as<LongFloat_O>();
	    LongFloat d = sfx->get();
	    LongFloat y = ceill(d);
	    v0 = Integer_O::create(y);
	    v1 = LongFloat_O::create(d-y);
	    break;
	}
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }





    T_mv ceiling2(Real_sp x, Real_sp y)
    {
	T_sp v0, v1;
	T_mv v0v1;
	Real_sp tv1;
	NumberType ty;
        ty = clasp_t_of(y);
	switch(clasp_t_of(x))
	{
	case number_Fixnum:
	    switch(ty) {
	    case number_Fixnum: {	/* FIX / FIX */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		Fixnum_sp fny = gc::As<Fixnum_sp>(y);
		int a = fnx->get();
		int b = fny->get();
		int q = a / b;
		int r = a % b;
		if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    v0 = make_fixnum(q+1);
		    v1 = make_fixnum(r-b);
		} else {
		    v0 = make_fixnum(q);
		    v1 = make_fixnum(r);
		}
		break;
	    }
	    case number_Bignum: {	/* FIX / BIG */
		/* We must perform the division because there is the
		 * pathological case
		 *	x = MOST_NEGATIVE_FIXNUM
		 *    y = - MOST_NEGATIVE_FIXNUM
		 */
		Bignum_sp bnx = Bignum_O::create(gc::As<Fixnum_sp>(x)->get());
		Bignum_sp bny = y.as<Bignum_O>();
		v0v1 = big_ceiling(bnx,bny);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Ratio:		/* FIX / RAT */
	    {	
		Ratio_sp ratio_y = y.as<Ratio_O>();
		v0v1 = ceiling2(contagen_mul(x,ratio_y->denominator()).as<Real_O>(),ratio_y->numerator());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();;
		v1 = Ratio_O::create(iv1,ratio_y->denominator());
		break;
	    }
	    case number_SingleFloat: {	/* FIX / SF */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = fnx->get()/n;
		float q = ceilf(p);
		v0 = Integer_O::create(q);
		v1 = clasp_make_single_float(p*n - q*n);
		break;
	    }
	    case number_DoubleFloat: {	/* FIX / DF */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		DoubleFloat_sp dfy = y.as<DoubleFloat_O>();
		double n = dfy->get();	
		double p = fnx->get()/n;
		double q = ceil(p);
		v0 = Integer_O::create(q);
		v1 = DoubleFloat_O::create(p*n - q*n);
		break;
	    }
	    case number_LongFloat: {	/* FIX / LF */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		LongFloat_sp lfy = y.as<LongFloat_O>();
		LongFloat n = lfy->get();	
		LongFloat p = fnx->get()/n;
		LongFloat q = ceill(p);
		v0 = Integer_O::create(q);
		v1 = LongFloat_O::create(p*n - q*n);
		break;
	    }
	    default:
		(void)0; /*Never reached */
	    }
	    break;
	case number_Bignum:
	    switch(ty) {
	    case number_Fixnum: {	/* BIG / FIX */
		Bignum_sp bx = x.as<Bignum_O>();
		Bignum_sp by = Bignum_O::create(gc::As<Fixnum_sp>(y)->get());
		v0v1 = big_ceiling(bx,by);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Bignum: {	/* BIG / BIG */
		Bignum_sp bx = x.as<Bignum_O>();
		Bignum_sp by = y.as<Bignum_O>();
		v0v1 = big_ceiling(bx,by);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Ratio:		/* BIG / RAT */
	    {
		Ratio_sp ry = y.as<Ratio_O>();
		v0v1 = ceiling2(contagen_mul(x, ry->denominator()).as<Real_O>(), ry->numerator());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,ry->denominator());
		break;
	    }
	    case number_SingleFloat: {	/* BIG / SF */
		Bignum_sp bnx = x.as<Bignum_O>();
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = bnx->as_float()/n;
		float q = ceilf(p);
		v0 = Integer_O::create(q);
		v1 = clasp_make_single_float(p*n - q*n);
		break;
	    }
	    case number_DoubleFloat: {	/* BIG / DF */
		Bignum_sp bnx = x.as<Bignum_O>();
		DoubleFloat_sp dfy = y.as<DoubleFloat_O>();
		double n = dfy->get();
		double p = bnx->as_double()/n;
		double q = ceil(p);
		v0 = Integer_O::create(q);
		v1 = DoubleFloat_O::create(p*n - q*n);
		break;
	    }
	    case number_LongFloat: {	/* BIG / LF */
		Bignum_sp bnx = x.as<Bignum_O>();
		LongFloat n = y.as<LongFloat_O>()->get();
		LongFloat p = bnx->as_long_float()/n;
		LongFloat q = ceill(p);
		v0 = Integer_O::create(q);
		v1 = LongFloat_O::create(p*n - q*n);
		break;
	    }
	    default:
		(void)0; /*Never reached */
	    }
	    break;
	case number_Ratio:
	    switch(clasp_t_of(y)) {
	    case number_Ratio:{		/* RAT / RAT */
		Ratio_sp rx = x.as<Ratio_O>();
		Ratio_sp ry = y.as<Ratio_O>();
		v0v1 = ceiling2(contagen_mul(rx->numerator(),ry->denominator()).as<Integer_O>(),
				contagen_mul(rx->denominator(),ry->numerator()).as<Integer_O>());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,contagen_mul(rx->denominator(),ry->denominator()).as<Integer_O>());
		break;
	    }
	    default:{		/* RAT / ANY */
		Ratio_sp rx = x.as<Ratio_O>();
		T_mv v0v1 = ceiling2(rx->numerator(),contagen_mul(rx->denominator(),y).as<Real_O>());
		v0 = v0v1;
		tv1 = v0v1.valueGet(1).as<Real_O>();
		v1 = contagen_div(tv1,rx->denominator());
	    }
	    }
	    break;
	case number_SingleFloat: {		/* SF / ANY */
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float n = y->as_float();
	    float p = sfx->get()/n;
	    float q = ceilf(p);
	    v0 = Integer_O::create(q);
	    v1 = clasp_make_single_float(p*n - q*n);
	    break;
	}
	case number_DoubleFloat: {		/* DF / ANY */
	    DoubleFloat_sp dfx = x.as<DoubleFloat_O>();
	    double n = y->as_double();
	    double p = dfx->get()/n;
	    double q = ceil(p);
	    v0 = Integer_O::create(q);
	    v1 = DoubleFloat_O::create(p*n - q*n);
	    break;
	}
	case number_LongFloat: {		/* LF / ANY */
	    LongFloat_sp dfx = x.as<LongFloat_O>();
	    LongFloat n = y->as_long_float();
	    LongFloat p = dfx->get()/n;
	    LongFloat q = ceil(p);
	    v0 = Integer_O::create(q);
	    v1 = LongFloat_O::create(p*n - q*n);
	    break;
	}
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }





    T_mv truncate1(Real_sp x)
    {
	T_sp v0, v1;
	T_mv v0v1;
	Real_sp tv1;
	Integer_sp iv1;
	switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = make_fixnum(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rx = x.as<Ratio_O>();
	    v0v1 = truncate2(rx->numerator(),rx->denominator());
	    v0 = v0v1;
	    iv1 = v0v1.valueGet(1).as<Integer_O>();
	    v1 = Ratio_O::create(iv1,rx->denominator());
	    break;
	}
	case number_SingleFloat: {
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float d = sfx->get();
	    float y = d > 0? floorf(d) : ceilf(d);
	    v0 = Integer_O::create(y);
	    v1 = clasp_make_single_float(d-y);
	    break;
	}
	case number_DoubleFloat: {
	    DoubleFloat_sp dfx = x.as<DoubleFloat_O>();
	    double d = dfx->get();
	    double y = d > 0? floor(d) : ceil(d);
	    v0 = Integer_O::create(y);
	    v1 = DoubleFloat_O::create(d-y);
	    break;
	}
	case number_LongFloat: {
	    LongFloat_sp lfx = x.as<LongFloat_O>();
	    LongFloat d = lfx->get();
	    LongFloat y = d > 0? floorl(d) : ceill(d);
	    v0 = Integer_O::create(y);
	    v1 = LongFloat_O::create(d-y);
	    break;
	}
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }

    T_mv truncate2(Real_sp x, Real_sp y)
    {
	if ( x->plusp() != y->plusp() )
	    return ceiling2(x, y);
	else
	    return floor2(x,y);
    }



T_mv floor1(Real_sp x)
{
    T_sp v0,v1;
    Integer_sp iv1;
    T_mv v0v1;
    switch (clasp_t_of(x)) {
    case number_Fixnum:
    case number_Bignum:
	v0 = x;
	v1 = make_fixnum(0);
	break;
    case number_Ratio:{
	Ratio_sp rx = x.as<Ratio_O>();
	v0v1 = floor2(rx->numerator(),rx->denominator());
	v0 = v0v1;
	iv1 = v0v1.valueGet(1).as<Integer_O>();
	v1 = Ratio_O::create(iv1,rx->denominator());
	break;
    }
    case number_SingleFloat: {
	SingleFloat_sp sfx = x.as<SingleFloat_O>();
	float d = sfx->get();
	float y = floorf(d);
	v0 = Integer_O::create(y);
	v1 = clasp_make_single_float(d-y);
	break;
    }
    case number_DoubleFloat: {
	DoubleFloat_sp dfx = x.as<DoubleFloat_O>();
	double d = dfx->get();
	double y = floor(d);
	v0 = Integer_O::create(y);
	v1 = DoubleFloat_O::create(d-y);
	break;
    }
    case number_LongFloat: {
	LongFloat_sp dfx = x.as<LongFloat_O>();
	LongFloat d = dfx->get();
	LongFloat y = floorl(d);
	v0 = Integer_O::create(y);
	v1 = LongFloat_O::create(d-y);
	break;
    }
    default:
	QERROR_WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
    }
    return Values(v0,v1);
}





    T_mv floor2(Real_sp x, Real_sp y)
    {
	T_sp v0, v1;
	NumberType ty;
	T_mv v0v1;
	Real_sp tv1;
	Integer_sp iv1;
        ty = clasp_t_of(y);
	switch(clasp_t_of(x)) {
	case number_Fixnum:
	    switch(ty) {
	    case number_Fixnum: {	/* FIX / FIX */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		Fixnum_sp fny = gc::As<Fixnum_sp>(y);
		int a = fnx->get();
		int b = fny->get();
		int q = a / b;
		int r = a % b;
		if ((r^b) < 0 && r) {	/* opposite signs and some remainder */
		    v0 = make_fixnum(q-1);
		    v1 = make_fixnum(r+b);
		} else {
		    v0 = make_fixnum(q);
		    v1 = make_fixnum(r);
		}
		break;
	    }
	    case number_Bignum: {	/* FIX / BIG */
		/* We must perform the division because there is the
		 * pathological case
		 *	x = MOST_NEGATIVE_FIXNUM
		 *    y = - MOST_NEGATIVE_FIXNUM
		 */
		Bignum_sp bnx = Bignum_O::create(gc::As<Fixnum_sp>(x)->get());
		Bignum_sp bny = y.as<Bignum_O>();
		v0v1 = big_floor(bnx,bny);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Ratio:		/* FIX / RAT */
	    {	
		Ratio_sp ratio_y = y.as<Ratio_O>();
		v0v1 = floor2(contagen_mul(x,ratio_y->denominator()).as<Integer_O>(),ratio_y->numerator());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();;
		v1 = Ratio_O::create(iv1,ratio_y->denominator());
		break;
	    }
	    case number_SingleFloat: {	/* FIX / SF */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = fnx->get()/n;
		float q = floorf(p);
		v0 = Integer_O::create(q);
		v1 = clasp_make_single_float((p-q)*n);
		break;
	    }
	    case number_DoubleFloat: {	/* FIX / DF */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		DoubleFloat_sp sfy = y.as<DoubleFloat_O>();
		double n = sfy->get();	
		double p = fnx->get()/n;
		double q = floor(p);
		v0 = Integer_O::create(q);
		v1 = DoubleFloat_O::create((p-q)*n);
		break;
	    }
	    case number_LongFloat: {	/* FIX / LF */
		Fixnum_sp fnx = gc::As<Fixnum_sp>(x);
		LongFloat_sp lfy = y.as<LongFloat_O>();
		LongFloat n = lfy->get();	
		LongFloat p = fnx->get()/n;
		LongFloat q = floorl(p);
		v0 = Integer_O::create(q);
		v1 = LongFloat_O::create((p-q)*n);
		break;
	    }
	    default:
		(void)0; /*Never reached */
	    }
	    break;
	case number_Bignum:
	    switch(ty) {
	    case number_Fixnum: {	/* BIG / FIX */
		Bignum_sp bx = x.as<Bignum_O>();
		Bignum_sp by = Bignum_O::create(gc::As<Fixnum_sp>(y)->get());
		v0v1 = big_floor(bx,by);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Bignum: {	/* BIG / BIG */
		Bignum_sp bx = x.as<Bignum_O>();
		Bignum_sp by = y.as<Bignum_O>();
		v0v1 = big_floor(bx,by);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Ratio:		/* BIG / RAT */
	    {
		Ratio_sp ry = y.as<Ratio_O>();
		v0v1 = floor2(contagen_mul(x, ry->denominator()).as<Integer_O>(), ry->numerator());
		v0 = v0v1;
		iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,ry->denominator());
		break;
	    }
	    case number_SingleFloat: {	/* BIG / SF */
		Bignum_sp bnx = x.as<Bignum_O>();
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = bnx->as_float()/n;
		float q = floorf(p);
		v0 = Integer_O::create(q);
		v1 = clasp_make_single_float((p-q)*n);
		break;
	    }
	    case number_DoubleFloat: {	/* BIG / DF */
		Bignum_sp bnx = x.as<Bignum_O>();
		DoubleFloat_sp dfy = y.as<DoubleFloat_O>();
		double n = dfy->get();
		double p = bnx->as_double()/n;
		double q = floor(p);
		v0 = Integer_O::create(q);
		v1 = DoubleFloat_O::create((p-q)*n);
		break;
	    }
	    case number_LongFloat: {	/* BIG / LF */
		Bignum_sp bnx = x.as<Bignum_O>();
		LongFloat n = y->as_long_float();
		LongFloat p = bnx->as_long_float()/n;
		LongFloat q = floorl(p);
		v0 = Integer_O::create(q);
		v1 = LongFloat_O::create((p-q)*n);
		break;
	    }
	    default:
		(void)0; /*Never reached */
	    }
	    break;
	case number_Ratio:
	    switch(clasp_t_of(y)) {
	    case number_Ratio:{		/* RAT / RAT */
		Ratio_sp rx = x.as<Ratio_O>();
		Ratio_sp ry = y.as<Ratio_O>();
		v0v1 = floor2(contagen_mul(rx->numerator(),ry->denominator()).as<Integer_O>(),
			      contagen_mul(rx->denominator(),ry->numerator()).as<Integer_O>());
		v0 = v0v1;
		iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,contagen_mul(rx->denominator(),ry->denominator()).as<Integer_O>());
		break;
	    }
	    default:{		/* RAT / ANY */
		Ratio_sp rx = x.as<Ratio_O>();
		v0v1 = floor2(rx->numerator(),contagen_mul(rx->denominator(),y).as<Real_O>());
		v0 = v0v1;
		tv1 = v0v1.valueGet(1).as<Real_O>();
		v1 = contagen_div(tv1,rx->denominator());
	    }
	    }
	    break;
	case number_SingleFloat: {		/* SF / ANY */
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float n = y->as_float();
	    float p = sfx->get()/n;
	    float q = floorf(p);
	    v0 = Integer_O::create(q);
	    v1 = clasp_make_single_float(p*n - q*n);
	    break;
	}
	case number_DoubleFloat: {		/* DF / ANY */
	    DoubleFloat_sp dfx = x.as<DoubleFloat_O>();
	    double n = y->as_double();
	    double p = dfx->get()/n;
	    double q = floor(p);
	    v0 = Integer_O::create(q);
	    v1 = DoubleFloat_O::create(p*n - q*n);
	    break;
	}
	case number_LongFloat: {		/* LF / ANY */
	    LongFloat_sp dfx = x.as<LongFloat_O>();
	    LongFloat n = y->as_long_float();
	    LongFloat p = dfx->get()/n;
	    LongFloat q = floorl(p);
	    v0 = Integer_O::create(q);
	    v1 = LongFloat_O::create(p*n - q*n);
	    break;
	}
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }

LAMBDA(x &optional y);
DECLARE();
DOCSTRING("truncate");
CL_DEFUN     T_mv cl__truncate(Real_sp x, Real_sp y)
    {_G();
	if (y.nilp())
	    return truncate1(x);
	return truncate2(x,y);
    };

LAMBDA(x &optional y);
DECLARE();
DOCSTRING("ceiling");
CL_DEFUN     T_mv cl__ceiling(Real_sp x, Real_sp y)
    {_G();
	if ( y.nilp() )
	{
	    return ceiling1(x);
	} else
	{
	    return ceiling2(x, y);
	}
    }

LAMBDA(numb &optional divisor );
DECLARE();
DOCSTRING("floor");
CL_DEFUN T_mv cl__floor(Real_sp number, Real_sp divisor )
{_G();
    if ( divisor.nilp() )
    {
	return floor1(number);
    } else
    {
	return floor2(number,divisor);
    }
};

LAMBDA(num div);
DECLARE();
DOCSTRING("mod");
CL_DEFUN     Real_mv cl__mod(Real_sp num, Real_sp div)
{_G();
    T_mv floor_mv = cl__floor(num,div);
    Real_sp res = floor_mv.valueGet(1).as<Real_O>();
    return(Values(res));
};

LAMBDA(numb divisor);
DECLARE();
DOCSTRING("rem");
CL_DEFUN T_sp cl__rem(Real_sp x, Real_sp y)
    {_G();
	T_mv v0v1 = truncate2(x,y);
	return v0v1.valueGet(1);
    };

#endif
void initialize_math() {
  _G();
#if 0
	SYMBOL_EXPORT_SC_(ClPkg,floor);
	SYMBOL_EXPORT_SC_(ClPkg,mod);
	SYMBOL_EXPORT_SC_(ClPkg,ceiling);
	SYMBOL_EXPORT_SC_(ClPkg,truncate);
	SYMBOL_EXPORT_SC_(ClPkg,rem);
#endif
}
};
