#define DEBUG_LEVEL_FULL


#include "foundation.h"
#include "lisp.h"
#include "object.h"
#include "symbolTable.h"
#include "lispMath.h"
#include "bignum.h"
#include "multipleValues.h"
#include "wrappers.h"
namespace core
{

#if 0

    T_mv ceiling1(Real_sp x)
    {
	T_sp v0, v1;
	T_mv v0v1;
	Integer_sp tv1;
	switch (x->number_type()) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = Fixnum_O::create(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rx = x.as<Ratio_O>();
	    v0v1 = ceiling2(rx->numerator(),rx->denomenator());
	    v0 = v0v1;
	    tv1 = v0v1.valueGet(1).as<Integer_O>();
	    v1 = Ratio_O::create(tv1,rx->denomenator());
	    break;
	}
	case number_SingleFloat: {
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float d = sfx->get();
	    float y = ceilf(d);
	    v0 = Integer_O::create(y);
	    v1 = SingleFloat_O::create(d-y);
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
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }





    T_mv ceiling2(Real_sp x, Real_sp y)
    {
	T_sp v0, v1;
	T_mv v0v1;
	Real_sp tv1;
	NumberType ty;
        ty = y->number_type();
	switch(x->number_type())
	{
	case number_Fixnum:
	    switch(ty) {
	    case number_Fixnum: {	/* FIX / FIX */
		Fixnum_sp fnx = x.as<Fixnum_O>();
		Fixnum_sp fny = y.as<Fixnum_O>();
		int a = fnx->get();
		int b = fny->get();
		int q = a / b;
		int r = a % b;
		if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    v0 = Fixnum_O::create(q+1);
		    v1 = Fixnum_O::create(r-b);
		} else {
		    v0 = Fixnum_O::create(q);
		    v1 = Fixnum_O::create(r);
		}
		break;
	    }
	    case number_Bignum: {	/* FIX / BIG */
		/* We must perform the division because there is the
		 * pathological case
		 *	x = MOST_NEGATIVE_FIXNUM
		 *    y = - MOST_NEGATIVE_FIXNUM
		 */
		Bignum_sp bnx = Bignum_O::create(x.as<Fixnum_O>()->get());
		Bignum_sp bny = y.as<Bignum_O>();
		v0v1 = big_ceiling(bnx,bny);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Ratio:		/* FIX / RAT */
	    {	
		Ratio_sp ratio_y = y.as<Ratio_O>();
		v0v1 = ceiling2(contagen_mul(x,ratio_y->denomenator()).as<Real_O>(),ratio_y->numerator());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();;
		v1 = Ratio_O::create(iv1,ratio_y->denomenator());
		break;
	    }
	    case number_SingleFloat: {	/* FIX / SF */
		Fixnum_sp fnx = x.as<Fixnum_O>();
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = fnx->get()/n;
		float q = ceilf(p);
		v0 = Integer_O::create(q);
		v1 = SingleFloat_O::create(p*n - q*n);
		break;
	    }
	    case number_DoubleFloat: {	/* FIX / DF */
		Fixnum_sp fnx = x.as<Fixnum_O>();
		DoubleFloat_sp dfy = y.as<DoubleFloat_O>();
		double n = dfy->get();	
		double p = fnx->get()/n;
		double q = ceil(p);
		v0 = Integer_O::create(q);
		v1 = DoubleFloat_O::create(p*n - q*n);
		break;
	    }
	    case number_LongFloat: {	/* FIX / LF */
		Fixnum_sp fnx = x.as<Fixnum_O>();
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
		Bignum_sp by = Bignum_O::create(y.as<Fixnum_O>()->get());
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
		v0v1 = ceiling2(contagen_mul(x, ry->denomenator()).as<Real_O>(), ry->numerator());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,ry->denomenator());
		break;
	    }
	    case number_SingleFloat: {	/* BIG / SF */
		Bignum_sp bnx = x.as<Bignum_O>();
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = bnx->as_float()/n;
		float q = ceilf(p);
		v0 = Integer_O::create(q);
		v1 = SingleFloat_O::create(p*n - q*n);
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
	    switch(y->number_type()) {
	    case number_Ratio:{		/* RAT / RAT */
		Ratio_sp rx = x.as<Ratio_O>();
		Ratio_sp ry = y.as<Ratio_O>();
		v0v1 = ceiling2(contagen_mul(rx->numerator(),ry->denomenator()).as<Integer_O>(),
				contagen_mul(rx->denomenator(),ry->numerator()).as<Integer_O>());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,contagen_mul(rx->denomenator(),ry->denomenator()).as<Integer_O>());
		break;
	    }
	    default:{		/* RAT / ANY */
		Ratio_sp rx = x.as<Ratio_O>();
		T_mv v0v1 = ceiling2(rx->numerator(),contagen_mul(rx->denomenator(),y).as<Real_O>());
		v0 = v0v1;
		tv1 = v0v1.valueGet(1).as<Real_O>();
		v1 = contagen_div(tv1,rx->denomenator());
	    }
	    }
	    break;
	case number_SingleFloat: {		/* SF / ANY */
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float n = y->as_float();
	    float p = sfx->get()/n;
	    float q = ceilf(p);
	    v0 = Integer_O::create(q);
	    v1 = SingleFloat_O::create(p*n - q*n);
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
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }





    T_mv truncate1(Real_sp x)
    {
	T_sp v0, v1;
	T_mv v0v1;
	Real_sp tv1;
	Integer_sp iv1;
	switch (x->number_type()) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = Fixnum_O::create(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rx = x.as<Ratio_O>();
	    v0v1 = truncate2(rx->numerator(),rx->denomenator());
	    v0 = v0v1;
	    iv1 = v0v1.valueGet(1).as<Integer_O>();
	    v1 = Ratio_O::create(iv1,rx->denomenator());
	    break;
	}
	case number_SingleFloat: {
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float d = sfx->get();
	    float y = d > 0? floorf(d) : ceilf(d);
	    v0 = Integer_O::create(y);
	    v1 = SingleFloat_O::create(d-y);
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
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
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
    switch (x->number_type()) {
    case number_Fixnum:
    case number_Bignum:
	v0 = x;
	v1 = Fixnum_O::create(0);
	break;
    case number_Ratio:{
	Ratio_sp rx = x.as<Ratio_O>();
	v0v1 = floor2(rx->numerator(),rx->denomenator());
	v0 = v0v1;
	iv1 = v0v1.valueGet(1).as<Integer_O>();
	v1 = Ratio_O::create(iv1,rx->denomenator());
	break;
    }
    case number_SingleFloat: {
	SingleFloat_sp sfx = x.as<SingleFloat_O>();
	float d = sfx->get();
	float y = floorf(d);
	v0 = Integer_O::create(y);
	v1 = SingleFloat_O::create(d-y);
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
	WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
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
        ty = y->number_type();
	switch(x->number_type()) {
	case number_Fixnum:
	    switch(ty) {
	    case number_Fixnum: {	/* FIX / FIX */
		Fixnum_sp fnx = x.as<Fixnum_O>();
		Fixnum_sp fny = y.as<Fixnum_O>();
		int a = fnx->get();
		int b = fny->get();
		int q = a / b;
		int r = a % b;
		if ((r^b) < 0 && r) {	/* opposite signs and some remainder */
		    v0 = Fixnum_O::create(q-1);
		    v1 = Fixnum_O::create(r+b);
		} else {
		    v0 = Fixnum_O::create(q);
		    v1 = Fixnum_O::create(r);
		}
		break;
	    }
	    case number_Bignum: {	/* FIX / BIG */
		/* We must perform the division because there is the
		 * pathological case
		 *	x = MOST_NEGATIVE_FIXNUM
		 *    y = - MOST_NEGATIVE_FIXNUM
		 */
		Bignum_sp bnx = Bignum_O::create(x.as<Fixnum_O>()->get());
		Bignum_sp bny = y.as<Bignum_O>();
		v0v1 = big_floor(bnx,bny);
		v0 = v0v1;
		v1 = v0v1.valueGet(1);
		break;
	    }
	    case number_Ratio:		/* FIX / RAT */
	    {	
		Ratio_sp ratio_y = y.as<Ratio_O>();
		v0v1 = floor2(contagen_mul(x,ratio_y->denomenator()).as<Integer_O>(),ratio_y->numerator());
		v0 = v0v1;
		Integer_sp iv1 = v0v1.valueGet(1).as<Integer_O>();;
		v1 = Ratio_O::create(iv1,ratio_y->denomenator());
		break;
	    }
	    case number_SingleFloat: {	/* FIX / SF */
		Fixnum_sp fnx = x.as<Fixnum_O>();
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = fnx->get()/n;
		float q = floorf(p);
		v0 = Integer_O::create(q);
		v1 = SingleFloat_O::create((p-q)*n);
		break;
	    }
	    case number_DoubleFloat: {	/* FIX / DF */
		Fixnum_sp fnx = x.as<Fixnum_O>();
		DoubleFloat_sp sfy = y.as<DoubleFloat_O>();
		double n = sfy->get();	
		double p = fnx->get()/n;
		double q = floor(p);
		v0 = Integer_O::create(q);
		v1 = DoubleFloat_O::create((p-q)*n);
		break;
	    }
	    case number_LongFloat: {	/* FIX / LF */
		Fixnum_sp fnx = x.as<Fixnum_O>();
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
		Bignum_sp by = Bignum_O::create(y.as<Fixnum_O>()->get());
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
		v0v1 = floor2(contagen_mul(x, ry->denomenator()).as<Integer_O>(), ry->numerator());
		v0 = v0v1;
		iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,ry->denomenator());
		break;
	    }
	    case number_SingleFloat: {	/* BIG / SF */
		Bignum_sp bnx = x.as<Bignum_O>();
		SingleFloat_sp sfy = y.as<SingleFloat_O>();
		float n = sfy->get();
		float p = bnx->as_float()/n;
		float q = floorf(p);
		v0 = Integer_O::create(q);
		v1 = SingleFloat_O::create((p-q)*n);
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
	    switch(y->number_type()) {
	    case number_Ratio:{		/* RAT / RAT */
		Ratio_sp rx = x.as<Ratio_O>();
		Ratio_sp ry = y.as<Ratio_O>();
		v0v1 = floor2(contagen_mul(rx->numerator(),ry->denomenator()).as<Integer_O>(),
			      contagen_mul(rx->denomenator(),ry->numerator()).as<Integer_O>());
		v0 = v0v1;
		iv1 = v0v1.valueGet(1).as<Integer_O>();
		v1 = Ratio_O::create(iv1,contagen_mul(rx->denomenator(),ry->denomenator()).as<Integer_O>());
		break;
	    }
	    default:{		/* RAT / ANY */
		Ratio_sp rx = x.as<Ratio_O>();
		v0v1 = floor2(rx->numerator(),contagen_mul(rx->denomenator(),y).as<Real_O>());
		v0 = v0v1;
		tv1 = v0v1.valueGet(1).as<Real_O>();
		v1 = contagen_div(tv1,rx->denomenator());
	    }
	    }
	    break;
	case number_SingleFloat: {		/* SF / ANY */
	    SingleFloat_sp sfx = x.as<SingleFloat_O>();
	    float n = y->as_float();
	    float p = sfx->get()/n;
	    float q = floorf(p);
	    v0 = Integer_O::create(q);
	    v1 = SingleFloat_O::create(p*n - q*n);
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
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }







    
    
#define ARGS_af_truncate "(x &optional y)"
#define DECL_af_truncate ""
#define DOCS_af_truncate "truncate"
    T_mv af_truncate(Real_sp x, Real_sp y)
    {_G();
	if (y.nilp())
	    return truncate1(x);
	return truncate2(x,y);
    };






    
    
#define ARGS_af_ceiling "(x &optional y)"
#define DECL_af_ceiling ""
#define DOCS_af_ceiling "ceiling"
    T_mv af_ceiling(Real_sp x, Real_sp y)
    {_G();
	if ( y.nilp() )
	{
	    return ceiling1(x);
	} else
	{
	    return ceiling2(x, y);
	}
    }







#define ARGS_af_floor "(numb &optional divisor )"
#define DECL_af_floor ""
#define DOCS_af_floor "floor"
T_mv af_floor(Real_sp number, Real_sp divisor )
{_G();
    if ( divisor.nilp() )
    {
	return floor1(number);
    } else
    {
	return floor2(number,divisor);
    }
};






#define ARGS_af_mod "(num div)"
#define DECL_af_mod ""
#define DOCS_af_mod "mod"
    Real_mv af_mod(Real_sp num, Real_sp div)
{_G();
    T_mv floor_mv = af_floor(num,div);
    Real_sp res = floor_mv.valueGet(1).as<Real_O>();
    return(Values(res));
};





    
    
#define ARGS_af_rem "(numb divisor)"
#define DECL_af_rem ""
#define DOCS_af_rem "rem"
T_sp af_rem(Real_sp x, Real_sp y)
    {_G();
	T_mv v0v1 = truncate2(x,y);
	return v0v1.valueGet(1);
    };

#endif
    void initialize_math()
    {_G();
#if 0
	SYMBOL_EXPORT_SC_(ClPkg,floor);
	Defun(floor);
	SYMBOL_EXPORT_SC_(ClPkg,mod);
	Defun(mod);
	SYMBOL_EXPORT_SC_(ClPkg,ceiling);
	Defun(ceiling);
	SYMBOL_EXPORT_SC_(ClPkg,truncate);
	Defun(truncate);
	SYMBOL_EXPORT_SC_(ClPkg,rem);
	Defun(rem);
#endif
    }

};
