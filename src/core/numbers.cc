/*
    File: numbers.cc
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
#define	DEBUG_LEVEL_FULL

//#include "clasp_gmpxx.h"
#include "boost/format.hpp"
#include "common.h"
#include "numbers.h"
#include "multipleValues.h"
#include "symbolTable.h"
#include "symbol.h"
#include "bignum.h"
#include "evaluator.h"
#include "conditions.h"
#include "hashTable.h"
#include "mathDispatch.h"
#include "num_arith.h"
#include "math_fenv.h"


#include "wrappers.h"


namespace core
{

    SYMBOL_EXPORT_SC_(ClPkg,divisionByZero);
    SYMBOL_EXPORT_SC_(ClPkg,floatingPointInvalidOperation);
    SYMBOL_EXPORT_SC_(ClPkg,floatingPointOverflow);
    SYMBOL_EXPORT_SC_(ClPkg,floatingPointUnderflow);
    SYMBOL_EXPORT_SC_(ClPkg,floatingPointInexact);
    SYMBOL_EXPORT_SC_(ClPkg,arithmeticError);




    
    void brcl_deliver_fpe(int status)
    {
	int bits = status & _lisp->trapFpeBits();
	if ( bits ) {
	    T_sp condition;
	    if ( bits & FE_DIVBYZERO ) 		condition = cl::_sym_divisionByZero;
	    else if ( bits & FE_INVALID ) 	condition = cl::_sym_floatingPointInvalidOperation;
	    else if ( bits & FE_OVERFLOW ) 	condition = cl::_sym_floatingPointOverflow;
	    else if ( bits & FE_UNDERFLOW ) 	condition = cl::_sym_floatingPointUnderflow;
	    else if ( bits & FE_INEXACT ) 	condition = cl::_sym_floatingPointInexact;
	    else				condition = cl::_sym_arithmeticError;
	    eval::funcall(cl::_sym_error,condition);
	}
    }



#define ARGS_cl_zerop "(num)"
#define DECL_cl_zerop ""
#define DOCS_cl_zerop "fixnum_number_of_bits"
    bool cl_zerop(T_sp num)
    {_G();
	if ( num.nilp() ) {
	    WRONG_TYPE_ARG(num,cl::_sym_Number_O);
	} else if ( num.tagged_fixnump() ) {
	    return ( num.fixnum() == 0 );
	} else if ( Number_sp nnum = num.asOrNull<Number_O>() ) {
	    return nnum->zerop();
	}
	return false;
    }


#define ARGS_af_fixnum_number_of_bits "()"
#define DECL_af_fixnum_number_of_bits ""
#define DOCS_af_fixnum_number_of_bits "fixnum_number_of_bits"
    Fixnum_mv af_fixnum_number_of_bits()
    {_G();
	int num = Fixnum_O::number_of_bits();
	return(Values(Fixnum_O::create(num)));
    };






    Real_sp brcl_max2(Real_sp x, Real_sp y)
    {_G();
	Real_sp max = x;
	if ( brcl_number_compare(max,y)<0) max = y;
	return max;
    }


    Real_sp brcl_min2(Real_sp x, Real_sp y)
    {_G();
	Real_sp min = x;
	if ( brcl_number_compare(min,y)>0) min = y;
	return min;
    }


#define ARGS_cl_min "(min &rest nums)"
#define DECL_cl_min ""
#define DOCS_cl_min "min"
    Real_sp cl_min(Real_sp min, Cons_sp nums)
    {_G();
	/* INV: type check occurs in brcl_number_compare() for the rest of
	   numbers, but for the first argument it happens in brcl_zerop(). */
	if (!nums.nilp()) {
	    do {
		Real_sp numi = oCar(nums).as<Real_O>();
		nums = cCdr(nums);
		min = brcl_min2(min,numi);
		if (brcl_number_compare(min, numi) > 0)
		    min = numi;
	    } while (nums.notnilp());
	}
	return min;
    }



#define ARGS_cl_max "(max &rest nums)"
#define DECL_cl_max ""
#define DOCS_cl_max "max"
    Real_sp cl_max(Real_sp max, Cons_sp nums)
    {_G();
	/* INV: type check occurs in brcl_number_compare() for the rest of
	   numbers, but for the first argument it happens in brcl_zerop(). */
	if (!nums.nilp()) {
	    do {
		Real_sp numi = oCar(nums).as<Real_O>();
		nums = cCdr(nums);
		max = brcl_max2(max,numi);
	    } while (nums.notnilp());
	}
	return max;
    }



#define ARGS_cl_logand "(&rest integers)"
#define DECL_cl_logand ""
#define DOCS_cl_logand "logand"
    Integer_sp cl_logand(Cons_sp integers)
    {_G();
	mpz_class acc = oCar(integers).as<Integer_O>()->as_mpz();
	for ( Cons_sp cur = cCdr(integers); cur.notnilp(); cur=cCdr(cur) )
	{
	    Integer_sp icur = oCar(cur).as<Integer_O>();
	    mpz_class temp;
	    mpz_and(temp.get_mpz_t(),acc.get_mpz_t(),icur->as_mpz().get_mpz_t());
	    acc = temp;
	}
	return Integer_O::create(acc);
    };


#define ARGS_cl_logior "(&rest integers)"
#define DECL_cl_logior ""
#define DOCS_cl_logior "logior"
    Integer_sp cl_logior(Cons_sp integers)
    {_G();
	mpz_class acc = oCar(integers).as<Integer_O>()->as_mpz();
	for ( Cons_sp cur = cCdr(integers); cur.notnilp(); cur=cCdr(cur) )
	{
	    Integer_sp icur = oCar(cur).as<Integer_O>();
	    mpz_class temp;
	    mpz_ior(temp.get_mpz_t(),acc.get_mpz_t(),icur->as_mpz().get_mpz_t());
	    acc = temp;
	}
	return Integer_O::create(acc);
    };

#define ARGS_af_logxor "(&rest integers)"
#define DECL_af_logxor ""
#define DOCS_af_logxor "logxor"
    Integer_mv af_logxor(Cons_sp integers)
    {_G();
	mpz_class acc = oCar(integers).as<Integer_O>()->as_mpz();
	for ( Cons_sp cur = cCdr(integers); cur.notnilp(); cur=cCdr(cur) )
	{
	    Integer_sp icur = oCar(cur).as<Integer_O>();
	    mpz_class temp;
	    mpz_xor(temp.get_mpz_t(),acc.get_mpz_t(),icur->as_mpz().get_mpz_t());
	    acc = temp;
	}
	return(Values(Integer_O::create(acc)));
    };



#define ARGS_af_logeqv "(&rest integers)"
#define DECL_af_logeqv ""
#define DOCS_af_logeqv "logeqv"
    Integer_mv af_logeqv(Cons_sp integers)
    {_G();
	mpz_class x = oCar(integers).as<Integer_O>()->as_mpz();
	for ( Cons_sp cur = cCdr(integers); cur.notnilp(); cur=cCdr(cur) )
	{
	    mpz_class y = oCar(cur).as<Integer_O>()->as_mpz();
	    mpz_class x_and_y;
	    mpz_and(x_and_y.get_mpz_t(),x.get_mpz_t(),y.get_mpz_t());
	    mpz_class compx;
	    mpz_com(compx.get_mpz_t(),x.get_mpz_t());
	    mpz_class compy;
	    mpz_com(compy.get_mpz_t(),y.get_mpz_t());
	    mpz_class compx_and_compy;
	    mpz_and(compx_and_compy.get_mpz_t(),compx.get_mpz_t(),compy.get_mpz_t());
	    // calculate ex-nor
	    mpz_ior(x.get_mpz_t(),x_and_y.get_mpz_t(),compx_and_compy.get_mpz_t());
	}
	return(Values(Integer_O::create(x)));
    };







#define ARGS_cl_logandc1 "(a b)"
#define DECL_cl_logandc1 ""
#define DOCS_cl_logandc1 "logandc1"
    T_mv cl_logandc1(Integer_sp a, Integer_sp b)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class zb = b->as_mpz();
	mpz_class cza;
	mpz_com(cza.get_mpz_t(),za.get_mpz_t());
	mpz_class r;
	mpz_and(r.get_mpz_t(),cza.get_mpz_t(),zb.get_mpz_t());
	return(Values(Integer_O::create(r)));
    };


#define ARGS_cl_logandc2 "(a b)"
#define DECL_cl_logandc2 ""
#define DOCS_cl_logandc2 "logandc2"
    T_mv cl_logandc2(Integer_sp a, Integer_sp b)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class zb = b->as_mpz();
	mpz_class czb;
	mpz_com(czb.get_mpz_t(),zb.get_mpz_t());
	mpz_class r;
	mpz_and(r.get_mpz_t(),za.get_mpz_t(),czb.get_mpz_t());
	return(Values(Integer_O::create(r)));
    };



#define ARGS_cl_logorc1 "(a b)"
#define DECL_cl_logorc1 ""
#define DOCS_cl_logorc1 "logorc1"
    T_mv cl_logorc1(Integer_sp a, Integer_sp b)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class zb = b->as_mpz();
	mpz_class cza;
	mpz_com(cza.get_mpz_t(),za.get_mpz_t());
	mpz_class r;
	mpz_ior(r.get_mpz_t(),cza.get_mpz_t(),zb.get_mpz_t());
	return(Values(Integer_O::create(r)));
    };


#define ARGS_cl_logorc2 "(a b)"
#define DECL_cl_logorc2 ""
#define DOCS_cl_logorc2 "logorc2"
    T_mv cl_logorc2(Integer_sp a, Integer_sp b)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class zb = b->as_mpz();
	mpz_class czb;
	mpz_com(czb.get_mpz_t(),zb.get_mpz_t());
	mpz_class r;
	mpz_ior(r.get_mpz_t(),za.get_mpz_t(),czb.get_mpz_t());
	return(Values(Integer_O::create(r)));
    };


#define ARGS_af_lognot "(a)"
#define DECL_af_lognot ""
#define DOCS_af_lognot "lognot"
    T_mv af_lognot(Integer_sp a)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class cza;
	mpz_com(cza.get_mpz_t(),za.get_mpz_t());
	return(Values(Integer_O::create(cza)));
    };


#define ARGS_af_lognand "(a b)"
#define DECL_af_lognand ""
#define DOCS_af_lognand "lognand"
    T_mv af_lognand(Integer_sp a, Integer_sp b)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class zb = b->as_mpz();
	mpz_class zand;
	mpz_and(zand.get_mpz_t(),za.get_mpz_t(),zb.get_mpz_t());
	mpz_class r;
	mpz_com(r.get_mpz_t(),zand.get_mpz_t());
	return(Values(Integer_O::create(r)));
    };



#define ARGS_af_lognor "(a b)"
#define DECL_af_lognor ""
#define DOCS_af_lognor "lognor"
    T_mv af_lognor(Integer_sp a, Integer_sp b)
    {_G();
	mpz_class za = a->as_mpz();
	mpz_class zb = b->as_mpz();
	mpz_class zor;
	mpz_ior(zor.get_mpz_t(),za.get_mpz_t(),zb.get_mpz_t());
	mpz_class r;
	mpz_com(r.get_mpz_t(),zor.get_mpz_t());
	return(Values(Integer_O::create(r)));
    };











    Number_sp contagen_add(Number_sp na, Number_sp nb)
    {_G();
	MATH_DISPATCH_BEGIN(na,nb)
	{
        case_Fixnum_v_Fixnum:
	    {
		mpz_class za(na.as<Fixnum_O>()->ref());
		mpz_class zb(nb.as<Fixnum_O>()->ref());
		mpz_class zc = za+zb;
		return Integer_O::create(zc);
	    }
        case_Fixnum_v_Bignum:
	    {
		mpz_class za(na.as<Fixnum_O>()->ref());
		mpz_class zc = za+nb.as<Bignum_O>()->ref();
		return Integer_O::create(zc);
	    }
        case_Fixnum_v_Ratio:
        case_Bignum_v_Ratio:
	    {
		mpz_class za(na->as_mpz());
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class zb_den = rb->denominator_as_mpz();
		mpz_class za_scaled = za*zb_den;
		mpz_class zr = za_scaled+rb->numerator_as_mpz();
		return Ratio_O::create(zr,zb_den);
	    }
        case_Fixnum_v_SingleFloat:
	    {
		return SingleFloat_O::create(na->as_float() + nb->as_float());
	    }
        case_Fixnum_v_DoubleFloat:
	    {
		return DoubleFloat_O::create(na->as_double() + nb->as_double());
	    }
        case_Bignum_v_Fixnum:
	    {
		mpz_class zb(nb.as<Fixnum_O>()->ref());
		mpz_class zc = na.as<Bignum_O>()->ref() + zb;
		return Integer_O::create(zc);
	    }
        case_Bignum_v_Bignum:
	    {
		return Integer_O::create(na.as<Bignum_O>()->ref()
					 +nb.as<Bignum_O>()->ref());
	    }
        case_Bignum_v_SingleFloat:
        case_Ratio_v_SingleFloat:
	    {
		return SingleFloat_O::create(na->as_float()+nb->as_float());
	    }
        case_Bignum_v_DoubleFloat:
        case_Ratio_v_DoubleFloat:
	    {
		return DoubleFloat_O::create(na->as_double()+nb->as_double());
	    }
        case_Ratio_v_Fixnum:
        case_Ratio_v_Bignum:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		mpz_class z = ra->denominator_as_mpz() * nb->as_mpz();
		mpz_class res = ra->numerator_as_mpz() + z;
		return Ratio_O::create(res,ra->denominator_as_mpz());
	    }
        case_Ratio_v_Ratio:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class z1 = ra->numerator_as_mpz()*rb->denominator_as_mpz();
		mpz_class z = ra->denominator_as_mpz()*rb->numerator_as_mpz();
		z = z1 + z;
		z1 = ra->denominator_as_mpz()*rb->denominator_as_mpz();
		return Ratio_O::create(z,z1);
	    }
        case_SingleFloat_v_Fixnum:
        case_SingleFloat_v_Bignum:
        case_SingleFloat_v_Ratio:
	    return SingleFloat_O::create(na->as_float()+nb->as_float());
        case_SingleFloat_v_SingleFloat:
	    return SingleFloat_O::create(na->as_float()+nb->as_float());
        case_SingleFloat_v_DoubleFloat:
        case_DoubleFloat_v_Fixnum:
        case_DoubleFloat_v_Bignum:
        case_DoubleFloat_v_Ratio:
        case_DoubleFloat_v_SingleFloat:
        case_DoubleFloat_v_DoubleFloat:
	    return DoubleFloat_O::create(na->as_double()+nb->as_double());
#ifdef CLASP_LONG_FLOAT
        case_Fixnum_v_LongFloat:
        case_Bignum_v_LongFloat:
	case_Ratio_v_LongFloat:
        case_SingleFloat_v_LongFloat:
        case_DoubleFloat_v_LongFloat:
        case_LongFloat_v_Fixnum:
        case_LongFloat_v_Bignum:
        case_LongFloat_v_Ratio:
        case_LongFloat_v_SingleFloat:
        case_LongFloat_v_DoubleFloat:
        case_LongFloat_v_LongFloat:
	    return LongFloat_O::create(na->as_long_float()+nb->as_long_float());
        case_Complex_v_LongFloat:
#endif // CLASP_LONG_FLOAT
        case_Complex_v_Fixnum:
        case_Complex_v_Bignum:
        case_Complex_v_Ratio:
        case_Complex_v_SingleFloat:
        case_Complex_v_DoubleFloat:
	    {
                Number_sp aux = na;
                na = nb;
		nb = aux;
                goto Complex_v_Y;
	    }
	case_Fixnum_v_Complex:
	case_Bignum_v_Complex:
	case_Ratio_v_Complex:
	case_SingleFloat_v_Complex:
	case_DoubleFloat_v_Complex:
#ifdef CLASP_LONG_FLOAT
        case_LongFloat_v_Complex:
#endif
	Complex_v_Y:
	    return Complex_O::create(contagen_add(na,nb.as<Complex_O>()->real()).as<Real_O>(),
				     nb.as<Complex_O>()->imaginary());
	case_Complex_v_Complex:
	    {
                Real_sp r = contagen_add(na.as<Complex_O>()->real(),nb.as<Complex_O>()->real()).as<Real_O>();
		Real_sp i = contagen_add(na.as<Complex_O>()->imaginary(),nb.as<Complex_O>()->imaginary()).as<Real_O>();
		return Complex_O::create(r,i);
	    }
	    break;
	default:
	    SIMPLE_ERROR(BF("Cannot contagen_add two numbers of class %s and %s")
			 % na->_instanceClass()->classNameAsString()
			 % nb->_instanceClass()->classNameAsString());
	};
	MATH_DISPATCH_END();
    };




    Number_sp contagen_sub(Number_sp na, Number_sp nb)
    {_G();
	MATH_DISPATCH_BEGIN(na,nb)
	{
        case_Fixnum_v_Fixnum:
	    {
		mpz_class za(na.as<Fixnum_O>()->ref());
		mpz_class zb(nb.as<Fixnum_O>()->ref());
		mpz_class zc = za-zb;
		return Integer_O::create(zc);
	    }
        case_Fixnum_v_Bignum:
	    {
		mpz_class za(na.as<Fixnum_O>()->ref());
		mpz_class zc = za-nb.as<Bignum_O>()->ref();
		return Integer_O::create(zc);
	    }
        case_Fixnum_v_Ratio:
        case_Bignum_v_Ratio:
	    {
		mpz_class za(na->as_mpz());
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class zb_den = rb->denominator_as_mpz();
		mpz_class za_scaled = za*zb_den;
		mpz_class zr = za_scaled-rb->numerator_as_mpz();
		return Ratio_O::create(zr,zb_den);
	    }
        case_Fixnum_v_SingleFloat:
	    {
		return SingleFloat_O::create(na->as_float() - nb->as_float());
	    }
        case_Fixnum_v_DoubleFloat:
	    {
		return DoubleFloat_O::create(na->as_double() - nb->as_double());
	    }
        case_Bignum_v_Fixnum:
	    {
		mpz_class zb(nb.as<Fixnum_O>()->ref());
		mpz_class zc = na.as<Bignum_O>()->ref() - zb;
		return Integer_O::create(zc);
	    }
        case_Bignum_v_Bignum:
	    {
		return Integer_O::create(na.as<Bignum_O>()->ref()
					 -nb.as<Bignum_O>()->ref());
	    }
        case_Bignum_v_SingleFloat:
        case_Ratio_v_SingleFloat:
	    {
		return SingleFloat_O::create(na->as_float()-nb->as_float());
	    }
        case_Bignum_v_DoubleFloat:
        case_Ratio_v_DoubleFloat:
	    {
		return DoubleFloat_O::create(na->as_double()-nb->as_double());
	    }
        case_Ratio_v_Fixnum:
        case_Ratio_v_Bignum:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		mpz_class z = ra->denominator_as_mpz() * nb->as_mpz();
		mpz_class res = ra->numerator_as_mpz() - z;
		return Ratio_O::create(res,ra->denominator_as_mpz());
	    }
        case_Ratio_v_Ratio:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class z1 = ra->numerator_as_mpz()*rb->denominator_as_mpz();
		mpz_class z = ra->denominator_as_mpz()*rb->numerator_as_mpz();
		z = z1 - z;
		z1 = ra->denominator_as_mpz()*rb->denominator_as_mpz();
		return Ratio_O::create(z,z1);
	    }
        case_SingleFloat_v_Fixnum:
        case_SingleFloat_v_Bignum:
        case_SingleFloat_v_Ratio:
	    return SingleFloat_O::create(na->as_float()-nb->as_float());
        case_SingleFloat_v_SingleFloat:
	    return SingleFloat_O::create(na->as_float()-nb->as_float());
        case_SingleFloat_v_DoubleFloat:
        case_DoubleFloat_v_Fixnum:
        case_DoubleFloat_v_Bignum:
        case_DoubleFloat_v_Ratio:
        case_DoubleFloat_v_SingleFloat:
        case_DoubleFloat_v_DoubleFloat:
	    return DoubleFloat_O::create(na->as_double()-nb->as_double());
#ifdef CLASP_LONG_FLOAT
        case_Fixnum_v_LongFloat:
        case_Bignum_v_LongFloat:
	case_Ratio_v_LongFloat:
        case_SingleFloat_v_LongFloat:
        case_DoubleFloat_v_LongFloat:
        case_LongFloat_v_Fixnum:
        case_LongFloat_v_Bignum:
        case_LongFloat_v_Ratio:
        case_LongFloat_v_SingleFloat:
        case_LongFloat_v_DoubleFloat:
        case_LongFloat_v_LongFloat:
	    return LongFloat_O::create(na->as_long_float()-nb->as_long_float());
#endif
        case_Complex_v_LongFloat:
        case_Complex_v_Fixnum:
        case_Complex_v_Bignum:
        case_Complex_v_Ratio:
        case_Complex_v_SingleFloat:
        case_Complex_v_DoubleFloat:
	    {
		return Complex_O::create(contagen_sub(na.as<Complex_O>()->real(),nb).as<Real_O>(),
					 nb.as<Complex_O>()->imaginary());
	    }
	case_Fixnum_v_Complex:
	case_Bignum_v_Complex:
	case_Ratio_v_Complex:
	case_SingleFloat_v_Complex:
	case_DoubleFloat_v_Complex:
        case_LongFloat_v_Complex:
	    return Complex_O::create(contagen_sub(na,nb.as<Complex_O>()->real()).as<Real_O>(),
				     nb.as<Complex_O>()->imaginary()->negate().as<Real_O>());
	case_Complex_v_Complex:
	    {
                Real_sp r = contagen_sub(na.as<Complex_O>()->real(),nb.as<Complex_O>()->real()).as<Real_O>();
		Real_sp i = contagen_sub(na.as<Complex_O>()->imaginary(),nb.as<Complex_O>()->imaginary()).as<Real_O>();
		return Complex_O::create(r,i);
	    }
	    break;
	default:
	    SIMPLE_ERROR(BF("Cannot contagen_sub two numbers of class %s and %s")
			 % na->_instanceClass()->classNameAsString()
			 % nb->_instanceClass()->classNameAsString());
	};
	MATH_DISPATCH_END();
    }






    Number_sp contagen_mul(Number_sp na, Number_sp nb)
    {_G();
	MATH_DISPATCH_BEGIN(na,nb)
	{
        case_Fixnum_v_Fixnum:
	    {
		mpz_class za(na.as<Fixnum_O>()->ref());
		mpz_class zb(nb.as<Fixnum_O>()->ref());
		mpz_class zc = za * zb;
		return Integer_O::create(zc);
	    }
        case_Fixnum_v_Bignum:
	    {
		mpz_class za(na.as<Fixnum_O>()->ref());
		mpz_class zc = za * nb.as<Bignum_O>()->ref();
		return Integer_O::create(zc);
	    }
        case_Fixnum_v_Ratio:
        case_Bignum_v_Ratio:
	    {
		mpz_class za(na->as_mpz());
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class zr = za * rb->numerator_as_mpz();
		return Ratio_O::create(zr,rb->denominator_as_mpz());
	    }
        case_Fixnum_v_SingleFloat:
	    {
		return SingleFloat_O::create(na->as_float() * nb->as_float());
	    }
        case_Fixnum_v_DoubleFloat:
	    {
		return DoubleFloat_O::create(na->as_double() * nb->as_double());
	    }
        case_Bignum_v_Fixnum:
	    {
		mpz_class zb(nb.as<Fixnum_O>()->ref());
		mpz_class zc = na.as<Bignum_O>()->ref() * zb;
		return Integer_O::create(zc);
	    }
        case_Bignum_v_Bignum:
	    {
		return Integer_O::create(na.as<Bignum_O>()->ref() * nb.as<Bignum_O>()->ref());
	    }
        case_Bignum_v_SingleFloat:
        case_Ratio_v_SingleFloat:
	    {
		return SingleFloat_O::create(na->as_float() * nb->as_float());
	    }
        case_Bignum_v_DoubleFloat:
        case_Ratio_v_DoubleFloat:
	    {
		return DoubleFloat_O::create(na->as_double() * nb->as_double());
	    }
        case_Ratio_v_Fixnum:
        case_Ratio_v_Bignum:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		mpz_class z = nb->as_mpz();
		return Rational_O::create(z*ra->numerator_as_mpz(),ra->denominator_as_mpz());
	    }
        case_Ratio_v_Ratio:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		Ratio_sp rb = nb.as<Ratio_O>();
		return Rational_O::create(ra->numerator_as_mpz()*rb->numerator_as_mpz(),ra->denominator_as_mpz()*rb->denominator_as_mpz());
	    }
        case_SingleFloat_v_Fixnum:
        case_SingleFloat_v_Bignum:
        case_SingleFloat_v_Ratio:
        case_SingleFloat_v_SingleFloat:
	    return SingleFloat_O::create(na->as_float() * nb->as_float());
        case_SingleFloat_v_DoubleFloat:
        case_DoubleFloat_v_Fixnum:
        case_DoubleFloat_v_Bignum:
        case_DoubleFloat_v_Ratio:
        case_DoubleFloat_v_SingleFloat:
        case_DoubleFloat_v_DoubleFloat:
	    return DoubleFloat_O::create(na->as_double() * nb->as_double());
#ifdef CLASP_LONG_FLOAT
        case_Fixnum_v_LongFloat:
        case_Bignum_v_LongFloat:
	case_Ratio_v_LongFloat:
        case_SingleFloat_v_LongFloat:
        case_DoubleFloat_v_LongFloat:
        case_LongFloat_v_Fixnum:
        case_LongFloat_v_Bignum:
        case_LongFloat_v_Ratio:
        case_LongFloat_v_SingleFloat:
        case_LongFloat_v_DoubleFloat:
        case_LongFloat_v_LongFloat:
	    return LongFloat_O::create(na->as_long_float() * nb->as_long_float());
#endif
        case_Complex_v_LongFloat:
        case_Complex_v_Fixnum:
        case_Complex_v_Bignum:
        case_Complex_v_Ratio:
        case_Complex_v_SingleFloat:
        case_Complex_v_DoubleFloat:
	    {
                Number_sp aux = na;
                na = nb;
		nb = aux;
                goto Complex_v_Y;
	    }
	case_Fixnum_v_Complex:
	case_Bignum_v_Complex:
	case_Ratio_v_Complex:
	case_SingleFloat_v_Complex:
	case_DoubleFloat_v_Complex:
        case_LongFloat_v_Complex:
	Complex_v_Y:
	    return Complex_O::create(contagen_mul(na,nb.as<Complex_O>()->real()).as<Real_O>(),
				     contagen_mul(na,nb.as<Complex_O>()->imaginary()).as<Real_O>());
	case_Complex_v_Complex:
	    {
		Complex_sp ca = na.as<Complex_O>();
		Complex_sp cb = nb.as<Complex_O>();
		Real_sp x = ca->real();
		Real_sp y = ca->imaginary();
		Real_sp u = cb->real();
		Real_sp v = cb->imaginary();
		// (x + yi)(u + vi) = (xu – yv) + (xv + yu)i.
		return Complex_O::create(contagen_sub(contagen_mul(x,u),contagen_mul(y,v)).as<Real_O>(),
					 contagen_add(contagen_mul(x,v),contagen_mul(y,u)).as<Real_O>());
	    }
	    break;
	default:
	    SIMPLE_ERROR(BF("Cannot contagen_mul two numbers of class %s and %s")
			 % na->_instanceClass()->classNameAsString()
			 % nb->_instanceClass()->classNameAsString() );
	};
	MATH_DISPATCH_END();
    }

    // Forward declaration for contagen_div
    Number_sp contagen_div(Number_sp na, Number_sp nb);

    Complex_sp complex_divide(double ar, double ai,
			      double br, double bi )
    {
        /* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
        double z1 = (ar*br)+(ai*bi);
	double z2 = (ai*br)-(ar*bi);
	double absB = (br*br)+(bi*bi);
	return Complex_O::create(DoubleFloat_O::create(z1/absB),DoubleFloat_O::create(z2/absB));
    }



    Number_sp contagen_div(Number_sp na, Number_sp nb)
    {_G();
	MATH_DISPATCH_BEGIN(na,nb)
	{
	case_Fixnum_v_Fixnum:
	case_Bignum_v_Fixnum:
	case_Fixnum_v_Bignum:
	case_Bignum_v_Bignum:
	    return Rational_O::create(na->as_mpz(),nb->as_mpz());
	case_Fixnum_v_Ratio:
	case_Bignum_v_Ratio:
	    return Rational_O::create(contagen_mul(na,nb.as<Ratio_O>()->denominator()).as<Integer_O>(),
				      nb.as<Ratio_O>()->numerator());
	case_Fixnum_v_SingleFloat:
	    return SingleFloat_O::create(na->as_float()/nb->as_float());
	case_Fixnum_v_DoubleFloat:
	    return DoubleFloat_O::create(na->as_double()/nb->as_double());
	case_Bignum_v_SingleFloat:
	case_Ratio_v_SingleFloat:
	    return SingleFloat_O::create(na->as_float()/nb->as_float());
	case_Bignum_v_DoubleFloat:
	case_Ratio_v_DoubleFloat:
	    return DoubleFloat_O::create(na->as_double()/nb->as_double());
	case_Ratio_v_Fixnum:
	case_Ratio_v_Bignum:
	    {
		Integer_sp z = contagen_mul(na.as<Ratio_O>()->denominator(),nb).as<Integer_O>();
		return Rational_O::create(na.as<Ratio_O>()->numerator(),z);
	    }
	case_Ratio_v_Ratio:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		Ratio_sp rb = nb.as<Ratio_O>();
		Integer_sp num = contagen_mul(ra->numerator(),rb->denominator()).as<Integer_O>();
		Integer_sp denom = contagen_mul(ra->denominator(),rb->numerator()).as<Integer_O>();
		return Rational_O::create(num,denom);
	    }
	case_SingleFloat_v_Fixnum:
	case_SingleFloat_v_Bignum:
	case_SingleFloat_v_Ratio:
	case_SingleFloat_v_SingleFloat:
	    return SingleFloat_O::create(na->as_float()/nb->as_float());
	case_SingleFloat_v_DoubleFloat:
	case_DoubleFloat_v_Fixnum:
	case_DoubleFloat_v_Bignum:
	case_DoubleFloat_v_Ratio:
	case_DoubleFloat_v_SingleFloat:
	case_DoubleFloat_v_DoubleFloat:
	    return DoubleFloat_O::create(na->as_double()/nb->as_double());
#ifdef CLASP_LONG_FLOAT
	case_Fixnum_v_LongFloat:
	case_Bignum_v_LongFloat:
	case_Ratio_v_LongFloat:
	case_SingleFloat_v_LongFloat:
	case_DoubleFloat_v_LongFloat:
	case_LongFloat_v_Fixnum:
	case_LongFloat_v_Bignum:
	case_LongFloat_v_Ratio:
	case_LongFloat_v_SingleFloat:
	case_LongFloat_v_DoubleFloat:
	case_LongFloat_v_LongFloat:
	    return LongFloat_O::create(na->as_long_float()/nb->as_long_float());
#endif
	case_Complex_v_Fixnum:
	case_Complex_v_Bignum:
	case_Complex_v_Ratio:
	case_Complex_v_SingleFloat:
	case_Complex_v_DoubleFloat:
	case_Complex_v_LongFloat:
	    {
		Complex_sp ca = na.as<Complex_O>();
		return Complex_O::create(contagen_div(ca->real(),nb).as<Real_O>(),
					 contagen_div(ca->imaginary(),nb).as<Real_O>());
	    }
	case_Fixnum_v_Complex:
	case_Bignum_v_Complex:
	    {
		Complex_sp cb = nb.as<Complex_O>();
		return complex_divide(na->as_double(),0.0,
				      cb->real()->as_double(),cb->imaginary()->as_double());
	    }
	case_Complex_v_Complex:
	    {
		Complex_sp ca = na.as<Complex_O>();
		Complex_sp cb = nb.as<Complex_O>();
		return complex_divide(ca->real()->as_double(),ca->imaginary()->as_double(),
				      cb->real()->as_double(),cb->imaginary()->as_double());
	    }
	case_Ratio_v_Complex:
	case_SingleFloat_v_Complex:
	case_DoubleFloat_v_Complex:
	case_LongFloat_v_Complex:
	    {
		Complex_sp cb = nb.as<Complex_O>();
		return complex_divide(na->as_double(),0.0,
				      cb->real()->as_double(),cb->imaginary()->as_double());
	    }
	}
	MATH_DISPATCH_END();
	SIMPLE_ERROR(BF("Add support to div numbers %s[%s] and %s[%s]")
		     % _rep_(na) % na->_instanceClass()->classNameAsString()
		     % _rep_(nb) % nb->_instanceClass()->classNameAsString() );
    }






#define ARGS_af__PLUS_ "(&rest numbers)"
#define DECL_af__PLUS_ ""
#define DOCS_af__PLUS_ "See CLHS: +"
    T_mv af__PLUS_(Cons_sp numbers)
    {_G();
	if ( numbers.nilp() ) return(Values(Fixnum_O::create(0)));
	Number_sp result = oCar(numbers).as<Number_O>();
	for ( Cons_sp cur=cCdr(numbers); cur.notnilp(); cur = cCdr(cur) )
	{
	    result = contagen_add(result, oCar(cur).as<Number_O>());
	}
	return(Values(result));
    }



#define ARGS_af__TIMES_ "(&rest numbers)"
#define DECL_af__TIMES_ ""
#define DOCS_af__TIMES_ "See CLHS: +"
    T_mv af__TIMES_(Cons_sp numbers)
    {_G();
	if ( numbers.nilp() ) return(Values(Fixnum_O::create(1)));
	Number_sp result = oCar(numbers).as<Number_O>();
	for ( Cons_sp cur=cCdr(numbers); cur.notnilp(); cur = cCdr(cur) )
	{
	    result = contagen_mul(result, oCar(cur).as<Number_O>());
	}
	return(Values(result));
    }



#define ARGS_af__MINUS_ "(num &rest numbers)"
#define DECL_af__MINUS_ ""
#define DOCS_af__MINUS_ "See CLHS: +"
    T_mv af__MINUS_(Number_sp num, Cons_sp numbers)
    {_G();
	if ( numbers.nilp() )
	{
	    return(Values(num->negate()));
	}
	Number_sp result = num;
	for ( Cons_sp cur=numbers; cur.notnilp(); cur = cCdr(cur) )
	{
	    result = contagen_sub(result, oCar(cur).as<Number_O>());
	}
	return(Values(result));
    }


#define ARGS_af__DIVIDE_ "(num &rest numbers)"
#define DECL_af__DIVIDE_ ""
#define DOCS_af__DIVIDE_ "See CLHS: /"
    T_mv af__DIVIDE_(Number_sp num, Cons_sp numbers)
    {_G();
	if ( numbers.nilp() )
	{
	    return(Values(num->reciprocal()));
	}
	Number_sp result = num;
	for ( Cons_sp cur=numbers; cur.notnilp(); cur = cCdr(cur) )
	{
	    result = contagen_div(result, oCar(cur).as<Number_O>());
	}
	return(Values(result));
    }





/* ----------------------------------------------------------------------

   fix_compare

*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_comp.c  -- Comparisons on numbers.
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

/*
 * In Common Lisp, comparisons between floats and integers are performed
 * via an intermediate rationalization of the floating point number. In C,
 * on the other hand, the comparison is performed by converting the integer
 * into a floating point number. However, if the double type is too small
 * this may lead to a loss of precision and two numbers being told equal
 * when, by Common Lisp standards, would not.
 */
static int
double_fix_compare(Fixnum n, double d)
{
	if ((double)n < d) {
		return -1;
	} else if ((double)n > d) {
		return +1;
	} else if (sizeof(double) > sizeof(Fixnum)) {
		return 0;
	} else {
		/* When we reach here, the double type has no
		 * significant decimal part. However, as explained
		 * above, the double type is too small and integers
		 * may coerce to the same double number giving a false
		 * positive. Hence we perform the comparison in
		 * integer space. */
		Fixnum m = d;
		if (n == m) {
			return 0;
		} else if (n > m) {
			return +1;
		} else {
			return -1;
		}
	}
}

#ifdef CLASP_LONG_FLOAT
static int
long_double_fix_compare(Fixnum n, LongFloat d)
{
	if ((LongFloat)n < d) {
		return -1;
	} else if ((LongFloat)n > d) {
		return +1;
	} else if (sizeof(LongFloat) > sizeof(Fixnum)) {
		return 0;
	} else {
		Fixnum m = d;
		if (n == m) {
			return 0;
		} else if (n > m) {
			return +1;
		} else {
			return -1;
		}
	}
}
#endif


/* ----------------------------------------------------------------------

   basic_compare

*/



    /*! Return -1 if a<b
      0 if a == b
      +1 if a > b
    */
    int basic_compare(Number_sp na, Number_sp nb)
    {_G();
	MATH_DISPATCH_BEGIN(na,nb)
	{
        case_Fixnum_v_Fixnum:
	    {
		int& fa = na.as<Fixnum_O>()->ref();
		int& fb = nb.as<Fixnum_O>()->ref();
		if ( fa < fb ) return -1;
		if ( fa == fb ) return 0;
		return 1;
	    }
        case_Fixnum_v_Bignum:
	    {
		mpz_class za = na.as<Fixnum_O>()->as_mpz();
		mpz_class& zb = nb.as<Bignum_O>()->ref();
		if ( za < zb ) return -1;
		if ( za == zb ) return 0;
		return 1;
	    }
        case_Fixnum_v_Ratio:
        case_Bignum_v_Ratio:
	    {
		mpz_class za(na->as_mpz());
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class zb_den = rb->denominator_as_mpz();
		mpz_class za_scaled = za*rb->denominator_as_mpz();
		mpz_class zr = za_scaled-rb->numerator_as_mpz();
		if ( zr < 0 ) return -1;
		if ( zr == 0 ) return 0;
		return 1;
	    }
        case_Fixnum_v_SingleFloat:
	    {
		float a = na->as_float();
		float b = nb->as_float();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
	    }
        case_Fixnum_v_DoubleFloat:
	    {
		return double_fix_compare(na.as<Fixnum_O>()->get(),nb.as<DoubleFloat_O>()->get());
		break;
/*
		double a = na->as_double();
		double b = nb->as_double();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
*/
	    }
        case_Bignum_v_Fixnum:
	    {
		mpz_class& za(na.as<Bignum_O>()->ref());
		mpz_class zb = nb.as<Fixnum_O>()->ref();
		if (za < zb ) return -1;
		if ( za==zb ) return 0;
		return 1;
	    }
        case_Bignum_v_Bignum:
	    {
		mpz_class& za = na.as<Bignum_O>()->ref();
		mpz_class& zb = nb.as<Bignum_O>()->ref();
		if ( za < zb ) return -1;
		if ( za == zb ) return 0;
		if ( za > zb ) return 1;
	    }
        case_Bignum_v_SingleFloat:
        case_Ratio_v_SingleFloat:
	    {
		float a = na->as_float();
		float b = nb->as_float();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
	    }
        case_Bignum_v_DoubleFloat:
        case_Ratio_v_DoubleFloat:
	    {
		double a = na->as_double();
		double b = nb->as_double();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
	    }
        case_Ratio_v_Fixnum:
        case_Ratio_v_Bignum:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		mpz_class z = ra->denominator_as_mpz() * nb->as_mpz();
		mpz_class raz = ra->numerator_as_mpz();
		if ( raz < z ) return -1;
		if ( raz == z ) return 0;
		return 1;
	    }
        case_Ratio_v_Ratio:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class z1 = ra->numerator_as_mpz()*rb->denominator_as_mpz();
		mpz_class z = ra->denominator_as_mpz()*rb->numerator_as_mpz();
		if ( z1 < z ) return -1;
		if ( z1 == z ) return 0;
		return 1;
	    }
        case_SingleFloat_v_Fixnum:
        case_SingleFloat_v_Bignum:
        case_SingleFloat_v_Ratio:
        case_SingleFloat_v_SingleFloat:
	    {
		float a = na->as_float();
		float b = nb->as_float();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
	    }
        case_DoubleFloat_v_Fixnum:
	    return -double_fix_compare(nb.as<Fixnum_O>()->get(),na.as<DoubleFloat_O>()->get());
	    break;
        case_SingleFloat_v_DoubleFloat:
        case_DoubleFloat_v_Bignum:
        case_DoubleFloat_v_Ratio:
        case_DoubleFloat_v_SingleFloat:
        case_DoubleFloat_v_DoubleFloat:
	    {
		double a = na->as_double();
		double b = nb->as_double();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
	    }
#ifdef CLASP_LONG_FLOAT
        case_Fixnum_v_LongFloat:
	    return long_double_fix_compare(na.as<Fixnum_O>()->get(),nb.as<LongFloat_O>()->get());
	    break;
        case_LongFloat_v_Fixnum:
	    return -long_double_fix_compare(nb.as<Fixnum_O>()->get(),na.as<LongFloat_O>()->get());
	    break;
        case_Bignum_v_LongFloat:
	case_Ratio_v_LongFloat:
        case_SingleFloat_v_LongFloat:
        case_DoubleFloat_v_LongFloat:
        case_LongFloat_v_Bignum:
        case_LongFloat_v_Ratio:
        case_LongFloat_v_SingleFloat:
        case_LongFloat_v_DoubleFloat:
        case_LongFloat_v_LongFloat:
	    {
		LongFloat a = na->as_long_float();
		LongFloat b = nb->as_long_float();
		if ( a < b ) return -1;
		if ( a == b ) return 0;
		return 1;
	    }
#endif
	default:
	    SIMPLE_ERROR(BF("Cannot compare two numbers of class %s and %s")
			 % na->_instanceClass()->classNameAsString() % nb->_instanceClass()->classNameAsString());
	};
	MATH_DISPATCH_END();
    }


    T_sp numbers_monotonic(int s, int t, Cons_sp args)
    {_G();
	Number_sp c = oCar(args).as<Number_O>();
	Number_sp d;
	int dir;
	args = cCdr(args);
	while ( args.notnilp() )
	{
	    d = oCar(args).as<Number_O>();
	    dir = s*basic_compare(c,d);
	    if ( dir < t) return _lisp->_false();
	    c = d;
	    args = cCdr(args);
	}
	return _lisp->_true();
    };









#define DOCS_af__LT_ "LT less than function"
#define LOCK_af__LT_ 1
#define ARGS_af__LT_ "(&rest args)"
#define DECL_af__LT_ ""
    T_mv af__LT_(Cons_sp args)
    {_G();
	return(Values(numbers_monotonic(-1,1,args)));
    };



#define DOCS_af__GT_ "GT less than function"
#define LOCK_af__GT_ 1
#define ARGS_af__GT_ "(&rest args)"
#define DECL_af__GT_ ""
    T_mv af__GT_(Cons_sp args)
    {_G();
	return(Values(numbers_monotonic(1,1,args)));
    };




#define DOCS_af__LE_ "LT less than function"
#define LOCK_af__LE_ 1
#define ARGS_af__LE_ "(&rest args)"
#define DECL_af__LE_ ""
    T_mv af__LE_(Cons_sp args)
    {_G();
	return(Values(numbers_monotonic(-1,0,args)));
    };



#define DOCS_af__GE_ "GT less than function"
#define LOCK_af__GE_ 1
#define ARGS_af__GE_ "(&rest args)"
#define DECL_af__GE_ ""
    T_mv af__GE_(Cons_sp args)
    {_G();
	return(Values(numbers_monotonic(1,0,args)));
    };



    /*! Return true if two numbers are equal otherwise false */
    bool basic_equalp(Number_sp na, Number_sp nb)
    {_G();
	MATH_DISPATCH_BEGIN(na,nb)
	{
        case_Fixnum_v_Fixnum:
	    {
		int& fa = na.as<Fixnum_O>()->ref();
		int& fb = nb.as<Fixnum_O>()->ref();
		return fa == fb;
	    }
        case_Fixnum_v_Bignum:
	    {
		mpz_class za = na.as<Fixnum_O>()->as_mpz();
		mpz_class& zb = nb.as<Bignum_O>()->ref();
		return za==zb;
	    }
        case_Fixnum_v_Ratio:
        case_Bignum_v_Ratio:
	    {
		mpz_class za(na->as_mpz());
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class zb_den = rb->denominator_as_mpz();
		mpz_class za_scaled = za*rb->denominator_as_mpz();
		mpz_class zr = za_scaled-rb->numerator_as_mpz();
		return ( zr == 0 );
	    }
        case_Fixnum_v_SingleFloat:
	    {
		float a = na->as_float();
		float b = nb->as_float();
		return a==b;
	    }
        case_Fixnum_v_DoubleFloat:
	    {
		double a = na->as_double();
		double b = nb->as_double();
		return a==b;
	    }
        case_Bignum_v_Fixnum:
	    {
		mpz_class& za(na.as<Bignum_O>()->ref());
		mpz_class zb = nb.as<Fixnum_O>()->ref();
		return za==zb;
	    }
        case_Bignum_v_Bignum:
	    {
		mpz_class& za = na.as<Bignum_O>()->ref();
		mpz_class& zb = nb.as<Bignum_O>()->ref();
		return (za==zb);
	    }
        case_Bignum_v_SingleFloat:
        case_Ratio_v_SingleFloat:
	    {
		float a = na->as_float();
		float b = nb->as_float();
		return a==b;
	    }
        case_Bignum_v_DoubleFloat:
        case_Ratio_v_DoubleFloat:
	    {
		double a = na->as_double();
		double b = nb->as_double();
		return a==b;
	    }
        case_Ratio_v_Fixnum:
        case_Ratio_v_Bignum:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		mpz_class z = ra->denominator_as_mpz() * nb->as_mpz();
		mpz_class raz = ra->numerator_as_mpz();
		return raz==z;
	    }
        case_Ratio_v_Ratio:
	    {
		Ratio_sp ra = na.as<Ratio_O>();
		Ratio_sp rb = nb.as<Ratio_O>();
		mpz_class z1 = ra->numerator_as_mpz()*rb->denominator_as_mpz();
		mpz_class z = ra->denominator_as_mpz()*rb->numerator_as_mpz();
		return (z1==z);
	    }
        case_SingleFloat_v_Fixnum:
        case_SingleFloat_v_Bignum:
        case_SingleFloat_v_Ratio:
        case_SingleFloat_v_SingleFloat:
	    {
		float a = na->as_float();
		float b = nb->as_float();
		return a==b;
	    }
        case_SingleFloat_v_DoubleFloat:
        case_DoubleFloat_v_Fixnum:
        case_DoubleFloat_v_Bignum:
        case_DoubleFloat_v_Ratio:
        case_DoubleFloat_v_SingleFloat:
        case_DoubleFloat_v_DoubleFloat:
	    {
		double a = na->as_double();
		double b = nb->as_double();
		return a==b;
	    }
        case_Fixnum_v_LongFloat:
        case_Bignum_v_LongFloat:
	case_Ratio_v_LongFloat:
        case_SingleFloat_v_LongFloat:
        case_DoubleFloat_v_LongFloat:
        case_LongFloat_v_Fixnum:
        case_LongFloat_v_Bignum:
        case_LongFloat_v_Ratio:
        case_LongFloat_v_SingleFloat:
        case_LongFloat_v_DoubleFloat:
        case_LongFloat_v_LongFloat:
	    {
		LongFloat a = na->as_long_float();
		LongFloat b = nb->as_long_float();
		return a==b;
	    }
	default:
	    SIMPLE_ERROR(BF("Cannot compare two numbers of class %s and %s")
			 % na->_instanceClass()->classNameAsString() % nb->_instanceClass()->classNameAsString());
	};
	MATH_DISPATCH_END();
    }





#define DOCS_af__NE_ "NE_"
#define LOCK_af__NE_ 1
#define ARGS_af__NE_ "(&rest args)"
#define DECL_af__NE_ ""
    T_mv af__NE_(Cons_sp args)
    {_G();
	while ( args.notnilp() )
	{
	    Number_sp a = oCar(args).as<Number_O>();
	    for ( Cons_sp cur = cCdr(args); cur.notnilp(); cur=cCdr(cur) )
	    {
		Number_sp b = oCar(cur).as<Number_O>();
		if ( basic_equalp(a,b) ) return(Values(_Nil<T_O>()));
	    }
	    args = cCdr(args);
	}
	return(Values(_lisp->_true()));
    }


#define DOCS_af__EQ_ "EQ_"
#define LOCK_af__EQ_ 1
#define ARGS_af__EQ_ "(&rest args)"
#define DECL_af__EQ_ ""
    T_mv af__EQ_(Cons_sp args)
    {_G();
	if ( args.nilp() ) return(Values(_lisp->_true()));
	Number_sp a = oCar(args).as<Number_O>();
	args = cCdr(args);
	while ( args.notnilp() )
	{
	    Number_sp b = oCar(args).as<Number_O>();
	    if ( !basic_equalp(a,b) ) return(Values(_lisp->_false()));
	    args = cCdr(args);
	}
	return(Values(_lisp->_true()));
    };








    EXPOSE_CLASS(core,Number_O);

    void Number_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<Number_O>()
	    //	    .def("core:zerop",&Number_O::zerop)
	    .def("signum",&Number_O::signum)
	    .def("abs",&Number_O::abs)
	    .def("core:onePlus",&Number_O::onePlus)
	    .def("core:oneMinus",&Number_O::oneMinus)
	    ;
	;
	SYMBOL_EXPORT_SC_(ClPkg,max);
	ClDefun(max);
	SYMBOL_EXPORT_SC_(ClPkg,min);
	ClDefun(min);
	SYMBOL_EXPORT_SC_(ClPkg,zerop);
	ClDefun(zerop);

	SYMBOL_SC_(CorePkg,fixnum_number_of_bits);
	Defun(fixnum_number_of_bits);

	Defun(_LT_);
	Defun(_GT_);
	Defun(_LE_);
	Defun(_GE_);
	Defun(_EQ_);
	Defun(_NE_);

	SYMBOL_EXPORT_SC_(ClPkg,_LT_);
	SYMBOL_EXPORT_SC_(ClPkg,_GT_);
	SYMBOL_EXPORT_SC_(ClPkg,_LE_);
	SYMBOL_EXPORT_SC_(ClPkg,_GE_);
	SYMBOL_EXPORT_SC_(ClPkg,_EQ_);
	SYMBOL_EXPORT_SC_(ClPkg,_NE_);

	SYMBOL_EXPORT_SC_(ClPkg,_PLUS_);
	Defun(_PLUS_);

	SYMBOL_EXPORT_SC_(ClPkg,_TIMES_);
	Defun(_TIMES_);

	SYMBOL_EXPORT_SC_(ClPkg,_MINUS_);
	Defun(_MINUS_);

	SYMBOL_EXPORT_SC_(ClPkg,_DIVIDE_);
	Defun(_DIVIDE_);

    }
    void Number_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Number,"","",_lisp)
	    ;
#endif
    }




    Number_sp Number_O::create(double val)
    {
	return DoubleFloat_O::create(val);
    }

    Number_sp Number_O::create(int val)
    {
	return Fixnum_O::create(val);
    }

    Number_sp Number_O::create(uint val)
    {
	return Integer_O::create(val);
    }





    bool Number_O::operator<(T_sp obj) const
    {
	if ( af_numberP(obj) )
	{
	    return basic_compare(this->asSmartPtr(),obj.as<Number_O>())<0;
	}
	return this->Base::operator<(obj);
    }

    bool Number_O::operator<=(T_sp obj) const
    {
	if ( af_numberP(obj) )
	{
	    return basic_compare(this->asSmartPtr(),obj.as<Number_O>())<=0;
	}
	return this->Base::operator<=(obj);
    }

    bool Number_O::operator>(T_sp obj) const
    {
	if ( af_numberP(obj) )
	{
	    return basic_compare(this->asSmartPtr(),obj.as<Number_O>())>0;
	}
	return this->Base::operator>(obj);
    }

    bool Number_O::operator>=(T_sp obj) const
    {
	if ( af_numberP(obj) )
	{
	    return basic_compare(this->asSmartPtr(),obj.as<Number_O>())>=0;
	}
	return this->Base::operator>=(obj);
    }


    bool Number_O::equal(T_sp obj) const
    {
	if ( this->eq(obj) ) return true;
	return this->eql(obj);
    }

    string Number_O::valueAsString() const
    {
	stringstream ss;
	ss << "Value_of_class(" << this->_instanceClass()->classNameAsString() << ")";
	return ss.str();
    }






    EXPOSE_CLASS(core,Real_O);

    void Real_O::exposeCando(Lisp_sp lisp)
    {
	class_<Real_O>()
	    .def("minusp",&Real_O::minusp)
	    .def("plusp",&Real_O::plusp)
	    ;
    }


    void Real_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    ;
#endif
    };








    EXPOSE_CLASS(core,Float_O);

    void Float_O::exposeCando(Lisp_sp lisp)
    {
	class_<Float_O>()
	    .def("core:castToInteger",&Float_O::castToInteger)
	    ;
    }


    void Float_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Float,"","",_lisp)
	    ;
#endif
    };














    Rational_sp Rational_O::create(mpz_class const& num, mpz_class const& denom)
    {_G();
	mpz_class q, r;
	ASSERT(denom!=0);
	if ( denom == 1 )
	{
	    return Integer_O::create(num);
	}
	mpz_cdiv_qr(q.get_mpz_t(),r.get_mpz_t(),num.get_mpz_t(),denom.get_mpz_t());
	if ( r==0 )
	{
	    return Integer_O::create(q);
	}
	return Ratio_O::create(num,denom);
    }

    Rational_sp Rational_O::create(Integer_sp num, Integer_sp denom)
    {_G();
	return Rational_O::create(num->as_mpz(),denom->as_mpz());
    }





    EXPOSE_CLASS(core,Rational_O);

    void Rational_O::exposeCando(Lisp_sp lisp)
    {
	class_<Rational_O>()
	    ;
    }


    void Rational_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Rational,"","",_lisp)
	    ;
#endif
    };










#define ARGS_af_nan "(num)"
#define DECL_af_nan ""
#define DOCS_af_nan "Return a number that is NAN"
    DoubleFloat_mv af_nan()
    {_G();
	DoubleFloat_sp rnan = DoubleFloat_O::create(NAN);
	return(Values(rnan));
    }




// --------------------------------------------------------------------------------

    T_sp Integer_O::makeIntegerType(int low, int hi)
    {_G();
	return Cons_O::createList(cl::_sym_Integer_O,Integer_O::create(low),Integer_O::create(hi));
    }

    Integer_sp Integer_O::create(float v)
    {_G();
	if ( v > (float)(std::numeric_limits<int>::min()) && v < (float)(std::numeric_limits<int>::max()))
	{
	    return Fixnum_O::create((int)v);
	}
	Bignum rop;
	mpz_set_d(rop.get_mpz_t(),v);
	return Bignum_O::create(rop);
    }

    Integer_sp Integer_O::create(double v)
    {_G();
	if ( v > (double)(std::numeric_limits<int>::min()) && v < (double)(std::numeric_limits<int>::max()))
	{
	    return Fixnum_O::create((int)v);
	}
	Bignum rop;
	mpz_set_d(rop.get_mpz_t(),v);
	return Bignum_O::create(rop);
    }

    Integer_sp Integer_O::createLongFloat(LongFloat v)
    {_G();
	if ( v > (LongFloat)(std::numeric_limits<int>::min()) && v < (LongFloat)(std::numeric_limits<int>::max()))
	{
	    return Fixnum_O::create((int)v);
	}
	Bignum rop;
	mpz_set_d(rop.get_mpz_t(),v);
	return Bignum_O::create(rop);
    }

    Integer_sp Integer_O::create(const mpz_class& v)
    {_G();
	if ( v.fits_sint_p() )
	{
	    int i = v.get_si();
	    return Fixnum_O::create(i);
	}
	return Bignum_O::create(v);
    }

    Integer_sp Integer_O::create(int v)
    {_G();
	if ( v>=MOST_NEGATIVE_FIXNUM && v <= MOST_POSITIVE_FIXNUM) {
	    return Fixnum_O::create(v);
	}
	Bignum z(v);
	return Bignum_O::create(z);
    }


    Integer_sp Integer_O::create(uint v)
    {_G();
	if ( v <= MOST_POSITIVE_FIXNUM )
	{
	    return Fixnum_O::create((int)v);
	}
	Bignum z(v);
	return Bignum_O::create(z);
    }

    Integer_sp Integer_O::create(size_t v)
    {_G();
	if ( v <= MOST_POSITIVE_FIXNUM )
	{
	    return Fixnum_O::create((int)v);
	}
	Bignum z(v);
	return Bignum_O::create(z);
    }



#ifndef _TARGET_OS_LINUX
    Integer_sp Integer_O::create(uint64_t v)
    {_G();
	if ( v <= MOST_POSITIVE_FIXNUM )
	{
	    return Integer_O::create((int)v);
	}
	Bignum z;
	mpz_import(z.get_mpz_t(),2,_lisp->integer_ordering()._mpz_import_word_order,
		   _lisp->integer_ordering()._mpz_import_size,
		   _lisp->integer_ordering()._mpz_import_endian, 0, &v);
	return Bignum_O::create(z);
    }
#endif
    Integer_sp Integer_O::create(LongLongInt v)
    {_G();
	if ( v >= MOST_NEGATIVE_FIXNUM && v <= MOST_POSITIVE_FIXNUM )
	{
	    return Integer_O::create((uint)v);
	}
	Bignum z;
	mpz_import(z.get_mpz_t(),2,_lisp->integer_ordering()._mpz_import_word_order,
		   _lisp->integer_ordering()._mpz_import_size,
		   _lisp->integer_ordering()._mpz_import_endian, 0, &v);
	return Bignum_O::create(z);
    }
};

namespace core {
    uint64_t Integer_O::as_uint64() const
    {
        SUBIMP();
    };


    Integer_O::~Integer_O() {};

    EXPOSE_CLASS(core,Integer_O);

    void Integer_O::exposeCando(Lisp_sp lisp)
    {
	class_<Integer_O>()
	    .def("evenp",&Integer_O::evenp)
	    .def("oddp",&Integer_O::oddp)
	    ;
	SYMBOL_EXPORT_SC_(ClPkg,logand);
	ClDefun(logand);
	SYMBOL_EXPORT_SC_(ClPkg,logior);
	ClDefun(logior);
	SYMBOL_EXPORT_SC_(ClPkg,logandc1);
	ClDefun(logandc1);
	SYMBOL_EXPORT_SC_(ClPkg,logandc2);
	ClDefun(logandc2);
	SYMBOL_EXPORT_SC_(ClPkg,logeqv);
	Defun(logeqv);
	SYMBOL_EXPORT_SC_(ClPkg,lognand);
	Defun(lognand);
	SYMBOL_EXPORT_SC_(ClPkg,lognor);
	Defun(lognor);
	SYMBOL_EXPORT_SC_(ClPkg,lognot);
	Defun(lognot);
	SYMBOL_EXPORT_SC_(ClPkg,logorc1);
	ClDefun(logorc1);
	SYMBOL_EXPORT_SC_(ClPkg,logorc2);
	ClDefun(logorc2);
	SYMBOL_EXPORT_SC_(ClPkg,logxor);
	Defun(logxor);
    }


    void Integer_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Integer,"","",_lisp)
	    ;
#endif
    };





    EXPOSE_CLASS(core,Fixnum_O);


    string Fixnum_O::__repr__() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }


    void Fixnum_O::sxhash(HashGenerator& hg) const
    {
	hg.addPart(this->_Value);
    }


    int Fixnum_O::as_int() const
    {
	return this->_Value;
    }


    Integer_sp Fixnum_O::shift(int bits) const
    {
	if ( bits == 0 ) return Fixnum_O::create(this->_Value);
	if ( bits < 0 ) {
	    Fixnum y = this->_Value;
	    bits = -bits;
	    if ( bits >= FIXNUM_BITS) {
		y = (y < 0) ? -1 : 0;
	    } else {
		y >>= bits;
	    }
	    return Fixnum_O::create(y);
	} else {
	    Bignum val(this->_Value);
	    Bignum res;
	    mpz_mul_2exp(res.get_mpz_t(),val.get_mpz_t(),bits);
	    return Integer_O::create(res);
	}
    }


    int Fixnum_O::bit_length() const
    {
	int count, i(this->_Value);
	if ( i < 0 ) i = ~i;
	for ( count=0; i && (count<FIXNUM_BITS); i >>= 1, count++ );
	return count;
    }

    int Fixnum_O::number_of_bits()
    {
	int num = sizeof(Fixnum_type)*sizeof(unsigned char)*8;
	return num;
    }



    LongLongInt Fixnum_O::as_LongLongInt() const
    {
	return this->_Value;
    }

    Bignum Fixnum_O::as_mpz() const
    {
	mpz_class z = this->_Value;
	return z;
    }

    float Fixnum_O::as_float() const
    {_G();
	return (float)this->_Value;
    }

    double Fixnum_O::as_double() const
    {_G();
	return (double)this->_Value;
    }

    LongFloat Fixnum_O::as_long_float() const
    {_G();
	return (LongFloat)this->_Value;
    }


    Number_sp Fixnum_O::signum() const
    {_G();
	if ( this->_Value == 0 ) return Fixnum_O::create(0);
	if ( this->_Value > 0 ) return Fixnum_O::create(1);
	return Fixnum_O::create(-1);
    }





    Number_sp Fixnum_O::copy() const
    {_OF();
	return Fixnum_O::create(this->_Value);
    }


#if defined(OLD_SERIALIZE)
    void Fixnum_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
//	    node.setObject(this->sharedThis<Fixnum_O>());
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif
#if defined(XML_ARCHIVE)
    void Fixnum_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("val",this->_Value);
    }
#endif // defined(XML_ARCHIVE)









    Fixnum_sp Fixnum_O::create(int nm)
    {_G();
	GC_ALLOCATE(Fixnum_O,v );
	v->set(nm);
	return v;
    }

    Fixnum_sp Fixnum_O::create(LongLongInt nm)
    {_G();
	if ( nm > MOST_POSITIVE_FIXNUM || nm < MOST_NEGATIVE_FIXNUM )
	{
	    SIMPLE_ERROR(BF("LongLongInt value(%d) was too large/small to cast to int") % nm );
	}
	int inm = nm;
	return Fixnum_O::create(inm);
    }

    Fixnum_sp Fixnum_O::create(uint nm)
    {_G();
	if ( nm > MOST_POSITIVE_FIXNUM )
	{
	    SIMPLE_ERROR(BF("uint value(%d) was too large/small to cast to int") % nm );
	}
	int inm = nm;
	return Fixnum_O::create(inm);
    }

    uint64_t Fixnum_O::as_uint64() const
    {
	return this->_Value;
    }



    string Fixnum_O::asChar() const
    {
	boost::format f("%c");
	f % (char)(this->_Value);
	TRY_BOOST_FORMAT_STRING(f,f_str);
	return f_str;
    }












    bool	Fixnum_O::eql(T_sp obj) const
    {_G();
	if ( this->eq(obj) ) return true;
	if ( af_fixnumP(obj) )
	{
	    Fixnum_sp t = obj.as<Fixnum_O>();
	    return this->get() == t->get();
	} else if ( af_bignumP(obj))
	{
	    mpz_class me = this->_Value;
	    return me == obj.as<Bignum_O>()->ref();
	}
	return false;
    }

    bool	Fixnum_O::eqn(T_sp obj) const
    {_G();
	if ( this->eq(obj) ) return true;
	if ( af_doubleFloatP(obj) )
	{
	    DoubleFloat_sp t = safe_downcast<DoubleFloat_O>(obj);
	    return this->get() == t->get();
	} else if ( af_fixnumP(obj) )
	{
	    Fixnum_sp t = safe_downcast<Fixnum_O>(obj);
	    bool b = (this->get() == t->get());
	    LOG(BF("Compared if %d == %d -> %d") % this->get() % t->get() % b  );
	    return b;
	} else if ( af_bignumP(obj) )
	{
	    IMPLEMENT_ME();
#if 0
	    LongLongInt_sp t = safe_downcast<LongLongInt_O>(obj);
	    bool b = (this->get() == t->get());
	    LOG(BF("Compared if %d == %d -> %d") % this->get() % t->get() % b  );
	    return b;
#endif
	}
	ASSERT(!af_numberP(obj) );
	return false;
    }







    void Fixnum_O::exposeCando(Lisp_sp lisp)
    {
	core::class_<Fixnum_O>()
	    .def("core:setFromString", &Fixnum_O::setFromString )
//	    .def("core:set", &Fixnum_O::set )
//	    .def("get", &Fixnum_O::get)
	    .def("core:asChar",&Fixnum_O::asChar)
	    ;
    }
    void Fixnum_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Fixnum,"","",_lisp)
	    .def("valueAsString", &Fixnum_O::valueAsString )
	    .def("setFromString", &Fixnum_O::setFromString )
//	    .def("set", &Fixnum_O::set)
//	    .def("get", &Fixnum_O::get)
	    ;
//    boost::python::def("create_Fixnum",&create_Fixnum );
#endif
    }








// ------------------------------------------------------------------------




    float ShortFloat_O::as_float() const
    {
	return (float)this->_Value;
    }

    double ShortFloat_O::as_double() const
    {
	return (double)this->_Value;
    }

    LongFloat ShortFloat_O::as_long_float() const
    {
	return (LongFloat)this->_Value;
    }

    Integer_sp ShortFloat_O::castToInteger() const
    {
	if (this->_Value < 0) {
	    float f = -this->_Value;
	    int cf = *(int*)&f;
	    return Integer_O::create(cf)->negate().as<Integer_O>();
	}
	int cf = *(int*)&this->_Value;
	return Integer_O::create(cf);
    }



    Number_sp ShortFloat_O::reciprocal() const
    {_G();
	return ShortFloat_O::create(1.0/this->_Value);
    }

    Number_sp ShortFloat_O::signum() const
    {
	return ShortFloat_O::create(this->_Value>0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0 ));
    }


    string ShortFloat_O::valueAsString() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }

    Number_sp ShortFloat_O::abs() const
    {_G();
	return ShortFloat_O::create(fabs(this->_Value));
    }



    Number_sp ShortFloat_O::copy() const
    {_OF();
	return ShortFloat_O::create(this->_Value);
    }


#if defined(OLD_SERIALIZE)
    void ShortFloat_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
	    // node.setObject(this->sharedThis<ShortFloat_O>());
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif


#if defined(XML_ARCHIVE)
    void	ShortFloat_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("val",this->_Value);
    }
#endif // defined(XML_ARCHIVE)




    void ShortFloat_O::setFromString(const string& str)
    {_G();
	this->_Value = atof(str.c_str());
    }








    void ShortFloat_O::sxhash(HashGenerator& hg) const
    {_OF();
	hg.addPart(std::abs(::floor(this->_Value)));
    }


    bool	ShortFloat_O::eql(T_sp obj) const
    {
	if ( this->eq(obj) ) return true;
	if ( obj.isA<Number_O>() )
	{
	    Number_sp num = obj.as<Number_O>();
	    return this->get() == num->as_double();
	}
	return false;
    }

    bool	ShortFloat_O::eqn(T_sp obj) const
    {_OF();
	if ( af_shortFloatP(obj) )
	{
	    ShortFloat_sp t = safe_downcast<ShortFloat_O>(obj);
	    return this->get() == t->get();
	} else if ( af_fixnumP(obj) )
	{
	    Fixnum_sp t = safe_downcast<Fixnum_O>(obj);
	    return this->get() == t->get();
	}
	ASSERT(!af_numberP(obj) );
	return false;
    }




    string ShortFloat_O::__repr__() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }




    EXPOSE_CLASS(core,ShortFloat_O);


    void ShortFloat_O::exposeCando(Lisp_sp lisp)
    {
	class_<ShortFloat_O>()
	    ;
	;
    }

    void ShortFloat_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    ;
#endif
    }





//----------------------------------------------------------------------



    float SingleFloat_O::as_float() const
    {
	return (float)this->_Value;
    }

    double SingleFloat_O::as_double() const
    {
	return (double)this->_Value;
    }

    LongFloat SingleFloat_O::as_long_float() const
    {
	return (LongFloat)this->_Value;
    }


    Integer_sp SingleFloat_O::castToInteger() const
    {
	if (this->_Value < 0) {
	    float f = -this->_Value;
	    int cf = *(int*)&f;
	    return Integer_O::create(cf)->negate().as<Integer_O>();
	}
	int cf = *(int*)&this->_Value;
	return Integer_O::create(cf);
    }



    Number_sp SingleFloat_O::copy() const
    {_OF();
	return SingleFloat_O::create(this->_Value);
    }


#if defined(OLD_SERIALIZE)
    void SingleFloat_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
	    // node.setObject(this->sharedThis<SingleFloat_O>());
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif


#if defined(XML_ARCHIVE)
    void	SingleFloat_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("val",this->_Value);
    }
#endif // defined(XML_ARCHIVE)



    Number_sp SingleFloat_O::signum() const
    {
	return SingleFloat_O::create(this->_Value>0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0 ));
    }




    void SingleFloat_O::setFromString(const string& str)
    {_G();
	this->_Value = atof(str.c_str());
    }







    void SingleFloat_O::sxhash(HashGenerator& hg) const
    {_OF();
	hg.addPart(std::abs(::floor(this->_Value)));
    }


    bool	SingleFloat_O::eql(T_sp obj) const
    {
	if ( this->eq(obj) ) return true;
	if ( obj.isA<Number_O>() )
	{
	    Number_sp num = obj.as<Number_O>();
	    return this->get() == num->as_double();
	}
	return false;
    }

    bool	SingleFloat_O::eqn(T_sp obj) const
    {_OF();
	if ( af_singleFloatP(obj) )
	{
	    SingleFloat_sp t = safe_downcast<SingleFloat_O>(obj);
	    return this->get() == t->get();
	} else if ( af_fixnumP(obj) )
	{
	    Fixnum_sp t = safe_downcast<Fixnum_O>(obj);
	    return this->get() == t->get();
	}
	ASSERT(!af_numberP(obj) );
	return false;
    }






    Number_sp SingleFloat_O::reciprocal() const
    {_G();
	return SingleFloat_O::create(1.0/this->_Value);
    }

    string SingleFloat_O::valueAsString() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }

    Number_sp SingleFloat_O::abs() const
    {_G();
	return SingleFloat_O::create(fabs(this->_Value));
    }


    string SingleFloat_O::__repr__() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }






    EXPOSE_CLASS(core,SingleFloat_O);


    void SingleFloat_O::exposeCando(Lisp_sp lisp)
    {
	class_<SingleFloat_O>()
	    ;
	;
    }

    void SingleFloat_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    ;
#endif
    }










//--------------------------------------------------




    float DoubleFloat_O::as_float() const
    {
	return (float)this->_Value;
    }

    double DoubleFloat_O::as_double() const
    {
	return (double)this->_Value;
    }

    LongFloat DoubleFloat_O::as_long_float() const
    {
	return (LongFloat)this->_Value;
    }


    Integer_sp DoubleFloat_O::castToInteger() const
    {
	if (this->_Value < 0) {
	    double f = -this->_Value;
	    long long int cf = *(long long int*)&f;
	    return Integer_O::create(cf)->negate().as<Integer_O>();
	}
	long long int cf = *(long long int*)&this->_Value;
	return Integer_O::create(cf);
    }




    Number_sp DoubleFloat_O::copy() const
    {_OF();
	return DoubleFloat_O::create(this->_Value);
    }


#if defined(OLD_SERIALIZE)
    void DoubleFloat_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
	    // node.setObject(this->sharedThis<DoubleFloat_O>());
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif


#if defined(XML_ARCHIVE)
    void	DoubleFloat_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("val",this->_Value);
    }
#endif // defined(XML_ARCHIVE)



    string DoubleFloat_O::valueAsString() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }


    void DoubleFloat_O::setFromString(const string& str)
    {_G();
	this->_Value = atof(str.c_str());
    }


    Number_sp DoubleFloat_O::signum() const
    {
	return DoubleFloat_O::create(this->_Value>0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0 ));
    }








    void DoubleFloat_O::sxhash(HashGenerator& hg) const
    {_OF();
	hg.addPart(std::abs(::floor(this->_Value)));
    }


    bool	DoubleFloat_O::eql(T_sp obj) const
    {
	if ( this->eq(obj) ) return true;
	if ( obj.isA<Number_O>() )
	{
	    Number_sp num = obj.as<Number_O>();
	    return this->get() == num->as_double();
	}
	return false;
    }

    bool	DoubleFloat_O::eqn(T_sp obj) const
    {_OF();
	if ( af_doubleFloatP(obj) )
	{
	    DoubleFloat_sp t = safe_downcast<DoubleFloat_O>(obj);
	    return this->get() == t->get();
	} else if ( af_fixnumP(obj) )
	{
	    Fixnum_sp t = safe_downcast<Fixnum_O>(obj);
	    return this->get() == t->get();
	}
	ASSERT(!af_numberP(obj));
	return false;
    }




    string DoubleFloat_O::__repr__() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }



    EXPOSE_CLASS(core,DoubleFloat_O);


    void DoubleFloat_O::exposeCando(Lisp_sp lisp)
    {
	class_<DoubleFloat_O>()
//	    .def("exp",&DoubleFloat_O::exp)
	    .def("core:isnan",&DoubleFloat_O::isnan)
	    ;
	SYMBOL_SC_(CorePkg,nan);
	Defun(nan);
	;
    }

    void DoubleFloat_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    .def("valueAsString", &DoubleFloat_O::valueAsString )
	    .def("setFromString", &DoubleFloat_O::setFromString )
//	    .def("set", &DoubleFloat_O::set)
//	    .def("get", &DoubleFloat_O::get)
	    ;
#endif
    }








// ---------------------------------------------
//
// LongFloat stuff


#ifdef CLASP_LONG_FLOAT
    float LongFloat_O::as_float() const
    {
	return (float)this->_Value;
    }

    double LongFloat_O::as_double() const
    {
	return (double)this->_Value;
    }

    LongFloat LongFloat_O::as_long_float() const
    {
	return (LongFloat)this->_Value;
    }


    Integer_sp LongFloat_O::castToInteger() const
    {
	IMPLEMENT_MEF(BF("How do I cast the value to a bignum?"));
#if 0
	if (this->_Value < 0) {
	    double f = -this->_Value;
	    long long int cf = *(long long int*)&f;
	    return Integer_O::create(cf)->negate().as<Integer_O>();
	}
	long long int cf = *(long long int*)&this->_Value;
	return Integer_O::create(cf);
#endif
    }



    Number_sp LongFloat_O::copy() const
    {_OF();
	return LongFloat_O::create(this->_Value);
    }



#if defined(XML_ARCHIVE)
    void LongFloat_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("val",this->_Value);
    }
#endif // defined(XML_ARCHIVE)

#if defined(OLD_SERIALIZE)
    void LongFloat_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
	    // node.setObject(this->sharedThis<DoubleFloat_O>());
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif


    void LongFloat_O::setFromString(const string& str)
    {_G();
	this->_Value = atof(str.c_str());
    }


    Number_sp LongFloat_O::reciprocal() const
    {_G();
	return LongFloat_O::create(1.0/this->_Value);
    }

    string LongFloat_O::valueAsString() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }

    Number_sp LongFloat_O::abs() const
    {_G();
	return LongFloat_O::create(fabs(this->_Value));
    }




    void LongFloat_O::sxhash(HashGenerator& hg) const
    {_OF();
	hg.addPart(std::abs(::floor(this->_Value)));
    }


    bool	LongFloat_O::eql(T_sp obj) const
    {
	if ( this->eq(obj) ) return true;
	if ( obj.isA<Number_O>() )
	{
	    Number_sp num = obj.as<Number_O>();
	    return this->get() == num->as_double();
	}
	return false;
    }

    bool	LongFloat_O::eqn(T_sp obj) const
    {_OF();
	if ( af_longFloatP(obj) )
	{
	    LongFloat_sp t = safe_downcast<LongFloat_O>(obj);
	    return this->get() == t->get();
	} else if ( af_fixnumP(obj) )
	{
	    Fixnum_sp t = safe_downcast<Fixnum_O>(obj);
	    return this->get() == t->get();
	}
	ASSERT(!af_numberP(obj));
	return false;
    }



    Number_sp LongFloat_O::signum() const
    {
	return LongFloat_O::create(this->_Value>0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0 ));
    }

    string LongFloat_O::__repr__() const
    {_G();
	stringstream ss;
	ss << this->_Value;
	return ss.str();
    }









    EXPOSE_CLASS(core,LongFloat_O);


    void LongFloat_O::exposeCando(Lisp_sp lisp)
    {
	class_<LongFloat_O>()
	    ;
	;
    }

    void LongFloat_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    ;
#endif
    }

#endif



// --------------------------------------------------------------------------------


    float Ratio_O::as_float() const
    {_G();
	double d = this->as_double();
	return d;
    }

    double Ratio_O::as_double() const
    {_G();
	double d = this->_numerator->as_double();
	d /= this->_denominator->as_double();
	return d;
    }

    LongFloat Ratio_O::as_long_float() const
    {_G();
	double d = this->as_double();
	return d;
    }

    string Ratio_O::__repr__() const
    {_G();
	stringstream ss;
	ss << _rep_(this->_numerator) << "/" << _rep_(this->_denominator);
	return ss.str();
    }

    string Ratio_O::valueAsString() const
    {_G();
	return this->__repr__();
    }

    mpz_class Ratio_O::numerator_as_mpz() const
    {_G();
	return this->_numerator->as_mpz();
    }

    mpz_class Ratio_O::denominator_as_mpz() const
    {_G();
	return this->_denominator->as_mpz();
    }

    Number_sp Ratio_O::abs() const
    {_G();
	return Ratio_O::create(this->_numerator->abs().as<Integer_O>(), this->_denominator);
    }

    bool Ratio_O::eql(T_sp obj) const
    {_G();
	if ( this->eq(obj) ) return true;
	if ( !af_numberP(obj) ) return false;
	if ( basic_compare(this->const_sharedThis<Ratio_O>(),obj.as<Number_O>()) == 0 ) return true;
	return false;
    }

    bool Ratio_O::eqn(T_sp other) const
    {
	return this->eql(other);
    }

    Number_sp Ratio_O::copy() const
    {_G();
	return Ratio_O::create(this->_numerator,this->_denominator);
    }



    void Ratio_O::sxhash(HashGenerator& hg) const
    {_G();
	if ( hg.isFilling() ) hg.hashObject(this->_numerator);
	if ( hg.isFilling() ) hg.hashObject(this->_denominator);
    }


    Number_sp Ratio_O::signum() const
    {_G();
	ASSERT(this->_denominator->plusp());
	return this->_numerator->signum();
    }





    EXPOSE_CLASS(core,Ratio_O);


#if defined(OLD_SERIALIZE)
    void Ratio_O::serialize(serialize::SNode node)
    {
	IMPLEMENT_ME();
    }
#endif



#if defined(XML_ARCHIVE)
    void Ratio_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->archiveObject("numer",this->_numerator);
	node->archiveObject("denom",this->_denominator);
    }
#endif // defined(XML_ARCHIVE)


    void Ratio_O::setFromString(const string& str)
    {_G();
	vector<string> parts = split(str,"/");
	ASSERT(parts.size() == 2);
	this->_numerator = Integer_O::create(parts[0]);
	this->_denominator = Integer_O::create(parts[1]);
    }





    void Ratio_O::exposeCando(Lisp_sp lisp)
    {
	class_<Ratio_O>()
	    ;
	;
    }

    void Ratio_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    ;
#endif
    }





// --------------------------------------------------------------------------------


    string Complex_O::valueAsString() const
    {
	return this->__repr__();
    }


    Number_sp Complex_O::signum() const
    {_G();
	return Complex_O::create(this->_real->signum().as<Real_O>(),
				 this->_imaginary->signum().as<Real_O>());
    }



#if defined(OLD_SERIALIZE)
    void Complex_O::serialize(serialize::SNode node)
    {
	IMPLEMENT_ME();
    }
#endif

#if defined(XML_ARCHIVE)
    void Complex_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->archiveObject("re",this->_real);
	node->archiveObject("im",this->_imaginary);
    }
#endif // defined(XML_ARCHIVE)

    void Complex_O::setFromString(const string& str)
    {_G();
	IMPLEMENT_ME();
    }


    string Complex_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#C(" << _rep_(this->_real) << " " << _rep_(this->_imaginary) << ")";
	return ss.str();
    }



    void Complex_O::sxhash(HashGenerator& hg) const
    {_G();
	if ( hg.isFilling() ) hg.hashObject(this->_real);
	if ( hg.isFilling() ) hg.hashObject(this->_imaginary);
    }



    Number_sp Complex_O::copy() const
    {
	return Complex_O::create(this->_real,this->_imaginary);
    }

    bool Complex_O::eqn(T_sp o) const
    {
	if ( this->eq(o) ) return true;
	if ( !af_complexP(o) ) return false;
	Complex_sp co = o.as<Complex_O>();
	return ( this->_real == co->_real && this->_imaginary == co->_imaginary );
    }

    bool Complex_O::eql(T_sp o) const
    {
	if ( this->eq(o) ) return true;
	if ( !af_complexP(o) ) return false;
	Complex_sp co = o.as<Complex_O>();
	return ( this->_real == co->_real && this->_imaginary == co->_imaginary );
    }

    Number_sp Complex_O::abs() const
    {_G();
	IMPLEMENT_ME();
    }







    EXPOSE_CLASS(core,Complex_O);



    void Complex_O::exposeCando(Lisp_sp lisp)
    {
	class_<Complex_O>()
	    ;
	;
    }

    void Complex_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Real,"","",_lisp)
	    ;
#endif
    }







/* ----------------------------------------------------------------------

   sqrt

*/

/*
    sqrt.d  -- Square root.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


Number_sp Rational_O::sqrt() const
{
    if ( this->minusp() ) {
	Real_sp x = this->negate()->sqrt().as<Real_O>();
	return Complex_O::create(SingleFloat_O::create(0.0),x);
    } else {
	return SingleFloat_O::create(sqrtf(this->as_float()));
    }
}

Number_sp SingleFloat_O::sqrt() const
{
    if ( this->minusp() ) {
	Number_sp x = this->negate()->sqrt();
	return Complex_O::create(SingleFloat_O::create(0.0),x.as<Real_O>());
    } else {
	return SingleFloat_O::create(sqrtf(this->_Value));
    }
}

Number_sp DoubleFloat_O::sqrt() const
{
    if ( this->minusp() ) {
	Number_sp x = this->negate()->sqrt();
	return Complex_O::create(DoubleFloat_O::create(0.0),x.as<Real_O>());
    } else {
	return DoubleFloat_O::create(::sqrt(this->_Value));
    }
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sqrt() const
{
    if ( this->minusp() ) {
	Number_sp x = this->negate()->sqrt();
	return Complex_O::create(LongFloat_O::create(0.0),x.as<Real_O>());
    } else {
	return LongFloat_O::create(sqrtl(this->_Value));
    }
}
#endif





Number_sp Complex_O::sqrt() const
{
    return cl_expt(this->asSmartPtr(), _lisp->plusHalf());
}





#define ARGS_cl_sqrt "(arg)"
#define DECL_cl_sqrt ""
#define DOCS_cl_sqrt "sqrt"
Number_sp cl_sqrt(Number_sp x)
{_G();
    return x->sqrt();
};







/* ----------------------------------------------------------------------

   sin

*/

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sin.d  -- Trascendental functions: sine
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

Number_sp Rational_O::sin() const
{
    return SingleFloat_O::create(sinf(this->as_float()));
}

Number_sp SingleFloat_O::sin() const
{
    return SingleFloat_O::create(sinf(this->_Value));
}

Number_sp DoubleFloat_O::sin() const
{
    return DoubleFloat_O::create(::sin(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sin() const
{
    return LongFloat_O::create(sinl(this->_Value));
}
#endif

Number_sp Complex_O::sin() const
{
        /*
          z = x + I y
          z = x + I y
          sin(z) = sinh(I z) = sinh(-y + I x)
        */
    Number_sp dx = this->_real;
    Number_sp dy = this->_imaginary;
    Number_sp a = brcl_times(dx->sin(), dy->cosh()); //brcl_sin(dx), brcl_cosh(dy));
    Number_sp b = brcl_times(dx->cos(), dy->sinh()); // brcl_cos(dx), brcl_sinh(dy));
    return Complex_O::create(a.as<Real_O>(),b.as<Real_O>());
}






#define ARGS_cl_sin "(x)"
#define DECL_cl_sin ""
#define DOCS_cl_sin "sin"
Number_sp cl_sin(Number_sp x)
{_G();
    return x->sin();
}




/* ----------------------------------------------------------------------

   cos

*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cos.d  -- Trascendental functions: cosine
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


Number_sp Rational_O::cos() const
{
    return SingleFloat_O::create(cosf(this->as_float()));
}

    Number_sp SingleFloat_O::cos() const
{
    return SingleFloat_O::create(cosf(this->_Value));
}

Number_sp DoubleFloat_O::cos() const
{
    return DoubleFloat_O::create(::cos(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::cos() const
{
        return LongFloat_O::create(cosl(this->_Value));
}
#endif

Number_sp Complex_O::cos() const
{
	/* z = x + I y
           cos(z) = cosh(I z) = cosh(-y + I x)
        */
    Number_sp dx = this->_real;
    Number_sp dy = this->_imaginary;
    Number_sp a = brcl_times(dx->cos(), dy->cosh()); // brcl_cos(dx), brcl_cosh(dy));
    Number_sp b = brcl_times(dx->sin()->negate(), dy->sinh()); // brcl_negate(brcl_sin(dx)), brcl_sinh(dy));
    return Complex_O::create(a.as<Real_O>(),b.as<Real_O>()); // brcl_make_complex(a, b);
}






#define ARGS_cl_cos "(x)"
#define DECL_cl_cos ""
#define DOCS_cl_cos "cos"
Number_sp cl_cos(Number_sp x)
{_G();
    return x->cos();
}



/* ----------------------------------------------------------------------

   tan

*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    tan.d  -- Trascendental functions: tangent
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
Number_sp DoubleFloat_O::sin() const

*/

/*
 * As of 2006-10-13 I found this bug in GLIBC's tanf, which overflows
 * when the argument is pi/4. It is 2008 and this has not yet been
 * solved. Not only that, but if we use tan() on float, GCC automatically
 * and stupidly forces the use of tanf().
 */
#if defined(__amd64__) && defined(__GLIBC__)
static double safe_tanf(double x) { return tan(x); }
#else
# define safe_tanf(x) tanf(x)
#endif


Number_sp Rational_O::tan() const
{
    return SingleFloat_O::create(safe_tanf(this->as_float()));
}

Number_sp SingleFloat_O::tan() const
{
    return SingleFloat_O::create(safe_tanf(this->_Value));
}

Number_sp DoubleFloat_O::tan() const
{
    return DoubleFloat_O::create(::tan(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::tan() const
{
    return LongFloat_O::create(tanl(this->_Value));
}
#endif

Number_sp Complex_O::tan() const
{
    Number_sp a = this->sin();
    Number_sp b = this->cos();
    return brcl_divide(a, b);
}





#define ARGS_cl_tan "(x)"
#define DECL_cl_tan ""
#define DOCS_cl_tan "tan"
Number_sp cl_tan(Number_sp x)
{_G();
    return x->tan();
}



/* ----------------------------------------------------------------------

   sinh
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sinh.d  -- Trascendental functions: hyperbolic sine
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

Number_sp Rational_O::sinh() const
{
        return SingleFloat_O::create(sinhf(this->as_float()));
}

Number_sp SingleFloat_O::sinh() const
{
        return SingleFloat_O::create(sinhf(this->_Value));
}

Number_sp DoubleFloat_O::sinh() const
{
    return DoubleFloat_O::create(::sinh(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sinh() const
{
        return LongFloat_O::create(sinhl(this->_Value));
}
#endif

Number_sp Complex_O::sinh() const
{
        /*
          z = x + I y
          sinh(z) = (exp(z)-exp(-z))/2
          = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
          = sinh(x)*cos(y) + Icosh(x)*sin(y);
        */
    Number_sp dx = this->_real;
    Number_sp dy = this->_imaginary;
    Number_sp a = brcl_times(dx->sinh(), dy->cos()); // brcl_sinh(dx), brcl_cos(dy));
    Number_sp b = brcl_times(dx->cosh(), dy->sin()); // brcl_cosh(dx), brcl_sin(dy));
    return Complex_O::create(a.as<Real_O>(), b.as<Real_O>()); // brcl_make_complex(a, b);
}






#define ARGS_cl_sinh "(x)"
#define DECL_cl_sinh ""
#define DOCS_cl_sinh "sinh"
Number_sp cl_sinh(Number_sp x)
{_G();
    return x->sinh();
}


/* ----------------------------------------------------------------------

   cosh
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cosh.d  -- Trascendental functions: hyperbolic cosine
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


Number_sp Rational_O::cosh() const
{
        return SingleFloat_O::create(coshf(this->as_float()));
}

Number_sp SingleFloat_O::cosh() const
{
        return SingleFloat_O::create(coshf(this->_Value));
}

Number_sp DoubleFloat_O::cosh() const
{
    return DoubleFloat_O::create(::cosh(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::cosh() const
{
        return LongFloat_O::create(coshl(this->_Value));
}
#endif

Number_sp Complex_O::cosh() const
{
        /*
          z = x + I y
          cosh(z) = (exp(z)+exp(-z))/2
          = (exp(x)*(cos(y)+Isin(y))+exp(-x)*(cos(y)-Isin(y)))/2
          = cosh(x)*cos(y) + Isinh(x)*sin(y);
        */
    Number_sp dx = this->_real;
    Number_sp dy = this->_imaginary;
    Number_sp a = brcl_times(dx->cosh(), dy->cos()); // brcl_cosh(dx), brcl_cos(dy));
    Number_sp b = brcl_times(dx->sinh(), dy->sin()); // brcl_sinh(dx), brcl_sin(dy));
    return Complex_O::create(a.as<Real_O>(),b.as<Real_O>()); // brcl_make_complex(a, b);
}






#define ARGS_cl_cosh "(x)"
#define DECL_cl_cosh ""
#define DOCS_cl_cosh "cosh"
Number_sp cl_cosh(Number_sp x)
{_G();
    return x->cosh();
}




/* ----------------------------------------------------------------------

   tanh
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    tanh.d  -- Trascendental functions: hyperbolic tangent
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

Number_sp Rational_O::tanh() const
{
    return SingleFloat_O::create(tanhf(this->as_float()));
}

Number_sp SingleFloat_O::tanh() const
{
    return SingleFloat_O::create(tanhf(this->_Value));
}

Number_sp DoubleFloat_O::tanh() const
{
    return DoubleFloat_O::create(::tanh(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::tanh() const
{
        return LongFloat_O::create(tanhl(this->_Value));
}
#endif

Number_sp Complex_O::tanh() const
{
    Number_sp a = this->sinh();
    Number_sp b = this->cosh();
    return brcl_divide(a, b);
}





#define ARGS_cl_tanh "(x)"
#define DECL_cl_tanh ""
#define DOCS_cl_tanh "tanh"
    Number_sp cl_tanh(Number_sp x)
    {_G();
        return x->tanh();
    }






/* ----------------------------------------------------------------------

   complex-conjugate
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    conjugate.d  -- Trascendental functions: conjugateine
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

Number_sp Real_O::conjugate() const
{
    return this->asSmartPtr();
}

Number_sp Complex_O::conjugate() const
{
    return Complex_O::create(this->_real,this->_imaginary->negate().as<Real_O>());
}






#define ARGS_cl_conjugate "(x)"
#define DECL_cl_conjugate ""
#define DOCS_cl_conjugate "conjugate"
Number_sp cl_conjugate(Number_sp x)
{_G();
    return x->conjugate();
}




/* ----------------------------------------------------------------------

   exp

*/


/* -*- mode: c; c-basic-offset: 8 -*- */
/*
  exp.d  -- Trascendental functions: exponential
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


Number_sp Rational_O::exp() const
{
    return brcl_make_single_float(expf(this->as_float()));
}

    Number_sp SingleFloat_O::exp() const
    {
        return SingleFloat_O::create(expf(this->_Value));
    }

    Number_sp DoubleFloat_O::exp() const
    {
        return DoubleFloat_O::create(::exp(this->_Value));
    }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::exp() const
{
    return LongFloat_O::create(expl(this->_Value));
}
#endif

    Number_sp Complex_O::exp() const
    {
        Real_sp y, y1;
        y = this->_imaginary;
        Real_sp x = this->_real->exp().as<Real_O>();
        y1 = y->cos().as<Real_O>(); // brcl_cos(y);
        y = y->sin().as<Real_O>(); // brcl_sin(y);
        Complex_sp cy = Complex_O::create(y1,y);
        return brcl_times(x, cy);
    }





#define ARGS_cl_exp "(x)"
#define DECL_cl_exp ""
#define DOCS_cl_exp "exp"
Number_sp cl_exp(Number_sp x)
{_G();
    return x->exp();
}



/* ----------------------------------------------------------------------

   expt

*/

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    expt.d  -- Exponentiate.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


Fixnum
brcl_fixnum_expt(Fixnum x, Fixnum y)
{
    Fixnum z = 1;
    while (y > 0)
	if (y%2 == 0) {
	    x *= x;
	    y /= 2;
	} else {
	    z *= x;
	    --y;
	}
    return(z);
}








static Number_sp
expt_zero(Number_sp x, Number_sp y)
{
    NumberType ty, tx;
    Number_sp z;
    ty = brcl_t_of(y);
    tx = brcl_t_of(x);
    if (brcl_unlikely(!x.isA<Number_O>())) {
	QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Number_O);
    }
    /* INV: The most specific numeric types come first. */
    switch ((ty > tx)? ty : tx) {
    case number_Fixnum:
    case number_Bignum:
    case number_Ratio:
	return brcl_make_fixnum(1);
    case number_SingleFloat:
	return _lisp->singleFloatOne();
    case number_DoubleFloat:
	return _lisp->doubleFloatOne();
#ifdef CLASP_LONG_FLOAT
    case number_LongFloat:
	return _lisp->longFloatOne();
#endif
    case number_Complex:
	z = expt_zero((tx == number_Complex)? x.as<Complex_O>()->real().as<Number_O>() : x,
		      (ty == number_Complex)? y.as<Complex_O>()->real().as<Number_O>() : y);
	return Complex_O::create(z.as<Real_O>(), brcl_make_fixnum(0));
    default:
	/* We will never reach this */
	(void)0;
    }
    UNREACHABLE();
}

Number_sp
brcl_expt(Number_sp x, Number_sp y)
{
    NumberType ty, tx;
    Number_sp z;
    if (brcl_unlikely(brcl_zerop(y))) {
	return expt_zero(x, y);
    }
    ty = brcl_t_of(y);
    tx = brcl_t_of(x);
    if (brcl_unlikely(!x.isA<Number_O>())) {
	QERROR_WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Number_O);
    }
    if (brcl_zerop(x)) {
	z = brcl_times(x, y);
	if (!brcl_plusp((ty==number_Complex)?y.as<Complex_O>()->real():y.as<Real_O>()))
	    z = brcl_divide(brcl_make_fixnum(1), z);
    } else if (ty != number_Fixnum && ty != number_Bignum) {
	/* The following could be just
	   z = brcl_log1(x);
	   however, Maxima expects EXPT to have double accuracy
	   when the first argument is integer and the second
	   is double-float */
	z = brcl_log1(brcl_times(x, expt_zero(x, y)));
	z = brcl_times(z, y);
	z = cl_exp(z);
    } else if (brcl_minusp(y.as<Real_O>())) {
	z = brcl_negate(y);
	z = brcl_expt(x, z);
	z = brcl_divide(brcl_make_fixnum(1), z);
    } else {
	BRCL_MATHERR_CLEAR;
	z = brcl_make_fixnum(1);
	Integer_sp iy = y.as<Integer_O>();
	do {
	    /* INV: brcl_integer_divide outputs an integer */
	    if (!brcl_evenp(iy))
		z = brcl_times(z, x);
	    iy = brcl_integer_divide(iy, brcl_make_fixnum(2));
	    if (brcl_zerop(iy)) break;
	    x = brcl_times(x, x);
	} while (1);
	BRCL_MATHERR_TEST;
    }
    return z;
}

#define ARGS_cl_expt "(x y)"
#define DECL_cl_expt ""
#define DOCS_cl_expt "expt"
Number_sp cl_expt(Number_sp x, Number_sp y)
{_G();
    return brcl_expt(x, y);
}



/* ----------------------------------------------------------------------

   atan
*/
/*    atan1.d  -- Trascendental functions: arc tangent
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

static double
brcl_atan2_double(double y, double x)
{
  if (std::signbit(x)) {
    if (std::signbit(y)) {
	    return -BRCL_PI_D + atan(-y / -x);
	} else if (y == 0) {
	    return BRCL_PI_D;
	} else {
	    return BRCL_PI_D - atan(y / -x);
	}
    } else if (x == 0) {
    if (std::signbit(y)) {
	    return -BRCL_PI2_D;
	} else if (y == 0) {
	    return x / y;  /* Produces a NaN */
	} else {
	    return BRCL_PI2_D;
	}
    } else {
    if (std::signbit(y)) {
	    return -atan(-y / x);
	} else if (y == 0) {
	    return (double)0;
	} else {
	    return atan(y / x);
	}
    }
}

#ifdef CLASP_LONG_FLOAT
static LongFloat
brcl_atan2_LongFloat(LongFloat y, LongFloat x)
{
    if (signbit(x)) {
	if (signbit(y)) {
	    return -BRCL_PI_L + atanl(-y / -x);
	} else if (y == 0) {
	    return BRCL_PI_L;
	} else {
	    return BRCL_PI_L - atanl(y / -x);
	}
    } else if (x == 0) {
	if (signbit(y)) {
	    return -BRCL_PI2_L;
	} else if (y == 0) {
	    return x / y;  /* Produces a NaN */
	} else {
	    return BRCL_PI2_L;
	}
    } else {
	if (signbit(y)) {
	    return -atanl(-y / x);
	} else if (y == 0) {
	    return (LongFloat)0;
	} else {
	    return atanl(y / x);
	}
    }
}
#endif



Number_sp brcl_atan2(Number_sp y, Number_sp x)
{
    Number_sp output;
    BRCL_MATHERR_CLEAR;
    {
#ifdef CLASP_LONG_FLOAT
	NumberType tx = x->number_type();
	NumberType ty = y->number_type();
	if (tx < ty)
	    tx = ty;
	if (tx == number_LongFloat) {
	    LongFloat d = brcl_atan2_LongFloat(y->as_long_float(),
					       x->as_long_float());
	    output = brcl_make_long_float(d);
	} else {
	    double dx = x->as_double();
	    double dy = y->as_double();
	    double dz = brcl_atan2_double(dy, dx);
	    if (tx == number_DoubleFloat) {
		output = brcl_make_double_float(dz);
	    } else {
		output = brcl_make_single_float(dz);
	    }
	}
#else
	double dy = y->as_double();
	double dx = x->as_double();
	double dz = brcl_atan2_double(dy, dx);
	if (x->number_type() == number_DoubleFloat
	    || y->number_type() == number_DoubleFloat ) {
	    output = brcl_make_double_float(dz);
	} else {
	    output = brcl_make_single_float(dz);
	}
#endif
    }
    BRCL_MATHERR_TEST;
    return output;
}

Number_sp brcl_atan1(Number_sp y)
{
    if (y->number_type() == number_Complex) {
#if 0 /* ANSI states it should be this first part */
	Number_sp z = brcl_times(cl_core.imag_unit, y);
	z = brcl_plus(brcl_log1(brcl_one_plus(z)),
	    brcl_log1(brcl_minus(brcl_make_fixnum(1), z)));
	z = brcl_divide(z, brcl_times(brcl_make_fixnum(2),
	    cl_core.imag_unit));
#else
	Number_sp z1, z = brcl_times(_lisp->imaginaryUnit(), y);
	z = brcl_one_plus(z);
	z1 = brcl_times(y, y);
	z1 = brcl_one_plus(z1);
	z1 = brcl_sqrt(z1);
	z = brcl_divide(z, z1);
	z = brcl_log1(z);
	z = brcl_times(_lisp->imaginaryUnitNegative(),z);
#endif /* ANSI */
	return z;
    } else {
	return brcl_atan2(y, brcl_make_fixnum(1));
    }
    }





#define ARGS_af_atan "(x &optional y)"
#define DECL_af_atan ""
#define DOCS_af_atan "atan"
	T_sp af_atan(Number_sp x, Number_sp y)
	{_G();
	/* INV: type check in brcl_atan() & brcl_atan2() */
	/* FIXME brcl_atan() and brcl_atan2() produce generic errors
	   without recovery and function information. */
	if (y.nilp()) {
	return brcl_atan1(x);
    }
	return brcl_atan2(x,y);
    }

/* ----------------------------------------------------------------------

   log1

   Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
   Copyright (c) 1990, Giuseppe Attardi.
   Copyright (c) 2001, Juan Jose Garcia Ripoll.
   Copyright (c) 2013, Christian E. Schafmeister

   ECL is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   See file '../Copyright' for full details.

   This code is modified from ecl/src/numbers/log.d
*/

    Number_sp brcl_log1_complex_inner(Number_sp r, Number_sp i)
    {
	Real_sp a = r->abs().as<Real_O>();
	Real_sp p = i->abs().as<Real_O>();
	int rel = brcl_number_compare(a, p);
	if (rel > 0) {
	    Real_sp aux = p;
	    p = a; a = aux;
	} else if (rel == 0) {
	    /* if a == p,
	     * log(sqrt(a^2+p^2)) = log(2a^2)/2
	     */
	    a = brcl_times(a, a).as<Real_O>();
	    a = brcl_divide(brcl_log1(brcl_plus(a, a)), Fixnum_O::create(2)).as<Real_O>();
	    goto OUTPUT;
	}
	/* For the real part of the output we use the formula
	 *	log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
	 *			     = log(p) + log(1 + (a/p)^2)/2; */
	a = brcl_divide(a, p).as<Real_O>();
	a = brcl_plus(brcl_divide(brcl_log1p(brcl_times(a,a)), Fixnum_O::create(2)),
		      brcl_log1(p)).as<Real_O>();
    OUTPUT:
	p = brcl_atan2(i, r).as<Real_O>();
	return Complex_O::create(a, p);
    }




    Number_sp Bignum_O::log1() const
    {
	if ( this->minusp()) {
	    return brcl_log1_complex_inner(this->const_sharedThis<Bignum_O>(),Fixnum_O::create(0));
	} else {
	    Fixnum l = clasp_integer_length(this->const_sharedThis<Bignum_O>()) - 1;
	    Number_sp r = brcl_make_ratio(this->asSmartPtr(), clasp_ash(Fixnum_O::create(1), l));
	    float d = logf(r->as_float()) + l * logf(2.0);
	    return SingleFloat_O::create(d);
        }
    }

    Number_sp Rational_O::log1() const
    {
        float f = this->as_float();
        if (f < 0) return brcl_log1_complex_inner(this->asSmartPtr(),
						  brcl_make_fixnum(0));
        return brcl_make_single_float(logf(this->as_float()));
    }

    Number_sp SingleFloat_O::log1() const
    {
        float f = this->as_float();
        if (::isnan(f)) return this->asSmartPtr();
        if (f < 0) return brcl_log1_complex_inner(this->asSmartPtr(),
						  brcl_make_fixnum(0));
        return brcl_make_single_float(logf(f));
    }

    Number_sp DoubleFloat_O::log1() const
    {
        double f = this->as_double();
        if (::isnan(f)) return this->asSmartPtr();
        if (f < 0) return brcl_log1_complex_inner(this->asSmartPtr(),
						  brcl_make_fixnum(0));
        return brcl_make_double_float(log(f));
    }

#ifdef CLASP_LONG_FLOAT
    Number_sp LongFloat_O::log1() const
    {
        LongFloat f = this->as_long_float();
        if (::isnan(f)) return this->asSmartPtr();
        if (f < 0) return brcl_log1_complex_inner(this->asSmartPtr(),
						  brcl_make_fixnum(0));
        return brcl_make_long_float(logl(f));
    }
#endif


    Number_sp Complex_O::log1() const
    {
        return brcl_log1_complex_inner(this->real(),this->imaginary());
    }



    template <typename FLOAT>
    FLOAT _log1p(FLOAT x)
    {
	IMPLEMENT_MEF(BF("Implement specialized log1p for basic float type"));
    }

    template <>
    float _log1p<float>(float x)
    {
	float u = (float)1 + x;
	if ( u==1 ) {
	    return (float)0;
	}
	return (logf(u)*x)/(u-(float)1);
    }

    template <>
    double _log1p<double>(double x)
    {
	double u = (double)1 + x;
	if ( u==1 ) {
	    return (double)0;
	}
	return (log(u)*x)/(u-(double)1);
    }

#ifdef CLASP_LONG_FLOAT
    template <>
    LongFloat _log1p<LongFloat>(LongFloat x)
    {
	LongFloat u = (LongFloat)1 + x;
	if ( u==1 ) {
	    return (LongFloat)0;
	}
	return (logl(u)*x)/(u-(LongFloat)1);
    }
#endif




    Number_sp Number_O::log1p() const
    {
	return brcl_log1_complex_inner(brcl_one_plus(this->asSmartPtr()), brcl_make_fixnum(0));
    }

    Number_sp Rational_O::log1p() const
    {
	float f = this->as_float();
	if (f < -1) return this->Base::log1p();
	return brcl_make_single_float(_log1p(f));
    }



    Number_sp SingleFloat_O::log1p() const
    {
	float f = this->as_float();
	if (::isnan(f)) return this->asSmartPtr();
	if (f < -1) return this->Base::log1p();
	return brcl_make_single_float(_log1p(f));
    }

    Number_sp DoubleFloat_O::log1p() const
    {
        double f = this->as_double();
        if (::isnan(f)) return this->asSmartPtr();
        if (f < -1) return this->Base::log1p();
        return brcl_make_double_float(_log1p(f));
    }

#ifdef CLASP_LONG_FLOAT
    Number_sp LongFloat_O::log1p() const
    {
	LongFloat f = this->as_long_float();
	if (::isnan(f)) return this->asSmartPtr();
	if (f < -1) return this->Base::log1p();
	return brcl_make_long_float(_log1p(f));
    }
#endif

    Number_sp brcl_log2(Number_sp x, Number_sp y)
    {
	return brcl_divide(brcl_log1(y),brcl_log1(x));
    }


    Number_sp Complex_O::log1p() const
    {
	return brcl_log1_complex_inner(brcl_one_plus(this->real()), this->imaginary());
    }

#define ARGS_af_log "(number &optional base)"
#define DECL_af_log ""
#define DOCS_af_log "Calculate the log of (number) to base (base)."
    Number_sp af_log(Number_sp number, Number_sp base)
    {_G();
	if ( base.nilp() ) {
	    return brcl_log1(number);
	}
	return brcl_log2(base,number);
    }





#define ARGS_af_log1p "(arg)"
#define DECL_af_log1p ""
#define DOCS_af_log1p "log1p"
    Number_sp af_log1p(Number_sp arg)
    {_G();
	return brcl_log1p(arg);
    };


Integer_sp clasp_ash(Integer_sp x, int bits)
{
    return x->shift(bits);
};


    unsigned char clasp_toUint8(T_sp n)
    {
        if (Fixnum_sp fn = n.as<Fixnum_O>()) {
            Fixnum fi = fn->get();
            if ( fi>=0 && fi<=255 ) {
                return fi;
            }
        }
        TYPE_ERROR(n,Cons_O::create(cl::_sym_UnsignedByte,Fixnum_O::create(8)));
    }

signed char clasp_toSignedInt8(T_sp n)
    {
        if (Fixnum_sp fn = n.as<Fixnum_O>()) {
            Fixnum fi = fn->get();
            if ( fi>=-128 && fi<=127 ) {
                return fi;
            }
        }
        TYPE_ERROR(n,Cons_O::create(cl::_sym_SignedByte,Fixnum_O::create(8)));
    }


cl_index clasp_toSize(T_sp f)
{
    if ( Fixnum_sp fn = f.as<Fixnum_O>() ) {
        Fixnum ff = fn->get();
        if ( ff >= 0 ) {
            return ff;
        }
    }
    TYPE_ERROR(f,Cons_O::createList(cl::_sym_Integer_O,Fixnum_O::create(0),Fixnum_O::create(MOST_POSITIVE_FIXNUM)));
}


cl_fixnum
fixint(T_sp x)
{
    if (af_fixnumP(x))
        return x.as<Fixnum_O>()->get();
    if (af_bignumP(x)) {
        IMPLEMENT_MEF(BF("Implement convert Bignum to fixint"));
#if 0
        if (mpz_fits_slong_p(x->big.big_num)) {
            return mpz_get_si(x->big.big_num);
        }
#endif
    }
    ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_Fixnum_O,x,cl::_sym_Fixnum_O);
    UNREACHABLE();
}





#define ARGS_cl_integerLength "(i)"
#define DECL_cl_integerLength ""
#define DOCS_cl_integerLength "integerLength"
#define FILE_cl_integerLength __FILE__
#define LINE_cl_integerLength __LINE__
    int cl_integerLength(Integer_sp i)
    {_G();
        return clasp_integer_length(i);
    };





    void initialize_numbers()
    {
	SYMBOL_EXPORT_SC_(ClPkg,sqrt);
	ClDefun(sqrt);
	SYMBOL_EXPORT_SC_(ClPkg,sin);
	ClDefun(sin);
	SYMBOL_EXPORT_SC_(ClPkg,cos);
	ClDefun(cos);
	SYMBOL_EXPORT_SC_(ClPkg,tan);
	ClDefun(tan);
	SYMBOL_EXPORT_SC_(ClPkg,sinh);
	ClDefun(sinh);
	SYMBOL_EXPORT_SC_(ClPkg,cosh);
	ClDefun(cosh);
	SYMBOL_EXPORT_SC_(ClPkg,tanh);
	ClDefun(tanh);
	SYMBOL_EXPORT_SC_(ClPkg,conjugate);
	ClDefun(conjugate);
	SYMBOL_EXPORT_SC_(ClPkg,log);
	Defun(log);
	SYMBOL_EXPORT_SC_(CorePkg,log1p);
	Defun(log1p);
	SYMBOL_EXPORT_SC_(ClPkg,expt);
	ClDefun(expt);
	SYMBOL_EXPORT_SC_(ClPkg,exp);
	ClDefun(exp);
        ClDefun(integerLength);
    }


};
