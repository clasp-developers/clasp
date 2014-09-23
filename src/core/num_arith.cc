/*
    File: num_arith.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_arith.c  -- Arithmetic operations
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "foundation.h"
#include "object.h"
#include "symbolTable.h"
#include "numbers.h"
#include "bignum.h"
#include "num_arith.h"
#include "wrappers.h"


namespace core {


    SYMBOL_EXPORT_SC_(CorePkg,integer_divide);
    Integer_sp brcl_integer_divide(Integer_sp x, Integer_sp y)
    {
	NumberType tx, ty;
	tx = brcl_t_of(x);
	ty = brcl_t_of(y);
	if (tx == number_Fixnum) {
	    if (ty == number_Fixnum) {
		if (y == brcl_make_fixnum(0))
		    ERROR_DIVISION_BY_ZERO(x,y);
		return brcl_make_fixnum(brcl_fixnum(x) / brcl_fixnum(y));
	    } else if (ty == number_Bignum) {
		return _brcl_fix_divided_by_big(brcl_fixnum(x), y.as<Bignum_O>()->get());
	    } else {
		ERROR_WRONG_TYPE_NTH_ARG(core::_sym_integer_divide,2,y,cl::_sym_Integer_O);
	    }
	}
	if (tx == number_Bignum) {
	    if (ty == number_Bignum) {
		return _brcl_big_divided_by_big(x.as<Bignum_O>()->get(), y.as<Bignum_O>()->get());
	    } else if (ty == number_Fixnum) {
		return _brcl_big_divided_by_fix(x.as<Bignum_O>()->get(), brcl_fixnum(y));
	    } else {
		QERROR_WRONG_TYPE_NTH_ARG(2,y,cl::_sym_Integer_O);
	    }
	}
	ERROR_WRONG_TYPE_NTH_ARG(core::_sym_integer_divide,1,x,cl::_sym_Integer_O);
	UNREACHABLE();
    }



    
    
#define ARGS_cl_gcd "(&rest nums)"
#define DECL_cl_gcd ""
#define DOCS_cl_gcd "gcd"
    Integer_sp cl_gcd(Cons_sp nums)
    {_G();
	if (nums.nilp())
	    return brcl_make_fixnum(0);
	/* INV: brcl_gcd() checks types */
	Integer_sp gcd = oCar(nums).as<Integer_O>();
	nums = cCdr(nums);
	if (nums.nilp()) {
	    return (brcl_minusp(gcd) ? brcl_negate(gcd).as<Integer_O>() : gcd);
	}
	while (nums.notnilp()) {
	    gcd = brcl_gcd(gcd, oCar(nums).as<Integer_O>());
	    nums = cCdr(nums);
	}
	return gcd;
    }

    Integer_sp brcl_gcd(Integer_sp x, Integer_sp y, int yidx)
    {
	switch (brcl_t_of(x)) {
	case number_Fixnum: {
	    Bignum_sp big(Bignum_O::create(brcl_fixnum(x)));
	    x = big;
	}
	case number_Bignum:
	    break;
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(yidx,x,cl::_sym_Integer_O);
	}
	switch (brcl_t_of(y)) {
	case number_Fixnum: {
	    Bignum_sp big(Bignum_O::create(brcl_fixnum(y)));
	    y = big;
	}
	case number_Bignum:
	    break;
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(1+yidx,y,cl::_sym_Integer_O);
        }
        return _brcl_big_gcd(x.as<Bignum_O>(), y.as<Bignum_O>());
    }




    
    
#define ARGS_cl_lcm "(&rest args)"
#define DECL_cl_lcm ""
#define DOCS_cl_lcm "lcm"
    Integer_sp cl_lcm(Cons_sp nums)
    {_G();
	if (nums.nilp())
	    return brcl_make_fixnum(1);
	/* INV: brcl_gcd() checks types. By placing `numi' before `lcm' in
	   this call, we make sure that errors point to `numi' */
	Integer_sp lcm = oCar(nums).as<Integer_O>();
	int yidx=1;
	nums = cCdr(nums);
	while (nums.notnilp()) {
	    Integer_sp numi = oCar(nums).as<Integer_O>();
	    nums = cCdr(nums);
	    yidx++;
	    Number_sp t = brcl_times(lcm, numi);
	    Number_sp g = brcl_gcd(numi, lcm);
	    if (!g->zerop()) {
		lcm = brcl_divide(t, g).as<Integer_O>();
	    }
	}
	return brcl_minusp(lcm) ? brcl_negate(lcm).as<Integer_O>() : lcm.as<Integer_O>();
    };


    void initialize_num_arith()
    {
	SYMBOL_EXPORT_SC_(ClPkg,gcd);
	ClDefun(gcd);
	SYMBOL_EXPORT_SC_(ClPkg,lcm);
	ClDefun(lcm);
    };
};
