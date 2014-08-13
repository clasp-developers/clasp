/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <float.h>
#include "foundation.h"
#include "object.h"
#include "strWithFillPtr.h"
#include "numbers.h"
#include "symbolTable.h"
#include "bignum.h"
#include "numberToString.h"
#include "wrappers.h"


namespace core {



#define ARGS_af_bignumToString "(buffer x base)"
#define DECL_af_bignumToString ""
#define DOCS_af_bignumToString "bignumToString"
    StrWithFillPtr_sp af_bignumToString(StrWithFillPtr_sp buffer, const Bignum& bn, Fixnum_sp base)
    {_G();
	if ( base->get()<2 || base->get()>36) {
	    QERROR_WRONG_TYPE_NTH_ARG(3,base,Cons_O::createList(cl::_sym_integer,Fixnum_O::create(2),Fixnum_O::create(360)));
	}
	size_t str_size = mpz_sizeinbase(bn.get_mpz_t(), base->ref());
	if ( bn<0 ) str_size++;
	buffer->ensureSpaceAfterFillPointer(str_size+1);
	mpz_get_str(static_cast<char*>(buffer->addressOfFillPtr()),base->get(),bn.get_mpz_t());
	buffer->incrementFillPointer(str_size);
	return buffer;
    }




    static void write_base_prefix(StrWithFillPtr_sp buffer, int base)
    {
	if (base == 2) {
	    buffer->pushString("#b");
	} else if (base == 8) {
	    buffer->pushString("#o");
	} else if (base == 16) {
	    buffer->pushString("#x");
	} else if (base >= 10) {
	    string prefix = "#00r";
	    prefix[1] = base/10 + '0';
	    prefix[2] = base%10 + '0';
	    buffer->pushString(prefix.c_str());
	} else {
	    string prefix  = "#0r";
	    prefix[1] = base + '0';
	    buffer->pushString(prefix.c_str());

	}
    }




    
    
#define ARGS_af_integerToString "(buffer integer base radix decimalp)"
#define DECL_af_integerToString ""
#define DOCS_af_integerToString "integerToString"
    StrWithFillPtr_sp af_integerToString(StrWithFillPtr_sp buffer, Integer_sp integer,
					 Fixnum_sp base, bool radix, bool decimalp)
    {	
	if (radix) {
	    if (!decimalp || base->get() != 10 ) {
		buffer->ensureSpaceAfterFillPointer(10);
		write_base_prefix(buffer,base->get());
	    }
	    buffer = af_integerToString(buffer, integer, base, false, false );
	    if (decimalp && base->get() == 10) {
		buffer->pushCharExtend('.');
	    }
	    return buffer;
	}
	if ( integer.isA<Fixnum_O>() ) {
	    char txt[64];
	    int fn = integer.as<Fixnum_O>()->get();
	    switch (base->get()) {
	    case 8:
		sprintf(txt,"%o",fn);
		buffer->pushString(txt);
		break;
	    case 10:
		sprintf(txt,"%d",fn);
		buffer->pushString(txt);
		break;
	    case 16:
		sprintf(txt,"%x",fn);
		buffer->pushString(txt);
		break;
	    default:
		Bignum bn(fn);
		af_bignumToString(buffer,bn,base);
		break;
	    }
	    return buffer;
	} else if ( integer.isA<Bignum_O>() ) {
	    af_bignumToString(buffer,integer.as<Bignum_O>()->get(),base);
	} else {
	    QERROR_WRONG_TYPE_NTH_ARG(2,base,cl::_sym_integer);
	}
	return buffer;
    }



    void initialize_numberToString()
    {
	SYMBOL_EXPORT_SC_(CorePkg,integerToString);
	Defun(integerToString);
    }


};
