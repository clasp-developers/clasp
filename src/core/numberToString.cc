/*
    File: numberToString.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bignum.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/wrappers.h>


namespace core {



#define ARGS_core_bignumToString "(buffer x base)"
#define DECL_core_bignumToString ""
#define DOCS_core_bignumToString "bignumToString"
    StrWithFillPtr_sp core_bignumToString(StrWithFillPtr_sp buffer, const Bignum& bn, Fixnum_sp base)
    {_G();
	if ( base->get()<2 || base->get()>36) {
	    QERROR_WRONG_TYPE_NTH_ARG(3,base,Cons_O::createList(cl::_sym_integer,Fixnum_O::create(2),Fixnum_O::create(36)));
	}
	int ibase = base->get();
	size_t str_size = mpz_sizeinbase(bn.get_mpz_t(), ibase);
	if ( bn<0 ) str_size++;
	buffer->ensureSpaceAfterFillPointer(str_size+1);
	char* bufferStart = static_cast<char*>(buffer->addressOfFillPtr());
	mpz_get_str(bufferStart,-base->get(),bn.get_mpz_t());
	//	printf("%s:%d str_size = %zu\n    bufferStart[str_size-1] = %d  bufferStart[str_size] = %d bufferStart=[%s]\n", __FILE__, __LINE__, str_size, bufferStart[str_size-1], bufferStart[str_size], bufferStart);
	if ( bufferStart[str_size-1] == '\0' ) {
	    buffer->incrementFillPointer(str_size-1);
	} else {
	    buffer->incrementFillPointer(str_size);
	}
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




    
    
#define ARGS_core_integerToString "(buffer integer base radix decimalp)"
#define DECL_core_integerToString ""
#define DOCS_core_integerToString "integerToString"
    StrWithFillPtr_sp core_integerToString(StrWithFillPtr_sp buffer, Integer_sp integer,
					 Fixnum_sp base, bool radix, bool decimalp)
    {	
	if (radix) {
	    if (!decimalp || base->get() != 10 ) {
		buffer->ensureSpaceAfterFillPointer(10);
		write_base_prefix(buffer,base->get());
	    }
	    buffer = core_integerToString(buffer, integer, base, false, false );
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
		sprintf(txt,"%X",fn);
		buffer->pushString(txt);
		break;
	    default:
		Bignum bn(fn);
		core_bignumToString(buffer,bn,base);
		break;
	    }
	    return buffer;
	} else if ( integer.isA<Bignum_O>() ) {
	    core_bignumToString(buffer,integer.as<Bignum_O>()->get(),base);
	} else {
	    QERROR_WRONG_TYPE_NTH_ARG(2,base,cl::_sym_integer);
	}
	return buffer;
    }



    void initialize_numberToString()
    {
	SYMBOL_EXPORT_SC_(CorePkg,integerToString);
	CoreDefun(integerToString);
    }


};
