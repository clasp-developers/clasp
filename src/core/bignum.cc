/*
    File: bignum.cc
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

#include "boost/format.hpp"
#include "common.h"
#include "numbers.h"
#include "symbol.h"
#include "conditions.h"
#include "hashTable.h"
#include "bignum.h"
#include "wrappers.h"




namespace core
{



    unsigned int* BignumExportBuffer::getOrAllocate(const mpz_class& bignum, int nail ) {
            size_t size = _lisp->integer_ordering()._mpz_import_size;
            size_t numb = (size<<3) - nail; // *8
            size_t count = (mpz_sizeinbase(bignum.get_mpz_t(),2) + numb - 1) / numb;
            size_t bytes = count*size;
            if ( bytes > this->bufferSize ) {
                if ( this->buffer ) {
                    free (this->buffer);
                }
                this->buffer = (unsigned int*)malloc(bytes);
            }
            return this->buffer;
        };
    
    
#define ARGS_Bignum_O_make "(value_in_string)"
#define DECL_Bignum_O_make ""
#define DOCS_Bignum_O_make "make"
    Bignum_sp Bignum_O::make(const string& value_in_string)
    {_G();
        GC_ALLOCATE(Bignum_O,bn );
        bn->_value = value_in_string;
	return((bn));
    };


 
    Bignum Bignum_O::as_mpz() const
    {_G();
	return((this->_value));
    }


    LongLongInt Bignum_O::as_LongLongInt() const
    {_G();
	if ( this->_value.fits_sint_p() )
	{
	    return((this->_value.get_si()));
	}
	SIMPLE_ERROR(BF("Cannot convert Bignum %s to sint") % this->__repr__() );
    }


    void Bignum_O::sxhash(HashGenerator& hg) const
    {
	hg.addPart(this->_value);
    }


    int Bignum_O::as_int() const
    {_G();
	if ( this->_value.fits_sint_p() )
	{
	    return((this->_value.get_si()));
	}
	SIMPLE_ERROR(BF("Cannot convert Bignum %s to sint") % this->__repr__() );
    }

static BignumExportBuffer static_Bignum_O_as_uint64_buffer;  

    uint64_t Bignum_O::as_uint64() const
    {_G();
	unsigned int* valsP = static_Bignum_O_as_uint64_buffer.getOrAllocate(this->_value,0);
	size_t count;
	valsP = (unsigned int*)::mpz_export(valsP, &count,
					    _lisp->integer_ordering()._mpz_import_word_order,
					    _lisp->integer_ordering()._mpz_import_size,
					    _lisp->integer_ordering()._mpz_import_endian, 
					    0,
					    this->_value.get_mpz_t() );
	if ( valsP == NULL )
	{
	    return((0));
	} else if ( count == 1 || count == 2 )
	{
	    unsigned int val0 = valsP[0];
	    unsigned int val1 = 0;
	    if ( count > 1 ) val1 = valsP[1];
	    if ( count == 1 )
	    {
		return((val0&0xfffffffful));
	    } else if ( count == 2 )
	    {
		uint64_t ret = val1;
		ret = ret << 32;
		ret |= val0;
		return((ret));
	    }
	}
	SIMPLE_ERROR(BF("Cannot convert Bignum %s to sint") % this->__repr__() );
    }

    /*! This helps us debug the as_uint64 function by returning a string representation of the uint64 */
    string Bignum_O::as_uint64_string() const
    {_G();
	uint64_t ui64 = this->as_uint64();
	stringstream ss;
	ss << ui64;
	return((ss.str()));
    }



    bool Bignum_O::fits_sint_p()
    {_G();
	return((this->_value.fits_sint_p()));
    }



    float Bignum_O::as_float() const
    {
	return((this->_value.get_d()));
    }


    double Bignum_O::as_double() const
    {
	return((this->_value.get_d()));
    }

    LongFloat Bignum_O::as_long_float() const
    {
	return((this->_value.get_d()));
    }


    void Bignum_O::setFromString( const string& strVal)
    {_G();
	this->_value = strVal;
    }



    int Bignum_O::bit_length() const
    {
	Bignum x = this->_value;
	if ( this->sign() < 0 ) {
	    x = -x;
	}
	return mpz_sizeinbase(x.get_mpz_t(),2);
    }

    /*! Return the value shifted by BITS bits.
      If BITS < 0 shift right, if BITS >0 shift left. */
    Integer_sp Bignum_O::shift(int bits) const
    {
	if ( bits == 0 ) return this->asSmartPtr();
	Bignum res;
	if ( bits < 0 ) {
	    mpz_div_2exp(res.get_mpz_t(),this->_value.get_mpz_t(),-bits);
	} else {
	    mpz_mul_2exp(res.get_mpz_t(),this->_value.get_mpz_t(),bits);
	}
	return Integer_O::create(res);
    }




    string Bignum_O::__repr__() const
    {_G();
	stringstream ss;
	ss << this->_value;
	return((ss.str()));
    }


    Bignum	Bignum_O::get() const
    {_G();
	return((this->_value));
    }


    Number_sp Bignum_O::copy() const
    {_G();
        GC_ALLOCATE(Bignum_O,cp );
        cp->_value = this->_value;
	return((cp));
    };

    Number_sp Bignum_O::abs() const
    {_G();
        GC_ALLOCATE(Bignum_O,cp );
        cp->_value = this->_value*::sgn(this->_value);
	return((cp));
    }





    bool Bignum_O::eql(T_sp o) const
    {_G();
	if ( this->eq(o) ) return((true));
	if ( af_integerP(o) )
	{
	    if ( this->_value == o.as<Integer_O>()->as_mpz() ) return((true));
	}
	return((false));
    }


    bool Bignum_O::eqn(T_sp o) const
    {
	return((this->eql(o)));
    }





    EXPOSE_CLASS(core,Bignum_O);

    void Bignum_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<Bignum_O>()	
	    .def("core:fitsSintP",&Bignum_O::fits_sint_p)
	    .def("core:asUint64String",&Bignum_O::as_uint64_string)
	    ;
	af_def(CorePkg,"make-bignum",&Bignum_O::make,ARGS_Bignum_O_make,DECL_Bignum_O_make,DOCS_Bignum_O_make);
    }






    void Bignum_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CurrentPkg,Bignum,"","",_lisp)
	    ;
#endif
    }




    Integer_mv big_ceiling(Bignum_sp a, Bignum_sp b)
    {
	Bignum mpzq, mpzr;
	mpz_cdiv_qr(mpzq.get_mpz_t(),
		    mpzr.get_mpz_t(),
		    a->ref().get_mpz_t(),
		    b->ref().get_mpz_t());
	return Values(Integer_O::create(mpzq),Integer_O::create(mpzr));
    }

    Integer_mv big_floor(Bignum_sp a, Bignum_sp b)
    {
	Bignum_sp q = _lisp->bigRegister0();
	Bignum_sp r = _lisp->bigRegister1();
	mpz_fdiv_qr(q->ref().get_mpz_t(),r->ref().get_mpz_t(),
		    a->ref().get_mpz_t(),b->ref().get_mpz_t());
	return Values(Integer_O::create(q->get()),Integer_O::create(r->get()));
    }



    Integer_sp _brcl_big_gcd(Bignum_sp x, Bignum_sp y)
    {
	Bignum zz;
	mpz_gcd(zz.get_mpz_t(),x->ref().get_mpz_t(),y->ref().get_mpz_t());
	return Bignum_O::create(zz);
    }


    Integer_sp _brcl_big_divided_by_big(const Bignum& a, const Bignum& b)
    {
	size_t size_a = BRCL_BIGNUM_ABS_SIZE(a.get_mpz_t());
	size_t size_b = BRCL_BIGNUM_ABS_SIZE(b.get_mpz_t());
	Fixnum size_z = size_a - size_b + 1;
	if (size_z <= 0) size_z = 1;
	Bignum z;
	mpz_tdiv_q(z.get_mpz_t(),a.get_mpz_t(),b.get_mpz_t());
	return Integer_O::create(z);
    }


    Integer_sp _brcl_big_divided_by_fix(const Bignum& x, const Fixnum& y)
    {
	Bignum by(y);
	return _brcl_big_divided_by_big(x, by);
    }

    Integer_sp _brcl_fix_divided_by_big(const Fixnum& x, const Bignum& y)
    {
	Bignum bx(x);
	return _brcl_big_divided_by_big(bx, y);
    }

    
};
