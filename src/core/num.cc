/*
    File: num.cc
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
       
#define	DEBUG_LEVEL_NONE

#include "lisp.h"
#include "numerics.h"
#ifdef	darwin
#include <stdint.h>
#include <mach/mach_time.h>
#else
#include <time.h>
#endif
#include "core/bignum.h"
#include <boost/random.hpp>
#include "wrappers.h"


namespace core {


LongLongInt longLongIntAdd(const LongLongInt& x, const LongLongInt& y, const Lisp_sp& lisp)
{_G();
    if ( ( LongLongMax - x ) < y )
    {
	THROW(_lisp->create<MathException_O>("longLongIntAdd>> LongLongInt is about to overflow"));
    }
    return x + y;
}

/*!
 * Multiply the number by the scale and add an offset,
 * if the result is larger than LongLongIntBoundary then
 * return -1.
 */
LongLongInt longLongIntScaleAndAdd(LongLongInt num, int scale, int off, const Lisp_sp& lisp )
{_G();
    LongLongInt newNum;
    if ( num < 0 ) return num;
    if ( scale > LongLongMaxScale )
    {
	stringstream ss;
	ss << "longLongIntScaleAndAdd scale(" << scale << ") must be smaller than LongLongMaxScale(" << LongLongMaxScale << ")";
        THROW(_lisp->create<MathException_O>(ss.str() ));
    }
    newNum = num*scale+off;
    if ( newNum > LongLongIntBoundary )
    {
        return -1;
    }
    return newNum;
}

    LongLongInt	mixedBaseDigitsToLongLongInt(const vector<uint>& bases, const vector<uint>& digits,const Lisp_sp& lisp)
{_G();
LongLongInt			index;
vector<uint>::const_iterator	bi, di;
    ASSERT(bases.size()==digits.size());
    ASSERT(bases.size()>=1);
    ASSERT(digits[0]<bases[0]);
    index = digits[0];
    for ( bi=bases.begin()+1,di=digits.begin()+1;
    		bi!=bases.end(); bi++, di++ )
    {
        index = longLongIntScaleAndAdd(index,*bi,*di,lisp);
	if ( index < 0 ) break;
    }
    return index;
}

    LongLongInt	numberOfIndicesForMixedBase(const vector<uint>& bases,const Lisp_sp& lisp)
{_G();
vector<uint>::const_iterator	bi;
LongLongInt	numSeq;
    ASSERT(bases.size()>=1);
    numSeq = 1;
    for ( bi=bases.begin(); bi!=bases.end(); bi++ )
    {
        numSeq = longLongIntScale(numSeq,*bi,lisp);
	if ( numSeq < 0 ) break;
    }
    return numSeq;
}

/*! Convert a collection of positive mixed-base digits to a LongLongInt index.
 * If the index can not be stored in a LongLongInt then return -1
 */
    vector<uint> longLongIntToMixedBaseDigits(const LongLongInt index, const vector<uint>& bases,const Lisp_sp& lisp)
{_G();
LongLongInt	curIndex;
vector<uint>	digits;
vector<uint>::const_reverse_iterator	bi;
vector<uint>::reverse_iterator		di;
int	digitIdx;
    curIndex = index;
    LOG(BF("*starting index=%20lld") % curIndex  );
    ASSERT(bases.size()>=1);
    digits.resize(bases.size());
    digitIdx = digits.size()-1;
    for ( bi=bases.rbegin(),di=digits.rbegin(); digitIdx>=0; bi++,di++,digitIdx-- )
    {
        *di = curIndex % *bi;
	curIndex /= *bi;
	LOG(BF("*di=%d  *bi=%d curIndex=%20lld") % *di % *bi % curIndex  );
    }
    LOG(BF("digits[0] = %d") % digits[0]  );
    return digits;
}




    
    
#define ARGS_af_getUniversalTime "()"
#define DECL_af_getUniversalTime ""
#define DOCS_af_getUniversalTime "getUniversalTime"
    Integer_mv af_getUniversalTime()
    {_G();
	time_t currentTime;
	time(&currentTime);
	stringstream ss;
	ss << (long long int) time;
	return Integer_O::create(ss.str());
    }




boost::mt11213b	globalRealRandom01Producer;
boost::uniform_real<>	globalRealRandom01Distribution(0,1);
boost::variate_generator<boost::mt11213b&,boost::uniform_real<> >
	globalRandomReal01Generator(globalRealRandom01Producer,
						globalRealRandom01Distribution);
boost::mt11213b	globalRealRandomNormal01Producer;
boost::normal_distribution<double>	globalNormal01Distribution(0,1);
boost::variate_generator<boost::mt11213b&,boost::normal_distribution<double> >
	globalRandomRealNormal01Generator(globalRealRandomNormal01Producer,globalNormal01Distribution);




void	seedRandomNumberGenerators(uint i,Lisp_sp lisp)
{_G();
    globalRealRandom01Producer.seed(static_cast<uint>(i));
    globalRealRandomNormal01Producer.seed(static_cast<uint>(i));
}

void	seedRandomNumberGeneratorsUsingTime(Lisp_sp lisp)
{_G();
clock_t	currentTime;
int	tt;
#ifdef	darwin
    currentTime = mach_absolute_time();
#else
    currentTime = clock();
#endif
    tt = currentTime%32768;
    LOG(BF("seedRandomNumberGeneratorsUsingTime using value(%d)") % tt  );
//    printf("seedRandomNumberGeneratorsUsingTime using value(%d)\n", tt );
    seedRandomNumberGenerators(tt,_lisp);
//    for ( int x=0; x<5; x++ ) {
//	printf( "Random number#%d = %lf\n", x, randomNumber01(_lisp) );
//    }
}





    
    
#define ARGS_af_random "(olimit &optional random-state)"
#define DECL_af_random ""
#define DOCS_af_random "random"
    T_mv af_random(T_sp olimit, T_sp random_state)
    {_G();
	if ( random_state.notnilp() )
	{	
	    SIMPLE_ERROR(BF("Support random-state in random")_);
	}
	
	if ( olimit.isA<Fixnum_O>() )
	{
	    int limit = olimit.as<Fixnum_O>()->get();
	    return Fixnum_O::create((int)(globalRandomReal01Generator()*limit));
	} else if ( olimit.isA<Bignum_O>())
	{
	    IMPLEMENT_MEF(BF("Implement generating Bignum random numbers"));
	} else if ( olimit.isA<DoubleFloat_O>() )
	{
	    double limit = olimit.as<DoubleFloat_O>()->get();
	    return DoubleFloat_O::create(globalRandomReal01Generator()*limit);
	}
	SIMPLE_ERROR(BF("Illegal limit for random"));
    }

double	randomNumber01(Lisp_sp lisp)
{
    return globalRandomReal01Generator();
}


double	randomNumberNormal01(Lisp_sp lisp)
{
    return globalRandomRealNormal01Generator();
}


bool	almostEqualAbsoluteOrRelative(double va, double vb,
					double absEpsilon,
					double relEpsilon )
{
    if ( fabs(va-vb) < absEpsilon ) return true;
    if ( fabs(va)>fabs(vb) ) {
	if ( fabs(va-vb) < vb*relEpsilon ) return true;
    } else {
	if ( fabs(va-vb) < va*relEpsilon ) return true;
    }
    return false;
}



};


namespace core
{

void exposeCando_Numerics()
{_G();
    LOG(BF("Initializing numerics random") );
    af_def(CorePkg,"seedRandomNumberGenerators", &seedRandomNumberGenerators);
    af_def(CorePkg,"seedRandomNumberGeneratorsUsingTime", &seedRandomNumberGeneratorsUsingTime );
    af_def(CorePkg,"random", &random, ARGS_af_random, DECL_af_random, DOCS_af_random );
    af_def(CorePkg,"randomNumber01", &randomNumber01 );
    af_def(CorePkg,"randomNumberNormal01", &randomNumberNormal01 );
    SYMBOL_EXPORT_SC_(ClPkg,getUniversalTime);
    Defun(getUniversalTime);
}



#ifdef	USEBOOSTPYTHON

    void exposePython_Numerics(Lisp_sp lisp)
{
    boost::python::def("mixedBaseDigitsToLongLongInt",&mixedBaseDigitsToLongLongInt);
    boost::python::def("longLongIntToMixedBaseDigits",&longLongIntToMixedBaseDigits);
    boost::python::def("numberOfIndicesForMixedBase",&numberOfIndicesForMixedBase);
    boost::python::def("seedRandomNumberGenerators", &seedRandomNumberGenerators);
    boost::python::def("seedRandomNumberGeneratorsUsingTime", &seedRandomNumberGeneratorsUsingTime);
    boost::python::def("randomNumber01", &randomNumber01);
    boost::python::def("randomNumberNormal01", &randomNumberNormal01);
    boost::python::def("almostEqualAbsoluteOrRelative", &almostEqualAbsoluteOrRelative);
}

#endif
};
