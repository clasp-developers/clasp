       
       

#ifndef	NUMERICS_H
#define	NUMERICS_H


#include "foundation.h"
#include "object.h"

namespace core {
//#define	REAL	double	// we may want to use float on cell processors
//#define	FLOAT	float
//#define	DOUBLE	double


#ifdef	WIN32
#include <limits>
typedef		__int64 LongLongInt;
#define		LongLongMax	LLONG_MAX

#define	    myMAXFLOAT  FLT_MAX
#define	atoll(x)	(_atoi64(x))


#elif __PGI
#include "math.h"
#include "limits.h"
typedef		long long int	LongLongInt;
#define		LongLongMax	LONGLONG_MAX
#define	    myMAXFLOAT  HUGE
#else
#include "math.h"
typedef		long long int	LongLongInt;
#define		LongLongMax	LLONG_MAX
#define	    myMAXFLOAT  HUGE
#endif



#define LongLongMaxScale 4096    // was 256
#define LongLongIntBoundary LongLongMax/LongLongMaxScale


namespace numerics
{

    static const double pi = 3.14159265;

    inline double radFromDeg(const double& deg)
    {
	return deg*0.0174533;
    }

    inline double degFromRad(const double& rad)
    {
#define	DEG_FROM_RAD 1.0/0.0174533;
	return rad*DEG_FROM_RAD;
    }

};


/*! Convert a collection of positive mixed-base digits to a Bignum index.
 * If the index can not be stored in a Bignum then return -1
 */
extern Bignum	mixedBaseDigitsToBignum(const vector<int>& bases, const vector<int>& digits);

/*! Convert a collection of positive mixed-base digits to a LongLongInt index.
 * If the index can not be stored in a LongLongInt then return -1
 */
    extern vector<int> bignumToMixedBaseDigits(const Bignum& index, const vector<int>& bases);

    extern Bignum numberOfIndicesForMixedBase(const vector<int>& bases);


void	seedRandomNumberGenerators(uint i);
void	seedRandomNumberGeneratorsUsingTime();

double	randomNumber01();
double	randomNumberNormal01();

bool	almostEqualAbsoluteOrRelative(double va, double vb,
				double absEpsilon,double relEpsilon);

inline double degrees(double rad) { return rad/0.0174533;};
inline double radians(double deg) {return deg*0.0174533;};






void exposeCando_Numerics();

#ifdef USEBOOSTPYTHON
void exposePython_Numerics();
#endif

};


DEFINE_RETURN_VALUE_TYPE(core::LongLongInt);

#endif

