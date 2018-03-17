/*
    File: numerics.h
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

#ifndef NUMERICS_H
#define NUMERICS_H

#include <clasp/core/object.h>

namespace core {
//#define	REAL	double	// we may want to use float on cell processors
//#define	FLOAT	float
//#define	DOUBLE	double


namespace numerics {

static const double pi = 3.14159265;

inline double radFromDeg(const double &deg) {
  return deg * 0.0174533;
}

inline double degFromRad(const double &rad) {
#define DEG_FROM_RAD 1.0 / 0.0174533;
  return rad * DEG_FROM_RAD;
}
};

/*! Convert a collection of positive mixed-base digits to a Bignum index.
 * If the index can not be stored in a Bignum then return -1
 */
extern Bignum mixedBaseDigitsToBignum(const vector<int> &bases, const vector<int> &digits);

/*! Convert a collection of positive mixed-base digits to a LongLongInt index.
 * If the index can not be stored in a LongLongInt then return -1
 */
extern vector<int> bignumToMixedBaseDigits(const Bignum &index, const vector<int> &bases);

extern Bignum numberOfIndicesForMixedBase(const vector<int> &bases);

void seedRandomNumberGenerators(uint i);
void seedRandomNumberGeneratorsUsingTime();

double randomNumber01();
double randomNumberNormal01();

bool almostEqualAbsoluteOrRelative(double va, double vb,
                                   double absEpsilon, double relEpsilon);

inline double degrees(double rad) { return rad / 0.0174533; };
inline double radians(double deg) { return deg * 0.0174533; };

void exposeCando_Numerics();

#ifdef USEBOOSTPYTHON
void exposePython_Numerics();
#endif
};

#endif
