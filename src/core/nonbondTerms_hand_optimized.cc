/*
    File: nonbondTerms_hand_optimized.cc
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
double dx1,dy1,dz1,eeel,evdw,fx1,fx2,fy1,fy2,fz1,fz2,tx1,tx11,tx12,tx13,tx14,tx16,tx17,tx18,tx19,tx2,tx20,tx21,tx22,tx23,tx24,tx25,tx26,tx27,tx28,tx29,tx3,tx31,tx32,tx33,tx34,tx35,tx36,tx4,tx5,tx6,tx7,tx8,tx9,tzz38,tzz39,tzz40,tzz41,tzz42,tzz43,tzz44,xxxxDummy;
	tx1 = -x2; 		/* rule 1 */
	tx2 = -y2; 		/* rule 2 */
	tx3 = -z2; 		/* rule 3 */
	tx4 = tx1 + x1; 		/* rule 4 */
	tx5 = tx2 + y1; 		/* rule 5 */
	tx6 = tx3 + z1; 		/* rule 6 */
	tx7 = power2(tx4); 		/* rule 7 */
	tx8 = power2(tx5); 		/* rule 8 */
	tx9 = power2(tx6); 		/* rule 9 */
	tx33 = tx7 + tx8 + tx9; 		/* rule 10 */
	tx35 = reciprocal(tx33); 		/* rule 11 */
	tx36 = power2(tx35); 		/* rule 12 */
	tx28 = tx35*tx36; 		/* rule 13 */
	tx11 = power2(tx28); 		/* rule 14 */
	tx29 = power2(tx35); 		/* rule 15 */
	tx12 = tx29*tx35; 		/* rule 16 */
	tx13 = dA*tx11; 		/* rule 17 */
	tx14 = -(dC*tx12); 		/* rule 18 */
	evdw = tx13 + tx14; 		/* rule 19 */
	tx34 = mysqrt(tx35); 		/* rule 20 */
//	tx34 = reciprocal(tx31); 		/* rule 21 */
	eeel = dQ1Q2*tx34; 		/* rule 22 */
	/* ENERGY_DONE: */
	if ( !calcForce ) goto NO_GRADIENT;
	tx32 = power2(tx36); 		/* rule 25 */
	tx16 = tx28*tx32; 		/* rule 26 */
	tx17 = power2(tx29); 		/* rule 27 */
	tx18 = tx34*tx35; 		/* rule 28 */
	tzz38 = twelve*tx16; 		/* rule 29 */
	tzz42 = dA*tzz38; 		/* rule 30 */
	tzz43 = -tzz42; 		/* rule 31 */
	tx19 = tx4*tzz43; 		/* rule 32 */
	tzz39 = six*tx17; 		/* rule 33 */
	tzz41 = dC*tzz39; 		/* rule 34 */
	tx20 = tx4*tzz41; 		/* rule 35 */
	tzz40 = dQ1Q2*tx18; 		/* rule 36 */
	tzz44 = -tzz40; 		/* rule 37 */
	tx21 = tx4*tzz44; 		/* rule 38 */
	dx1 = tx19 + tx20 + tx21; 		/* rule 39 */
	tx22 = tx5*tzz43; 		/* rule 40 */
	tx23 = tx5*tzz41; 		/* rule 41 */
	tx24 = tx5*tzz44; 		/* rule 42 */
	dy1 = tx22 + tx23 + tx24; 		/* rule 43 */
	tx25 = tx6*tzz43; 		/* rule 44 */
	tx26 = tx6*tzz41; 		/* rule 45 */
	tx27 = tx6*tzz44; 		/* rule 46 */
	dz1 = tx25 + tx26 + tx27; 		/* rule 47 */
	fx1 = -dx1; 		/* rule 48 */
	fy1 = -dy1; 		/* rule 49 */
	fz1 = -dz1; 		/* rule 50 */
	fx2 = -fx1; 		/* rule 51 */
	fy2 = -fy1; 		/* rule 52 */
	fz2 = -fz1; 		/* rule 53 */
	/* GRADIENT_DONE: */
